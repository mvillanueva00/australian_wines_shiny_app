library(tidyverse)
library(lubridate)
library(shiny)
library(tsibble)
library(fable)
library(feasts)
library(fabletools)
library(urca)
library(knitr)

# ---- LOAD DATA ----
wines <- read.csv("AustralianWines.csv")

wines <- wines |>
  mutate(Date = as.Date(paste0("01-", Month), format = "%d-%b-%y"))

wines_ts <- wines |>
  mutate(across(c(Fortified, Red, Rose, sparkling, Sweet.white, Dry.white),
                ~ as.numeric(.))) |>
  pivot_longer(
    cols = c(Fortified, Red, Rose, sparkling, Sweet.white, Dry.white),
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Date, key = Varietal)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Australian Wine Sales"),

  tabsetPanel(

    # --------------------- ANALYSIS TAB ---------------------
    tabPanel("Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("wine_type", "Choose varietal(s):",
                      choices = unique(wines_ts$Varietal),
                      selected = c("Red", "Dry.white"),
                      multiple = TRUE),

          dateRangeInput("date_range", "Date range:",
                         start = min(wines_ts$Date),
                         end = max(wines_ts$Date)),

          dateInput("train_end", "Training ends:",
                    value = as.Date("1992-01-01")),

          numericInput("h", "Forecast horizon (months):",
                       value = 12, min = 1)
        ),

        mainPanel(
          plotOutput("plot", height = "600px"),
          h3("Forecast Accuracy"),
          tableOutput("results"),
          h3("Model Specifications"),
          tableOutput("model_specs")
        )
      )
    ),

    # --------------------- INSTRUCTIONS TAB ---------------------
    tabPanel("Instructions",
      fluidRow(
        column(12,
          h3("How to Use This App"),
          h4("Understanding the Inputs:"),
          tags$ul(
            tags$li(strong("Date range:"), " The full time period of data to analyze (1980-01-01 to 1995-12-01 available)"),
            tags$li(strong("Training ends:"), " Where to split data for model training vs validation"),
            tags$li(strong("Forecast horizon:"), " How many months ahead to forecast FROM the training end date")
          ),
          
          h4("Recommended Settings:"),
          tags$ul(
            tags$li("Date range: 1980-01-01 to 1995-12-01 (full dataset)"),
            tags$li("Training ends: 1992-01-01 or earlier"),
            tags$li("Forecast horizon: 12-48 months"),
            tags$li("This gives you several years of validation data to compare forecasts against actual sales")
          ),
          
          h4("Important Rules:"),
          tags$ul(
            tags$li("Training end must be inside the date range"),
            tags$li("Training end should be at least 12 months before the date range end"),
            tags$li("Forecast starts FROM the training end date and extends forward by the horizon"),
            tags$li("Example: Training ends 1992-01-01 + 48-month horizon = forecasts through 1996-01-01")
          ),

          h4("Understanding the Forecast Plot:"),
          tags$ul(
            tags$li("Black line = Actual historical sales data"),
            tags$li("Colored lines = Three forecast models (TSLM, ETS, ARIMA)"),
            tags$li("Red dashed line = Training cutoff (where validation period begins)"),
            tags$li("Blue shaded region = Forecast period with prediction intervals"),
            tags$li("Each wine varietal appears in its own panel")
          ),
          
          h4("Understanding the Accuracy Table:"),
          tags$ul(
            tags$li(strong("Training rows:"), " How well models fit the training data"),
            tags$li(strong("Validation rows:"), " How well models predict the held-out validation period"),
            tags$li("Lower RMSE, MAE, MAPE values = better model performance"),
            tags$li("Compare models within the same varietal to find the best performer")
          )
        )
      )
    ),

    # --------------------- ABOUT TAB ---------------------
    tabPanel("About",
      fluidRow(
        column(12,
          h3("About This App"),
          p("This Shiny app analyzes Australian monthly wine sales."),
          p("Three forecasting models are applied (TSLM, ETS, ARIMA)."),
          p("Forecasts, model accuracy, and model specifications are displayed.")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # --------------------- FILTER DATA ---------------------
  filtered_data <- reactive({
    wines_ts |>
      filter(
        Varietal %in% input$wine_type,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      ) |>
      fill_gaps() |>
      tidyr::fill(Sales, .direction = "down") |>
      filter(!is.na(Sales))
  })

  # --------------------- TRAIN / VALIDATION SPLIT ---------------------
  split_data <- reactive({
    df <- filtered_data()
    train_end <- as.Date(input$train_end)

    # Safety checks
    if (train_end < min(df$Date) || train_end >= max(df$Date)) {
      train_end <- max(df$Date) - months(12)
    }

    training <- df |> filter(Date <= train_end)
    validation <- df |> filter(Date > train_end)

    list(
      training = training,
      validation = validation,
      train_end = train_end
    )
  })

  # --------------------- MODELING ---------------------
  models <- reactive({
    parts <- split_data()
    training <- parts$training

    if (nrow(training) < 24) return(NULL)

    tryCatch({
      training |>
        group_by_key() |> 
        model(
          TSLM  = TSLM(Sales ~ trend() + season()),
          ETS   = ETS(Sales),
          ARIMA = ARIMA(Sales)
        )
    }, error = function(e) {
      training |>
        group_by_key() |> 
        model(
          TSLM  = TSLM(Sales ~ trend()),
          ETS   = ETS(Sales ~ error("A") + trend("A") + season("N")),
          ARIMA = ARIMA(Sales ~ pdq(1,1,1) + PDQ(0,0,0))
        )
    })
  })

  # --------------------- FORECASTS ---------------------
  forecasts <- reactive({
    mdl <- models()
    if (is.null(mdl)) return(NULL)
    
    # Forecast h periods from the training end
    mdl |> forecast(h = input$h)
  })

  # --------------------- ACCURACY ---------------------
  eval_results <- reactive({
    parts <- split_data()
    mdl <- models()
    training <- parts$training
    validation <- parts$validation

    if (is.null(mdl)) {
      return(data.frame(Message = "Insufficient data"))
    }

    # Get both training and validation accuracy
    train_acc <- accuracy(mdl) |> mutate(.type = "Training")
    
    if (nrow(validation) == 0) {
      return(train_acc |> select(.model, Varietal, .type, ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE, ACF1))
    }
    
    val_forecasts <- mdl |> forecast(new_data = validation)
    val_acc <- accuracy(val_forecasts, validation) |> mutate(.type = "Validation")
    
    # Combine both
    bind_rows(train_acc, val_acc) |>
      select(.model, Varietal, .type, ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE, ACF1)
  })

  # --------------------- MODEL SPECS TABLE ---------------------
  output$model_specs <- renderTable({
    mdl <- models()
    if (is.null(mdl)) return(data.frame(Message = "No model specifications available"))

    tryCatch({
      specs_list <- list()
      
      for (i in 1:nrow(mdl)) {
        varietal <- mdl$Varietal[i]
        
        # ARIMA
        tryCatch({
          arima_model <- mdl$ARIMA[[i]]
          arima_coef <- arima_model$fit$spec
          
          # Extract ARIMA orders
          p <- arima_coef$p
          d <- arima_coef$d
          q <- arima_coef$q
          P <- arima_coef$P
          D <- arima_coef$D
          Q <- arima_coef$Q
          period <- arima_coef$period
          
          arima_spec <- sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]", p, d, q, P, D, Q, period)
          
          specs_list[[length(specs_list) + 1]] <- data.frame(
            Varietal = varietal,
            Model = "ARIMA",
            Specification = arima_spec,
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          specs_list[[length(specs_list) + 1]] <<- data.frame(
            Varietal = varietal,
            Model = "ARIMA",
            Specification = "ARIMA(auto)",
            stringsAsFactors = FALSE
          )
        })
        
        # ETS
        tryCatch({
          ets_model <- mdl$ETS[[i]]
          
          # Use tidy() to get the actual state space model specification
          ets_tidy <- tidy(ets_model)
          
          # The .model column in glance sometimes has the spec
          ets_glance <- glance(ets_model)
          
          # Try to extract from the model's state names or parameters
          if ("states" %in% names(ets_glance) && !is.null(ets_glance$states[[1]])) {
            state_names <- names(ets_glance$states[[1]])
            # Parse state names to determine ETS type
            has_level <- any(grepl("l", state_names))
            has_trend <- any(grepl("b", state_names))
            has_season <- any(grepl("s", state_names))
            
            # Determine error type from parameters
            params <- ets_tidy$term
            error_type <- if(any(grepl("gamma", params))) "M" else "A"
            trend_type <- if(has_trend) "A" else "N"
            season_type <- if(has_season) "A" else "N"
            
            ets_spec <- sprintf("ETS(%s,%s,%s)", error_type, trend_type, season_type)
          } else {
            # Last resort: extract from model summary
            model_summary <- capture.output(summary(ets_model))
            ets_line <- grep("ETS\\(", model_summary, value = TRUE)
            if (length(ets_line) > 0) {
              ets_spec <- sub(".*?(ETS\\([^)]+\\)).*", "\\1", ets_line[1])
            } else {
              ets_spec <- "ETS(A,N,N)"  # Default fallback
            }
          }
          
          specs_list[[length(specs_list) + 1]] <- data.frame(
            Varietal = varietal,
            Model = "ETS",
            Specification = ets_spec,
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          specs_list[[length(specs_list) + 1]] <<- data.frame(
            Varietal = varietal,
            Model = "ETS",
            Specification = "ETS(A,N,N)",
            stringsAsFactors = FALSE
          )
        })
        
        # TSLM
        specs_list[[length(specs_list) + 1]] <- data.frame(
          Varietal = varietal,
          Model = "TSLM",
          Specification = "TSLM(Sales ~ trend() + season())",
          stringsAsFactors = FALSE
        )
      }
      
      do.call(rbind, specs_list)
    }, error = function(e) {
      data.frame(
        Message = paste("Error:", e$message)
      )
    })
  })

  # --------------------- PLOT ---------------------
  output$plot <- renderPlot({
    df <- filtered_data()
    fc <- forecasts()
    parts <- split_data()
    train_end <- parts$train_end

    p <- df |>
      ggplot(aes(Date, Sales)) +
      geom_line(color = "black") +
      labs(
        title = "Australian Wine Sales Forecasts",
        x = "Date", y = "Sales"
      ) +
      theme_minimal() +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1)

    # Training cutoff
    p <- p + geom_vline(aes(xintercept = train_end),
                        linetype = "dashed", color = "red")

    # Forecast shading + forecast lines
    if (!is.null(fc) && nrow(fc) > 0) {
      shade_start <- min(fc$Date)
      shade_end <- max(fc$Date)

      p <- p +
        annotate("rect",
                 xmin = shade_start, xmax = shade_end,
                 ymin = -Inf, ymax = Inf,
                 alpha = 0.1, fill = "lightblue") +
        autolayer(fc, alpha = 0.8)
    }

    p
  })

  # --------------------- ACCURACY TABLE ---------------------
  output$results <- renderTable({
    eval_results()
  })
}

# ---- RUN APP ----
shinyApp(ui, server)
