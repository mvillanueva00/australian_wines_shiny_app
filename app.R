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
                      selected = c("Red", "White"),
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
          h4("Date Selection Rules:"),
          tags$ul(
            tags$li("Dataset covers 1980-01-01 through 1995-12-01."),
            tags$li("Training end must be inside the date range."),
            tags$li("Training end must be at least 12 months earlier than end."),
            tags$li("Otherwise 'Insufficient data' will appear.")
          ),
          
          h4("Recommended Settings:"),
          tags$ul(
            tags$li("Use full range: 1980-01-01 to 1995-12-01"),
            tags$li("Training ends ≤ 1992-01-01"),
            tags$li("Gives 3+ years of validation data")
          ),

          h4("Understanding the Forecast Plot:"),
          tags$ul(
            tags$li("Black = Actual data"),
            tags$li("Colored lines = Forecast models (TSLM, ETS, ARIMA)"),
            tags$li("Red dashed = Training cutoff"),
            tags$li("Blue shaded region = Forecast horizon"),
            tags$li("Each varietal appears in its own facet panel")
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
    mdl |> forecast(h = input$h)
  })

  # --------------------- ACCURACY ---------------------
  eval_results <- reactive({
    parts <- split_data()
    mdl <- models()
    validation <- parts$validation

    if (is.null(mdl) || nrow(validation) == 0) {
      return(data.frame(Message = "Insufficient data for validation"))
    }

    val_forecasts <- mdl |> forecast(new_data = validation)
    accuracy(val_forecasts, validation)
  })

  # --------------------- MODEL SPECS TABLE ---------------------
  output$model_specs <- renderTable({
    mdl <- models()
    if (is.null(mdl)) return(data.frame(Message = "No model specifications available"))

    mdl |>
      glance() |>
      select(Varietal, .model, arima_order, ets_components) |>
      rename(
        Model = .model,
        ARIMA = arima_order,
        ETS = ets_components
      )
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
    if (!is.null(fc)) {
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
