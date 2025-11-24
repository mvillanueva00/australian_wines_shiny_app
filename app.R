library(tidyverse)
library(lubridate)
library(shiny)
library(tsibble)
library(fable)
library(feasts)
library(fabletools)
library(urca)

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
    tabPanel("Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("wine_type", "Choose varietal:",
                      choices = unique(wines_ts$Varietal),
                      selected = "Red",
                      multiple = FALSE),

          dateRangeInput("date_range", "Date range:",
                         start = min(wines_ts$Date),
                         end = max(wines_ts$Date)),

          dateInput("train_end", "Training ends:",
                    value = as.Date("1992-01-01")),

          numericInput("h", "Forecast horizon (months):",
                       value = 12, min = 1)
        ),

        mainPanel(
          plotOutput("plot"),
          tableOutput("results")
        )
      )
    ),

    tabPanel("Instructions",
      fluidRow(
        column(12,
          h3("How to Use This App"),
          h4("Date Selection Rules:"),
          tags$ul(
            tags$li("The dataset covers 1980-01-01 through 1995-12-01."),
            tags$li("Training end must be inside the date range."),
            tags$li("Training end must be at least 12 months earlier than end."),
            tags$li("Otherwise you may see 'Insufficient data'.")
          ),
          
          h4("Recommended Settings:"),
          tags$ul(
            tags$li("Use full range: 1980-01-01 to 1995-12-01"),
            tags$li("Training ends ≤ 1992-01-01"),
            tags$li("Gives 3+ years of validation data")
          ),

          h4("Understanding the Plot:"),
          tags$ul(
            tags$li("Black = actual data"),
            tags$li("Colors = forecasts (TSLM, ETS, ARIMA)"),
            tags$li("Red dashed = training cutoff"),
            tags$li("Blue shade = forecast region")
          )
        )
      )
    ),

    tabPanel("About",
      fluidRow(
        column(12,
          h3("About This App"),
          p("This Shiny app analyzes Australian wine sales using monthly data..."),
          p("Three models are fit (TSLM, ETS, ARIMA) and forecasts are visualized.")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  filtered_data <- reactive({
    wines_ts |>
      filter(
        Varietal %in% input$wine_type,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      ) |>
      fill_gaps(.full = TRUE) |>
      tidyr::fill(Sales, .direction = "down") |>
      filter(!is.na(Sales))
  })

  split_data <- reactive({
    df <- filtered_data()
    train_end <- as.Date(input$train_end)

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

  models <- reactive({
    parts <- split_data()
    training <- parts$training

    if (nrow(training) < 24) return(NULL)

    tryCatch({
      training |>
        model(
          TSLM = TSLM(Sales ~ trend() + season()),
          ETS = ETS(Sales),
          ARIMA = ARIMA(Sales)
        )
    }, error = function(e) {
      training |>
        model(
          TSLM = TSLM(Sales ~ trend()),
          ETS = ETS(Sales ~ error("A") + trend("A") + season("N")),
          ARIMA = ARIMA(Sales ~ pdq(1,1,1) + PDQ(0,0,0))
        )
    })
  })

  forecasts <- reactive({
    mdl <- models()
    if (is.null(mdl)) return(NULL)
    mdl |> forecast(h = input$h)
  })

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

  output$plot <- renderPlot({
    df <- filtered_data()
    fc <- forecasts()
    parts <- split_data()
    train_end <- parts$train_end

    p <- autoplot(df, Sales) +
      labs(
        title = paste(input$wine_type, "Wine Sales with Forecasts"),
        x = "Date", y = "Sales"
      ) +
      theme_minimal()

    p <- p +
      geom_vline(xintercept = train_end, linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = train_end, y = Inf, label = "Training ends",
               vjust = 1.5, hjust = -0.1, color = "red")

    if (!is.null(fc)) {
      forecast_start <- max(df$Date) + months(1)
      forecast_end <- forecast_start + months(input$h - 1)

      p <- p +
        annotate("rect",
                 xmin = forecast_start, xmax = forecast_end,
                 ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "lightblue") +
        autolayer(fc, Sales, alpha = 0.7)
    }

    p
  })

  output$results <- renderTable({
    eval_results()
  })
}

# ---- RUN APP ----
shinyApp(ui, server)
