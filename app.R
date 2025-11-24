library(shiny)

# Render the Quarto document as a Shiny app
shinyApp(
  ui = fluidPage(
    uiOutput("qmd_ui")
  ),
  server = function(input, output, session) {
    output$qmd_ui <- renderUI({
      includeMarkdown("australian_wines_shiny.qmd")
    })
  }
)
