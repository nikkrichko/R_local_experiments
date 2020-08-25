library(shiny)

ui <- shinyUI(
  fluidPage(
    includeHTML('D:\\github\\jtl_input_dt.html')
  )
)
server <- function(input, output) {
}

shinyApp(ui, server)
