library(shiny)

ui = fluidPage(
  titlePanel("opening web pages"),
  sidebarPanel(selectInput(
    inputId = 'test',
    label = 1,
    choices = c("jtl_input_dt", "zoo", "product","jtl_input_dt")
  )),
  mainPanel(htmlOutput("inc"))
)

server = function(input, output) {
  myhtmlfilepath <- "C:\\temp" # change to your path
  addResourcePath('myhtmlfiles', myhtmlfilepath)


  getPage <- function() {
    print(paste0("myhtmlfiles/",
                 input$test, ".html"))
    return(tags$iframe(src = paste0("myhtmlfiles/",
                                    input$test, ".html"),
                       height = "1200px",
                       width = "100%",
                       scrolling = "yes",
                       seamless=TRUE))
  }

  output$inc <- renderUI({
    req(input$test)
    getPage()
  })
}

shinyApp(ui, server)


