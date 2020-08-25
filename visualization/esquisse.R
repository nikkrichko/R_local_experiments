library(esquisse)

options("esquisse.display.mode" = "dialog")

dt <- mtcars


library(shiny)
library(DT)
library(shinyalert)
#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable


## module UI
test_data_table_ui  <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(outputId = ns("my_data_table")),
    textOutput(outputId = ns("my_text"))  
  )
  
}

## module server
test_data_table_server <- function(input, output, session ){
  ns = session$ns
  
  myValue <- reactiveValues(check = '')
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  
  my_data_table <- reactive({
    tibble::tibble(
      Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
      Motivation = c(62, 73, 3, 99, 52),
      Actions = shinyInput(actionButton, 5,
                           'button_',
                           label = "Fire",
                           onclick = paste0('Shiny.onInputChange(' , ns("select_button"), ', this.id)')
      )
    )
  })
  
  output$my_data_table <- DT::renderDataTable({
    return(my_data_table())
  }, escape = FALSE)
  
  
  observeEvent(input$select_button, {
    print(input$select_button)
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$check <<- paste('click on ',my_data_table()[selectedRow,1])
  })
  
  
  output$my_text <- renderText({
    myValue$check
  })
  
  
}


ui <- fluidPage(
  useShinyalert(),
  test_data_table_ui(id = "test_dt_inside_module")
)

server <- function(input, output, session) {
  callModule(module = test_data_table_server , id = "test_dt_inside_module")
}

shinyApp(ui, server)

restart()