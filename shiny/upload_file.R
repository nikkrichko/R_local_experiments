library(shiny)
library(DT)
library(data.table)
library(ggplot2)

ui <- function(){
  fluidPage(
    titlePanel("opening web pages"),
    tabsetPanel(
      tabPanel("UPLOAD file",
        h1("upload JTL file"),
        sidebarPanel(
          h2("side bar panel"),
          fileInput("Jmeter_uploaded_file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv", ".jtl")),
          # Horizontal line -------------------------------------
          conditionalPanel(condition = "output.fileUploaded",
                           tags$hr()),
          conditionalPanel(condition = "output.fileUploaded",
                           radioButtons("disp", "Display",
                                        choices = c(Head = "head",
                                                    All = "all"),
                                        selected = "head")),

          conditionalPanel(condition = "output.fileUploaded",
                           textInput(inputId="Upload_jmeter_text_input",
                                     "Type id (optional)",
                                     paste("scenario-Upload_by_user",format(Sys.Date(), "%Y_%m_%d"),sep = "-"))),
          conditionalPanel(condition = "output.fileUploaded",
                           "format: scenario_name-build-id"),
          conditionalPanel(condition = "output.fileUploaded",
                           actionButton("upload_file_to_DB_button", "Upload data to DB"))


        ),
        mainPanel(
          h2("main panel"),
          tableOutput("uploaded_contents"),
          # dynamic pages does not work without next string
          #"is preview"#,textOutput("fileUploaded"),

        )
        )
      )

  )

}


server = function(input, output) {

  output$uploaded_contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$Jmeter_uploaded_file)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$Jmeter_uploaded_file$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

  getData <- reactive({
    if(is.null(input$Jmeter_uploaded_file)) {return(NULL)
    } else {
      return(TRUE)
    }
  })

  output$fileUploaded <- reactive({
    print
    return(!is.null(getData()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observeEvent(input$upload_file_to_DB_button, {
    print(input$Jmeter_uploaded_file$datapath)
    print(input$Upload_jmeter_text_input)
    print("file uploaded")


    user_entered_build_id <- input$Upload_jmeter_text_input
    user_uploaded_file <- input$Jmeter_uploaded_file$datapath


    # post_curl <- paste('curl --insecure -v -F requested_field=@',
    #                    user_uploaded_file,
    #                    ' POST http://perf-test.eng.hintmd.com/api/jmeter_file?build_name=',
    #                    user_entered_build_id,
    #                    ' -H  "accept: application/json" -u test:test', sep="")
    # print(post_curl)
    # post_curl <- 'curl --insecure -v -F requested_field=@/home/rstudio/results.jtl POST http://perf-test.eng.hintmd.com/api/jmeter_file?build_name=krychko-mykyta-999 -H  "accept: application/json" -u test:test'
    # response <- system(post_curl)
    # if (response == 0) {
    #   print("file was upload successfully")
    # } else (
    #   print("ERROR: something goes wrong with file downloading")
    # )

  })


}

shinyApp(ui, server)


