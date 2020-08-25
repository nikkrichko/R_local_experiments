library(shiny)
library(DT)
library(tidyverse)
library(data.table)
library(ggplot2)
library(esquisse)
library(shinycssloaders)
library(lubridate)


upload_jtl_file_ui <- function(id, label="upload jtl file"){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      h4("Import jmeter log (*.jtl-file)"),
      # actionButton(ns("reset_button"),"RESET"),
      fileInput(ns("Jmeter_uploaded_file"), label,
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".jtl")),
      # Horizontal line -------------------------------------
      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       tags$hr(style='display: inline-block; height: 5px;width: 100%;color: #555555; background-color: #555555')
                       ),
      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       radioButtons(ns("head_display"), "demo display",
                                    choices = c(All = "all",Head = "head"),
                                    selected = "all")),

      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       tags$hr(style='display: inline-block; height: 1px;width: 100%;color: #555555; background-color: #555555'),
                       textInput(inputId=ns("Upload_jmeter_text_input"),
                                 "Type exclude label",
                                 ),
                       actionButton(ns("Clean_requests"), "Clean_requests",style='width: 100%;color: white; background-color: #008CBA')),


      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       tags$hr(style='display: inline-block; height: 1px;width: 100%;color: #555555; background-color: #555555'),
                       textInput(inputId=ns("words_to_exclude_text_input"),
                                 "Type words to delete from labels",
                       ),
                       actionButton(ns("Clean_words_from_labels"), "Clean_words_from_labels",style='width: 100%;color: white; background-color: #008CBA')),
      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       tags$hr(style='display: inline-block; height: 1px;width: 100%;color: #555555; background-color: #555555'),
                       h5("select how many % of request you want to see in report"),
                       h6("(less - improve performance)"),
                       sliderInput(ns("percent_dots_to_show"), "Percent dots to show",
                                   min = 0, max = 100,
                                   value = 10)
      ),
      conditionalPanel(condition = "output.fileUploaded == 'TRUE'",ns = ns,
                       tags$hr(style='display: inline-block; height: 1px;width: 100%;color: #555555; background-color: #555555'),
                       actionButton(ns("end_with_preparation_button"), "End_with_prepadation",style='width: 100%;color: white; background-color: #4CAF50'))

    ),
    mainPanel(
      conditionalPanel( condition = "output.fileUploaded == 'TRUE'",ns = ns,
                        h2("Preview")),
                        verbatimTextOutput(ns("list_unique_labels")),
                        dataTableOutput(ns("uploaded_contents"))
    )

  )

}

upload_jtl_file_server <- function(id){
  moduleServer(id,
               function(input, output, session){
                 # output$uploaded_contents <- renderTable({

                 # input$file1 will be NULL initially. After the user selects
                 # and uploads a file, head of that data file by default,
                 # or all rows if selected, will be shown.
                 temp <- reactiveValues(data = NULL,
                                        exclude_filter = '',
                                        exclude_words='',
                                        end_with_preparation=FALSE,
                                        list_unique_labels='')

                 set_exclude_filter_false = function() {
                   # do stuff
                   print("set filter false")
                   temp$exclude_filter <- FALSE
                 }

                 userFile <- reactive({
                   # If no file is selected, don't do anything
                   validate(need(input$Jmeter_uploaded_file, message = FALSE))
                   temp$exclude_filter = ''
                   temp$exclude_words=''
                   temp$end_with_preparation=FALSE
                   temp$list_unique_labels=''
                   # print(input$Jmeter_uploaded_file)
                   input$Jmeter_uploaded_file
                 })


                 dataframe <- reactive({
                   print("uploading from file")
                   data <-read.csv(userFile()$datapath) %>% as.data.table()
                 })


                 temp$data <- reactive({
                   data <- dataframe()
                   data
                 })



                 output$fileUploaded <- reactive({
                   if(!is.null(temp$data())){
                     result <- "TRUE"
                   } else {
                     result  <- "FALSE"
                   }
                   if(temp$end_with_preparation){
                     result  <- "FALSE"
                   }
                   return(result)
                 })
                 outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)


                observeEvent(input$Clean_requests, {
                  req(input$Clean_requests)
                  temp$exclude_filter <- input$Upload_jmeter_text_input
                })


                observeEvent(input$Clean_words_from_labels, {
                  req(input$Clean_words_from_labels)
                  temp$exclude_words <- input$words_to_exclude_text_input
                })

                observeEvent(input$end_with_preparation_button, {
                  req(input$end_with_preparation_button)
                  temp$end_with_preparation <- TRUE

                })

                # observeEvent(input$reset_button, {
                #   req(input$reset_button)
                #
                #   # temp$data() = NULL
                #   temp$exclude_filter = ''
                #   temp$exclude_words=''
                #   temp$end_with_preparation=FALSE
                #   temp$list_unique_labels=''
                # })





                delete_specific_words <- function(input_dt, delete_words_list=NULL){
                  if(!is.null(delete_words_list)){
                    for (delete_word in delete_words_list){
                      input_dt$label <- input_dt$label %>% sub(delete_word, "", ., ignore.case = TRUE)
                    }
                  }
                  input_dt
                }


                 output$uploaded_contents <- renderDataTable({
                   # print("RENDER DATA TABLE function ------------")

                   data <- temp$data()

                   ### display only head or all
                   if(input$head_display == "head"){
                     data <- head(data)
                   }

                     if(nchar(temp$exclude_filter) > 1 ){
                       req(input$Clean_requests)

                       list_of_words_to_exclude <- strsplit(temp$exclude_filter, ",")[[1]] %>%
                         trimws() %>%
                         paste(collapse = "|")
                       # print(list_of_words_to_exclude)
                       # print("Clean_requests button was pressed")
                       data <- data[!grepl(list_of_words_to_exclude, label),]
                       # temp$exclude_filter <- FALSE

                     }


                   if(nchar(temp$exclude_words) > 1 ){
                     req(input$Clean_words_from_labels)
                     list_of_words_to_exclude <- strsplit(temp$exclude_words, ",")[[1]] %>%
                       trimws()
                     # print(list_of_words_to_exclude)
                     # print("Clean_words button was pressed")
                     data <- delete_specific_words(data,list_of_words_to_exclude)
                   }
                   if(temp$end_with_preparation){
                      # print("Assigning results data ++++++++++++++++++++++++++++++++")
                     result_data<-data
                     result_data[,":="("request_name"=label)]
                     result_data[,":="("response_time"=round(elapsed))]

                     # result_data[,":="("responseCode"=as.factor(responseCode))]
                     # result_data[,":="("responseCode"=as.factor(responseCode))]
                     # result_data[,":="("responseCode"=as.factor(responseCode))]
                     # result_data[,":="("responseCode"=as.factor(responseCode))]

                     # result_data[,":="("timeStamp"=as.numeric(timeStamp)/1000)]
                     # result_data[,":="("request_name"=label)]
                     # result_data[,":="("request_name"=label)]
                     # result_data[,":="("request_name"=label)]


                     # result_data$timeStamp <- as.numeric(result_data$timeStamp)
                     # result_data$timeStamp <- as.POSIXct(result_data$timeStamp/1000, origin="1970-01-01")



                     # result_data$dateTime <- ymd_hms(as.POSIXct(as.numeric(result_data$timeStamp)/1000, origin="1970-01-01"))

                     # result_data$dateTime <- as.character(as.POSIXct(as.numeric(result_data$timeStamp)/1000, origin="1970-01-01", tz="UTC"))


                     # result_data$responseCode <- as.factor(result_data$responseCode)
                     # result_data$responseMessage <- as.factor(result_data$responseMessage)
                     # result_data$elapsed <- as.numeric(result_data$elapsed)
                     # result_data$date <- as.Date(result_data$dateTime)
                     result_data$dateTime <- ymd_hms(as.POSIXct(as.numeric(result_data$timeStamp)/1000, origin="1970-01-01"))
                     result_data$time <- format(result_data$dateTime, "%H:%M:%OS3")
                     # result_data$fulltime <- as.POSIXct(result_data$timeStamp, origin="1970-01-01")
                     result_data$success <- as.logical(result_data$success)
                     result_data$Latency <- as.numeric(result_data$Latency)
                     # result_data$request_name <- as.factor(result_data$label)


                      # result_data[sample(.N,NROW(result_data)/10)]

                     # result_data <<- result_data
                     ### ????????????
                     result_data <<- result_data[sample(.N,round(NROW(result_data)*input$percent_dots_to_show/100))]
                     # result_data <<- result_data[sample(.N,round(NROW(result_data)*10/100))]

                     assign("result_data", result_data)
                   }
                    # print( unique(data$label))
                    temp$list_unique_labels <- unique(data$label)

                   data
                 })


                 output$list_unique_labels <- renderText({
                    if(length(temp$list_unique_labels)>1){
                      paste("UNIQUE LABELS:\nAMOUNT:",length(unique(temp$list_unique_labels)),"\n",
                      paste(unique(temp$list_unique_labels), collapse = "\n"), sep = "")
                    }
                 })


                 observe({
                   msg <- sprintf("File %s was uploaded", userFile()$name)
                   cat(msg, "\n")
                 })


               } # end funciton
  ) # end module server
} # end upload_jtl_file_server

UI_iframe_with_report <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      downloadButton(ns("downloadData"), label = "Download this report"),
      htmlOutput(ns("inc"))
    )
  )

}

SERVER_render_iframe_with_report <- function(id, input_dt=NULL){
  moduleServer(id,
               function(input, output, session){
                 ### setup requirement variables
                 myhtmlfilepath <- "C:/temp" # change to your path
                 temp_dir_path <- paste(myhtmlfilepath,"/",sep = "")
                 temp_file_name <- "jtl_input_dt.html"
                 addResourcePath('myhtmlfiles', myhtmlfilepath)
                 source_path_for_iframe <- paste0("myhtmlfiles/",temp_file_name)
                 full_path_to_ready_report_file <- paste(temp_dir_path,temp_file_name, sep="")



                 getPage<-function() {

                   report.preprocessing:::generate_report(input_data_table=input_dt,
                                                          output_dir = myhtmlfilepath,
                                                          output_file = temp_file_name)

                   return(tags$iframe(src = source_path_for_iframe,
                                      height = "1200px",
                                      width = "100%",
                                      scrolling = "yes",
                                      seamless=TRUE))

                 }

                 output$inc<-renderUI({getPage()})

                 output$downloadData <- downloadHandler(
                   filename <- function() {
                     paste("small_report_",Sys.Date(),".html",sep="")
                   },
                   content  <- function(file) {
                     file.copy(full_path_to_ready_report_file, file)
                   },
                   contentType = "html"

                 )
               }
  )
}

### UI function
ui <- function(){
  fluidPage(
    titlePanel("opening web pages"),
    tabsetPanel(id = "tabs",
      ### tab UPLOAD file ------------------------
      tabPanel("UPLOAD file",
               upload_jtl_file_ui("upload_module","Select file:")

      ),
      ### tab Small report ------------------------
      tabPanel("Small report",
                UI_iframe_with_report("small_report")
               ),
      ### tab Esquisse graph generator ------------------------
      tabPanel("Esquisse graph generator",
                 h2("esquisse"),
                 esquisserUI(
                   id = "esquisse",
                   header = FALSE,
                   disable_filters = FALSE
                 )
      )
    ### closing braces ----------------------

    )
  )
}

### server function --------------------------
server = function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)
  upload_jtl_file_server("upload_module")

  data_r <- reactiveValues(data = "", name = "")

  observeEvent(input$tabs, {
    if(input$tabs == "Esquisse graph generator" & exists("result_data")){
      data_r$data <- result_data
      data_r$name <- "result_data"

      callModule(module = esquisserServer, id = "esquisse",data=data_r)
    }
  }
  )

  observeEvent(input$tabs, {
    if(input$tabs == "Small report" & exists("result_data")){

      showNotification("Please wait some moment\nYour report generation ...\nIN PROGRESS....\nFEW MORE MOMENT\nJUST WAIT...",
                       type="warning",duration = 10)
      SERVER_render_iframe_with_report("small_report",get("result_data") )
    }
  }
  )




}

shinyApp(ui, server)


