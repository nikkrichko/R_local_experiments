library(shiny)
library(knitr)
library(shinycssloaders)
library(plotly)




server <- function(input, output) {
  myhtmlfilepath <- "C:\\temp" # change to your path
  addResourcePath('myhtmlfiles', myhtmlfilepath)


  output$markdown <- renderUI({

    # temp_dir_path <- "c:\\temp\\"
    # temp_file_name <- "jtl_input_dt.html"
    # report.preprocessing:::generate_report(input_data_table=report.preprocessing::jdt_dirty,
    #                                        output_dir = "temp_dir_path"
    #                                        output_file = temp_file_name)
    # result_path <- paste(temp_dir_path,temp_file_name, sep="")
    # result_path
  })


  getPage<-function() {
    temp_dir_path <- "c:\\temp\\"
    temp_file_name <- "jtl_input_dt.html"
    report.preprocessing:::generate_report(input_data_table=report.preprocessing::jdt_dirty,
                                           output_dir = temp_dir_path,
                                           output_file = temp_file_name)

    result_path <- paste(temp_dir_path,temp_file_name, sep="")
    # print(temp_path)
    # result_path <- paste(temp_dir_path,"result_body.html", sep="")
    # print(print(result_path))
    # xml2::write_html(rvest::html_node(xml2::read_html(temp_path), "body"), file =result_path_2)



    # return(includeHTML(result_path))
    print(result_path)
    return(tags$iframe(src = paste0("myhtmlfiles/",
                                    "jtl_input_dt", ".html"),
                       height = "1200px",
                       width = "100%",
                       scrolling = "yes",
                       seamless=TRUE))
    # fileConn<-file('c:\\temp\\path.txt')
    # writeChar(result_path, fileConn)
    # close(fileConn)
    # return(result_path)
  }

  # output$inc<-renderUI({getPage()})
  # outputOptions(output, 'inc', suspendWhenHidden = FALSE)

  output$inc<-renderUI({getPage()})

  output$downloadData <- downloadHandler(
    filename = function() {
      "report_output.html"
    },
    content = function(con) {
      file.copy("c:\\temp\\jtl_input_dt.html", file)
      }

  )


}

ui <- shinyUI(
  fluidPage(
    # uiOutput('markdown')
    # includeHTML('c:\\temp\\jtl_input_dt.html')
    # fileName <- 'c:\\temp\\path.txt',
    # path <- readChar(fileName, file.info(fileName)$size),
    # includeHTML(path)

    # includeHTML()
    # htmlOutput("markdown")
    downloadButton("donwload_report_link", label = "Download this report"),
    htmlOutput("inc")

  )
)

shinyApp(ui, server)


# That is because you included a complete HTML file in the shiny UI, and you should only include the content between <body> and </body> (quoted from yihui)
#
# A solution could be to run an extra line to fix your Test.html automatically after running rmarkdown::render():
#
#   xml2::write_html(rvest::html_node(xml2::read_html("Test.html"), "body"), file = "Test2.html")
#
# and then have

#output$uia <- renderUI(includeHTML(path = file.path(tempdir(), "Test2.html")))



##############################

# my_data <- read.delim("c:\\temp\\path.txt")
#
#
# fileName <- 'c:\\temp\\path.txt'
# path <- readChar(fileName, file.info(fileName)$size)
#
#
#
#
# fileConn<-file(fileName)
# writeChar(paste("c:\\temp\\","helloworld.html", sep=""), fileConn)
# close(fileConn)


