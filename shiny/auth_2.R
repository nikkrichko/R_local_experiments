library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

# ���������� ������� ����� ���������� #
mainbody <- div(
  tabItems(tabItem("ReportTab",class = "active",
                   box(title='������ 1', footer='Lorem ipsum...',plotOutput("plot1"),solidHeader = TRUE, width=6,status = "info")
  )
  )
)
# ���������� ����� ������ #
loginbody <-   tagList(
  div(id = "logindiv",
      wellPanel(
        tags$img(src='https://www.esi-tec.com/assets/trum/dnv-gl.png', 
                 height='145px',  width = "145px", align = "center"),
        tags$br(),tags$br(),                        
        tags$b("������� ��� ����� � ������"),
        tags$br(),tags$br(),
        textInput("userName","�����"),
        passwordInput("passWord","������"),
        tags$br(),
        actionButton("loginbutton","�����")
      )),
  tags$style(HTML("#logindiv {font-size:12px; text-align:left; position:absolute; top:20%; left:50%; margin-top:-100px;margin-left:-150px;}"
  )))



# body - ������������ �� ��� ������, � ������ ������� ������������� � server() UI
body    <- dashboardBody(
  ###### ��� ������ ���� - ���������� �� js ������� ���������� ��������� ######
  shinyjs::useShinyjs(), 
  extendShinyjs(text = "shinyjs.hidehead = function(parm){$('header').css('display', parm);}"),
  ###### ��� ������� ������������� body
  uiOutput("body1")
)

# ��������� � ������� ������ - ����������� �� ������ ������, ������� ����� ���������� �����
header  <- dashboardHeader()
sidebar <- dashboardSidebar(sidebarMenu(menuItem("�����", tabName="ReportTab",icon=icon("dashboard"))),
                            tags$br(),tags$br(),tags$br(),
                            actionButton("PrintUser","������������", width = 160),
                            tags$br(),tags$br(),
                            actionButton("logoutButton","�����", width = 160)
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  user <- reactiveValues(Logged = FALSE, usrName='') # ���������� "������" ������������ �� �������� � ������
  
  
  observeEvent(input$logoutButton, # ���� ����� ����� - ���������� "������������"
               {  
                 user$Logged <- FALSE
                 user$usrName <- ''
               })  
  
  observeEvent(input$loginbutton,{ # ���� ����� ���� - ������� ������ � ��������� "������������"
    if (user$Logged == FALSE){
      Username <- as.character(isolate(input$userName))
      Password <- as.character(isolate(input$passWord))
      
      if (Username==Password){
        user$Logged <- TRUE
        user$usrName <- Username
      }
    }
  })
  
  observeEvent(input$PrintUser,{ # �������� ����� ������������ ��� ������� �����
    showModal(modalDialog(
      title="�� ����� ��� �������������:",
      div(tags$h1(user$usrName, style = "color: red; size: 36px")),      
      easyClose = TRUE, size = 's',
      footer = NULL
    ))
  })  
  
  
  
  
  observe({ # ������� ����������. ���� ������������ ����� - ���������� �������� ����. ���� ������������ �� ��������� - ���������� �����
    if(user$Logged != TRUE)     
    {
      js$hidehead('none')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      output$body1 <- renderUI({loginbody})
    }
    if(user$Logged == TRUE)     
    {
      js$hidehead('')
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      output$plot1 <- renderPlot(ggplot()+geom_point(data=cars, aes(x=speed, y=dist, color=dist)))
      output$body1 <- renderUI({mainbody})    
    }
    
  })
  
  
}

shinyApp(ui, server)