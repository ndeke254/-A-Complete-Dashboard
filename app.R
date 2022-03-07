library(ggplot2)
library(readr)
library(readxl)
library(plotly)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(bslib)
library(tidyverse)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFeedback)
library(shinyalert)
library(shinyjs)
library(png)
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Candara"
  ,appFontColor = "rgb(0,0,153)"
  ,primaryFontColor = "rgb(0,0,255)"
  ,infoFontColor = "rgb(8,58,68)"
  ,successFontColor = "rgb(238,255,0)"
  ,warningFontColor = "rgb(255,255,0)"
  ,dangerFontColor = "rgb(255,0,0)"
  ,bodyBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(47,206,143)"
    ,colorMiddle = "rgb(81,242,177)"
    ,colorEnd = "rgb(145,233,198)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  
  ### header
  ,logoBackColor = "rgb(51,153,115)"
 
  ,headerButtonBackColor = "rgb(0,255,255)"
  ,headerButtonIconColor = "rgb(0,0,204)"
  ,headerButtonBackColorHover = "rgb(255,255,0)"
  ,headerButtonIconColorHover = "rgb(102,51,0)"
  
  ,headerBackColor = "rgb(0,179,179)"
  ,headerBoxShadowColor = "#33001a"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor ="rgb(47,206,143)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "53,255,148"
  ,sidebarMenuPadding = 0.5
  ,sidebarMenuBorderRadius = 0.5
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#33001a"
  
  ,sidebarUserTextColor = "rgb(0,255,255)"
  
  ,sidebarSearchBackColor = "rgb(189,54,121)"
  ,sidebarSearchIconColor = "rgb(102,51,0)"
  ,sidebarSearchBorderColor = "rgb(102,255,255)"
  
  ,sidebarTabTextColor = "rgb(8,2,191)"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(102,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(13,124,94)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(64,157,203)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(44,222,235)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "#33001a"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(170,234,5)"
  ,boxPrimaryColor = "rgb(138,268,183)"
  ,boxInfoColor = "rgb(0,179,179)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(0,179,179)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(44,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(17,12,156)"
  ,tabBoxTabTextColorSelected = "rgb(90,252,154)"
  ,tabBoxBackColor = "rgb(3,3,156)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(102,255,255)"
  ,buttonTextColor = "rgb(0,0,102)"
  ,buttonBorderColor = "rgb(0,204,0)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(255,255,0)"
  ,buttonTextColorHover = "rgb(102,51,0)"
  ,buttonBorderColorHover = "rgb(118,118,102)"
  
  ,textboxBackColor = "rgb(173,173,133)"
  ,textboxBorderColor = "rgb(255,255,0)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(0,255,255)"
  ,textboxBorderColorSelect = "rgb(102,255,255)"
  
  ### tables
  ,tableBackColor = "rgb(75,255,255)"
  ,tableBorderColor = "rgb(179,0,218)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  )
header <- dashboardHeader(
  tags$li(class = "dropdown",
                              tags$style(".main-header {max-height: 60px}"),
                              tags$style(".main-header .logo {height: 70px}")),
                              title = tags$a(href='http://company.fr/', 
                                               'INFINICALS',
                                         style = "color:yellow; 
                                         font-family:Candara;
                                         font-size:24px;
                                         font-style: bold;",
                                         tags$img(src='logo.png')))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),
                            tags$style(".left-side, .main-sidebar
                                       {padding-top: 80px}"))
body <- dashboardBody(uiOutput("body"))
ui <- fluidPage(useShinyFeedback(),
                customTheme,
                dashboardPage(header,sidebar, body),
                tags$head(includeScript("returnClick.js")),
                style='padding-left:0px;padding-right:0px')
login_details <- data.frame(user = c("JEFFERSON","SAM", "PAM", "RON"),
                            pswd = c("123A","123B","123C","123D"))
login <- dashboardBody(
  fluidRow(
    column(12,offset=1,box(icon=icon("lock"),title = " SIGN IN TO INFINICALS",status="warning"
             ,background="olive",solidHeader= TRUE, 
             textInput("userName",label = div(icon("user-plus",
                                          style = "color:yellow;"),'Username')),
             passwordInput('passwd',label = div(icon("key", 
                                                     style = "color:yellow;"
                                                     ),"Password")),
             br(),
             actionButton("Login", "Log in"),
             br(),
             br(),
             tags$a(href='http://company.fr/',
                    'Forgotten Password',style = "color:#0000b6;"),
             br(),
             tags$a(href='http://jeff.com/',
                    'New User',style = "color:#0000b6;")
                    ))))
server <- function(input, output, session) {
  datae <-reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    kenya_status1 %>%
      filter(
        indicator== input$indicator,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))})
  datae2<-reactive({
    req(input$choose_item)
    req(input$date4)
    validate(need(!is.na(input$date4[1]) & !is.na(input$date4[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date4[1] < input$date4[2], "Error: Start date should be earlier than end date."))
    exports1%>%filter(
      exports1$type%in%input$choose_category &
        exports1$crop%in%input$choose_item & dates > as.POSIXct(input$date4[1]) & dates < as.POSIXct(input$date4[2]))
    })
  datae3<-reactive({
    validate(need(!is.na(input$select_crop), "Select a maximum of three Crops"))
    exports1%>% select(dates,crop,weight)%>% filter(exports1$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2]))
  })
  upload<- reactive({
    req(input$file1)
   ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           tsv = vroom::vroom(input$file1$datapath, delim = "\t"),
           xls = vroom::vroom(input$file1$datapath, delim = ","),
           xlsx = vroom::vroom(input$file1$datapath, delim = ","),
           validate("Invalid file: Please upload a .csv , .tsv , .xls or .xlsx file!")
    )
  })
  style <- reactive({
    input$graph_type
  })
  plottype <- reactive({
    switch(style(),
           "Line" = ggplot(datae3(),aes(x=dates,y=weight,group=crop))+
             geom_line(aes(col=crop),linetype='solid'),
           "Comparative Bar" = ggplot(datae3(),aes(x=dates,y=weight,fill=crop))+geom_bar(stat='identity',position='dodge'),
           "Cumulative Bar"= ggplot(datae3(),aes(x=dates,y=weight,fill=crop))+geom_bar(stat='identity'))
  })
  login.page = 
  paste(
  isolate(session$clientData$url_protocol),
  "//",
  isolate(session$clientData$url_hostname),
  ":",
  isolate(session$clientData$url_port),
  sep = ""
  )
USER <- reactiveValues(Logged = F)
observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if (nrow(login_details[login_details$user == Username & 
                             login_details$pswd == Password,]) >= 1)                        {USER$Logged <- TRUE
          }
          }
        }
      }
    }
  )
observeEvent(input$Login, {
  shinyalert('PLEASE WAIT', "Signing in...", 
             type = "info", 
             timer =1300,
             showConfirmButton = FALSE,
             animation = "pop",
             imageUrl= 'logo.png'
             )
  })
  observeEvent(input$Login, { 
    if(USER$Logged!=TRUE){
    shinyFeedback::showFeedbackDanger(inputId = 'passwd',
                                      text = 'INVALID DETAILS!')
      } else {
        hideFeedback('passwd')
        }
    })
  observeEvent(input$Login, { 
    if(USER$Logged!=TRUE){
      shinyFeedback::showFeedbackDanger(inputId = 'userName',text = '')
    } else {
      hideFeedback('userName')
    }
  })
exports1<- readxl::read_xlsx(path='~/Programming/R/DATA/exports1.xlsx')
exports<- readxl::read_xlsx(path='~/Programming/R/DATA/exports.xlsx')
kenya_status1<- readxl::read_xlsx('~/Programming/R/DATA/kenya_status1.xlsx')
  output$sidebarpanel <-renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("user"), "LOG OUT", href = login.page)
        ),
        sidebarMenu(
          menuItem('Dashboard',
                   tabName='t_item1',
                   icon=icon('tachometer-alt')),
          menuItem("Company data",
                   tabName = "t_item2",
                   icon = icon("database")),
          menuItem(
            "sales trend",
            tabName = "t_item3",
            icon = icon("chart-line")),
          menuItem('Your data',
                   tabName='t_item4',
                   icon = icon('file-import')),
          menuItem('Settings',
           tabName='t_item5',
           icon = icon('hammer')),
          menuItem('About',
                   tabName ='t_item6',
                   icon= icon('address-card'))
        )
      )
    }
  })
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tabItems(
        tabItem(tabName = 't_item1',
                fluidPage(
                  titlePanel('DATA FOR A SINGLE CROP BASED ON TYPE'),
                  output$page<-renderUI({tabsetPanel(
                    tabPanel(title='Crops Data',icon=icon('table'),
                             titlePanel('Data Table'),
                             sidebarLayout(
                               sidebarPanel(
                  selectInput(
                    inputId="choose_category",
                    label= "TYPE OF EXPORTS",
                    multiple = FALSE,
                    choices = unique(exports1$type),
                    selected= unique(exports1$type)[1]),
                  output$cat_choice <- renderUI({
                    selectInput(inputId="choose_item",
                                label="Select Crop",
                                multiple= FALSE,
                                selected=unique(exports1$crop)[1], 
                                choices = unique(exports1
                                                 [exports1$type==input$choose_category,"crop"]))}),
                  dateRangeInput(inputId = 'date3',
                                 label = strong('Period'),
                                 start= min(exports1$dates),
                                 end=max(exports1$dates),
                                 min=min(exports1$dates),
                                 max=max(exports1$dates)),
                  tags$head(
                    tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))
                  )),
                  mainPanel(
        output$text1<-renderText({
          req(input$date3)
          validate(need(!is.na(input$date3[1]) & !is.na(input$date3[2]), "Error: Please provide both a start and an end date."))
                  validate(need(input$date3[1] < input$date3[2], "Error: Start date should be earlier than end date."))}),
                  output$tb_chosen <-renderDataTable(exports1%>%filter(
                    exports1$type%in%input$choose_category &
                    exports1$crop%in%input$choose_item & dates > as.POSIXct(input$date3[1]) & dates < as.POSIXct(input$date3[2])))
                  ))),tabPanel(title='Graph',icon=icon('chart-area'),
                             dateRangeInput(inputId = 'date4',
                                            label = strong('Period'),
                                            start= min(exports1$dates),
                                            end=max(exports1$dates),
                                            min=min(exports1$dates),
                                            max=max(exports1$dates)),
                             tags$head(
                               tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))
                             ),
                             output$table5<-renderPlotly({
                               ggplot(datae2(),aes(x=dates,y=weight))+
                                 geom_line(linetype='solid',col='tomato3')+
                                 labs(x='Dates', y='Weight Harvested',title=paste('weight of',input$choose_item,'exported',sep=' '),subtitle='A graphical representation of each crop',caption='Jefferson Ndeke,Econstats')+
                                 theme(plot.title = element_text(size=15,hjust = 0.5,face = 'bold'),plot.subtitle =element_text(hjust = 0.5),plot.caption = element_text(hjust = 0,face = 'italic'),axis.text = element_text(angle=45,hjust=1))
                             })
                               
                             ))})
                )),
        tabItem(
          tabName = "t_item2",
          fluidPage(
            titlePanel('COMPARISON DATA'),
            output$page <-renderUI({tabsetPanel(tabPanel(title='Data Table',icon=icon('table'),titlePanel('VIEW DIFFERENT CROPS'),
            selectInput(inputId="col_view",
                        label="Select Crops to view",
                        multiple = TRUE,
                        selected= unique(exports$dates),
                        choices = unique(colnames(exports))),
            dateRangeInput('datum',strong('Period'),
                           start= min(exports$dates),
                           end=max(exports$dates),
                           min=min(exports$dates),
                           max=max(exports$dates)),
            tags$head(
              tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))
              ),
            output$DateRange <- renderText({
              req(input$datum)
              validate(need(!is.na(input$datum[1]) & !is.na(input$datum[2]),
                         "Error: Please provide both a start and an end date."))
              validate(need(input$datum[1] < input$datum[2], "Error: Start date should be earlier than end date."))
              }),
            output$table1<- renderDataTable(
              exports%>% select(dates,input$col_view)%>% filter(
                dates > as.POSIXct(input$datum[1]) & dates < as.POSIXct(input$datum[2])))
            ),
            tabPanel(title='Visualise', icon=icon('eye'),titlePanel('GRAPHS'),
                       pickerInput(
                       inputId = 'select_crop',
                       label = strong('Select Crop'),
                       multiple = TRUE,
                       options=list(`max-options`=3),
                       choices =unique(exports1$crop),
                       selected =''),
                       dateRangeInput(inputId = 'date2',
                                      label = strong('Period'),
                                      start= min(exports1$dates),
                                      end=max(exports1$dates),
                                      min=min(exports1$dates),
                                      max=max(exports1$dates)),
                     tags$head(
                       tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))),
                     fluidRow(tabsetPanel(tabPanel(title='Table',
                     output$DateRange2 <- renderText({
                       req(input$date2)
                       validate(need(!is.na(input$date2[1]) & !is.na(input$date2[2]),
                                     "Error: Please provide both a start and an end date."))
                       validate(need(input$date2[1] < input$date2, "Error: Start date should be earlier than end date."))
                     }),
                     output$tableu<- renderDataTable(
                       exports1%>% select(dates,crop,weight)%>% filter(exports1$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2]))
                     )),
                    tabPanel(title='Graphs',titlePanel('Type of Graph'),
                             radioButtons(inputId='graph_type',label='Choose the Graph',choices=c('Line','Comparative Bar','Cumulative Bar'),selected=NULL ),
                             output$graph3<-renderPlotly({
                       plottype() +
                         labs(fill='Crops',x='Dates', y='Weight Harvested',title=paste('A',input$graph_type,'Graph for',input$select_crop[1],input$select_crop[2],'and',input$select_crop[3],'exported',sep=' '),subtitle='Comparison of Crops Weights exported',caption='Jefferson Ndeke,Econstats')+
                         theme(plot.title = element_text(size=15,hjust = 0.5,face = 'bold'),plot.subtitle =element_text(hjust = 0.5),plot.caption = element_text(hjust = 0,face = 'italic'),axis.text = element_text(angle=45,hjust=1))
                     }))))
                     )
            )
              })
            )
          ),
        tabItem(tabName = "t_item3",
                output$page<-renderUI({
                fluidPage(titlePanel('TRENDS'),tabsetPanel(tabPanel(
                  title ='Graphs',icon = icon('chart-pie'),
                          titlePanel('KENYA DEBT'),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "indicator",
                                          label = strong("Type of Debt"),
                                      choices = unique(kenya_status1$indicator),
                                          selected = "domestic_debt"),
                              dateRangeInput("date",
                                             strong("period"), 
                                             start = "1999-09-01",
                                             end = "2021-06-01",
                                             min = "1999-09-01",  
                                             max = "2021-06-01")
                            ),
                            mainPanel(
                              tags$head(
                                tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))
                              ),
                              tags$a(href = "https://www.centralbank.go.ke/public-debt/", "Source: Central Bank of Kenya", target = "_blank"),
                           
      output$table4<-renderPlotly({
    ggplot(datae(),aes(x=date,y=value))+
          geom_line(linetype='solid',col='tomato3')+
          labs(x='Dates', y='Debt Trend',title=paste('Debt Analysis of',input$indicator,sep=' '),subtitle='Analysis of Kenyan Debt by Type',caption='Jefferson Ndeke,Econstats')+
          theme(plot.title = element_text(size=15,hjust = 0.5,face = 'bold'),plot.subtitle =element_text(hjust = 0.5),plot.caption = element_text(hjust = 0,face = 'italic'),axis.text = element_text(angle=45,hjust=1))
       })
         ))),
      tabPanel(title ='Summary',icon = icon('calculator'))))})),
           tabItem(
              tabName = 't_item4',
             fluidPage(
               output$page<- renderUI({
                 tabsetPanel(
                 tabPanel('import Data',
                          fileInput('file1','Data',buttonLabel='Upload...',accept=c('.cvs','.tsv','.xls','.xlsx')),
                          numericInput('n','No. of Rows to preview',value=10,min=1,step=1),
                          tags$head(
                            tags$style(HTML("
                              .shiny-output-error-validation {
                              color: #ff0000;
                              font-weight: bold;
                              }
                              "))
                          ),
                          output$head <- renderDataTable({
                            head(upload(), input$n)
                          }),
                          output$download <- downloadHandler(
                            filename = function() {
                              paste0(input$file1, ".csv")
                            },
                            content = function(file) {
                              vroom::vroom(upload(),file)
                            }
                          )
                          ),
                 tabPanel('Set parameters'),
                 tabPanel('visualise Results'
                 ))
               })
             )
           ),
    tabItem(tabName='t_item6',
            fluidPage(
              output$page<- renderUI({
                
              })
            ))
        )
      } else {
      login
    }
  })
}
shinyApp(ui = ui, server = server)


