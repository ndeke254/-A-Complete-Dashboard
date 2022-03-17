library(ECharts2Shiny)
library(waiter)
library(echarts4r)
library(readr)
library(readxl)
library(shiny)
library(shinycssloaders)
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
  ,appFontColor = "rgb(8, 2, 132)"
  ,primaryFontColor = "rgb(8, 2, 132)"
  ,infoFontColor = "rgb(8,58,68)"
  ,successFontColor = "rgb(238,255,0)"
  ,warningFontColor = "rgb(255,255,0)"
  ,dangerFontColor = "rgb(255,0,0)"
  ,bodyBackColor = "rgb(177, 219, 0)"
  
  ### header
  ,logoBackColor = "rgb(51,153,115)"
  
  ,headerButtonBackColor = "rgb(177, 219, 0)"
  ,headerButtonIconColor = "rgb(0,0,204)"
  ,headerButtonBackColorHover = "rgb(102,255,255)"
  ,headerButtonIconColorHover = "rgb(102,51,0)"
  
  ,headerBackColor = "rgb(0,179,179)"
  ,headerBoxShadowColor = "#33001a"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor ="rgb(190, 135, 0)"
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
  
  ,sidebarTabTextColor = "rgb(8, 2, 132)"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(0, 0, 3)"
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
  ,sidebarTabTextColorSelected = "rgb(0, 113, 0)"
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
  ,boxBackColor = "rgb(190, 135, 0)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgb(107, 33, 104)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(170,234,5)"
  ,boxPrimaryColor = "rgb(138,268,183)"
  ,boxInfoColor = "rgb(0,179,179)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(0,179,179)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(44,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(2, 3, 102)"
  ,tabBoxTabTextColorSelected = "rgb(90,252,154)"
  ,tabBoxBackColor = "rgb(3,3,156)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(255, 255, 0)"
  ,buttonTextColor = "rgb(0,0,102)"
  ,buttonBorderColor = "rgb(0, 255, 0)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(0,255,255)"
  ,buttonTextColorHover = "rgb(163, 164, 162)"
  ,buttonBorderColorHover = "rgb(118,118,102)"
  
  ,textboxBackColor = "rgb(255, 255, 0)"
  ,textboxBorderColor = "rgb(0,255,255)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(0,255,255)"
  ,textboxBorderColorSelect = "rgb(254, 1, 0)"
  
  ### tables
  ,tableBackColor = "rgb(190, 135, 0)"
  ,tableBorderColor = "rgb(7, 255, 255)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)
header <- dashboardHeader(title = tags$a(tags$img(src='logo.png'),
                                         href='http://company.fr/','INFINICALS',
                                         style = "color:yellow; 
                                         font-family:Candara;
                                         font-size:24px;
                                         font-style: bold;"),
                          userOutput('user'))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(uiOutput("body"),
  customTheme,useShinyFeedback(),
  tags$head(includeScript('returnClick.js'))
                      )
ui <-dashboardPage(header,sidebar,body,
                   options = list(sidebarExpandOnHover = TRUE),
                   preloader = list(html = tagList(spin_orbiter(), "Loading ..."), color = "#b1db00")
                   )
login_details <- data.frame(user = c("JEFFERSON","SAM", "PAM", "RON"),
                            pswd = c("123A","123B","123C","123D"))
login <-fluidRow(
  column(12,
         br(),
         br(),
         br(),
         box(icon=icon("lock"),title = " SIGN IN TO INFINICALS",
             status="warning",
             solidHeader= TRUE,
             textInput("userName",
                       label = div(icon("user-plus",
                                        style = "color:yellow;"),'Username')),
             passwordInput('passwd',
                           label = div(icon("key", 
                                            style = "color:yellow;"),
                                       "Password")),
             br(),
             actionButton("Login", "Log in"),
             br(),
             br(),
             tags$a(href='http://company.fr/',
                    'Forgotten Password',style = "color:#0000b6;"),
             br(),
             tags$a(href='http://jeff.com/',
                    'New User',style = "color:#0000b6;")
         ))
  )
server <- function(input, output, session) {
  exports1<- readxl::read_xlsx(path='~/Programming/R/DATA/exports1.xlsx')
  exports<- readxl::read_xlsx(path='~/Programming/R/DATA/exports.xlsx')
  kenya_status1<- readxl::read_xlsx('~/Programming/R/DATA/kenya_status1.xlsx')
  output$menu<-renderMenu({
    sidebarMenu(
      menuItem('Dashboard',
               tabName='t_item1',
               icon=icon('tachometer-alt')),
      menuItem("Company data",
               tabName = "t_item2",
               icon = icon("database")),
      menuItem("sales trend",
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
  })
  output$text1<-renderText({
    req(input$date3)
    validate(need(!is.na(input$date3[1]) & !is.na(input$date3[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date3[1] < input$date3[2], "Error: Start date should be earlier than end date."))
  })
  output$tb_chosen <-renderDataTable(exports1%>%filter(
    exports1$type%in%input$choose_category &
      exports1$crop%in%input$choose_item & dates > as.POSIXct(input$date3[1]) & dates < as.POSIXct(input$date3[2])))
  datae2<-reactive({
    req(input$choose_item)
    req(input$date4)
    validate(need(!is.na(input$date4[1]) & !is.na(input$date4[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date4[1] < input$date4[2], "Error: Start date should be earlier than end date."))
    exports1%>%filter(
      exports1$type%in%input$choose_category &
        exports1$crop%in%input$choose_item & dates > as.POSIXct(input$date4[1]) & dates < as.POSIXct(input$date4[2]))
  })
  output$graph1<-renderEcharts4r({
    datae2()|> 
      group_by(crop)|>
      e_charts(dates)|>
      e_line(weight,symbol='none')|> 
      e_animation(duration = 4000)|>
      e_tooltip(trigger='axis')|>
      e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
      e_title(paste('Weight of',input$choose_item,'exported',sep=' '),
              left='center',top=10)|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_legend(orient = 'vertical',right = '5', top = '15%')|>
      e_color(my_colors)
  })
  output$DateRange <- renderText({
    req(input$datum)
    validate(need(!is.na(input$datum[1]) & !is.na(input$datum[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$datum[1] < input$datum[2], "Error: Start date should be earlier than end date."))
  })
  output$table2<- renderDataTable(
    exports%>% select(dates,input$col_view)%>% filter(
      dates > as.POSIXct(input$datum[1]) & dates < as.POSIXct(input$datum[2])))
  output$DateRange2 <- renderText({
    req(input$date2)
    validate(need(!is.na(input$date2[1]) & !is.na(input$date2[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date2[1] < input$date2[2], "Error: Start date should be earlier than end date."))
  })
  output$text2<-renderText({
    validate(need(!is.na(input$select_crop), "Select a maximum of three Crops"))
    validate(need(!is.na(input$date2[1]) & !is.na(input$date2[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date2[1] < input$date2[2], "Error: Start date should be earlier than end date."))
  })
  output$table22<- renderDataTable(
    exports1%>% select(dates,crop,weight)%>% filter(exports1$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])))
  datae3<-reactive({
    validate(need(!is.na(input$select_crop), "You have not selected any crop to view"))
    exports1%>% select(dates,crop,weight)%>% filter(exports1$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2]))
  })
  style <- reactive({
    input$graph_type
  })
  my_colors<-c('#01038d','#d80105','#01fff8')
  plottype <- reactive({
    switch(style(),
           "Line" = datae3()|> 
             group_by(crop)|>
             e_charts(dates)|>
             e_line(weight,symbol='none')|> 
             e_animation(duration = 4000)|>
             e_tooltip(trigger='axis')|>
             e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
             e_title(paste('A',input$graph_type,'Graph of',input$select_crop[1],',',input$select_crop[2],'and',input$select_crop[3]),
                     left='center',top=10)|>
             e_toolbox_feature(feature = "saveAsImage")|>
             e_legend(orient = 'vertical',right = '5', top = '15%')|>
             e_color(my_colors),
           'Bar'= datae3()|> 
             group_by(crop)|>
             e_charts(dates,timeline=TRUE)|>
             e_bar(weight,itemStyle = list(
               borderColor = "red", borderWidth = '1'))|>
             e_animation(duration = 4000)|>
             e_timeline_opts(autoPlay = TRUE, top = "55")|>
             e_tooltip(trigger='axis')|>
             e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
             e_title(paste('A',input$graph_type,'Graph of',input$select_crop[1],',',input$select_crop[2],'and',input$select_crop[3]),
                     left='center',top=10)|>
             e_toolbox_feature(feature = "saveAsImage")|>
             e_legend(orient = 'vertical', 
                      right = '5', top = '15%')|>
             e_color(my_colors),
           "Comparative Bar" = datae3()|> 
             group_by(crop)|>
             e_charts(dates)|>
             e_bar(weight)|>
             e_animation(duration = 4000)|>
             e_tooltip(trigger='axis')|>
             e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
             e_title(paste('A',input$graph_type,'Graph of',input$select_crop[1],',',input$select_crop[2],'and',input$select_crop[3]),
                     left='center',top=10)|>
             e_toolbox_feature(feature = "saveAsImage")|>
             e_legend(orient = 'vertical', 
                      right = '5', top = '15%')|>
             e_color(my_colors),
           'Pie'= datae3()|>
             group_by(dates)|>
             e_charts(crop)|>
             e_pie(weight)|>
           e_animation(duration = 4000)|>
             e_title(paste('A',input$graph_type,'Graph of',input$select_crop[1],',',input$select_crop[2],'and',input$select_crop[3]),
                     left='center',top=10)|>
             e_toolbox_feature(feature = "saveAsImage")|>
             e_legend(orient = 'vertical', 
                      right = '5', top = '15%')|>
             e_color(my_colors)
           )
  })
  datam<-reactive({
  exports%>% select(dates,avocado,tea,coffee)%>% 
      data.frame(row.names = 1)%>% head()
  })
renderBarChart(div_id = "test", 
                            grid_left = '1%', 
                            direction = "vertical",
                            data=datam())
  output$graph2<-renderEcharts4r({
    plottype() 
          
  })
  datae <-reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    kenya_status1 %>%
      filter(
        indicator== input$indicator,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  output$graph3<-renderEcharts4r({
    datae()|>
      e_charts(date)|>
      e_line(value,symbol='none')|>
      e_animation(duration = 4000)|>
      e_tooltip(trigger='axis')|>
      e_axis_labels(x='Years',y = 'Level of debt in Ksh.')|>
      e_title(paste('Trend in',input$indicator,'in Kenya'),
              left='center',top=10)|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_color(my_colors)|>
      e_legend(orient = 'vertical', 
               right = '5', top = '15%')
  })
  doc<- data.frame(val = c(0.9, 0.5, 0.4))
  output$liquid<- renderEcharts4r({
    doc|>
      e_charts()|>
      e_liquid(val)|>
      e_animation(duration = 4000)|>
      e_title('Total Debt to Debt Ceiling Ratio ',left='center')
  })
output$clock<-renderEcharts4r({
  e_charts() |> 
    e_gauge(62, "PERCENT") |> 
    e_animation(duration = 4000)|>
    e_title('Total Debt to GDP Ratio',left='center')
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
  output$head <- renderDataTable({
    head(upload(), input$n)
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
  output$sidebarpanel <-renderUI({
    if (USER$Logged == TRUE) {
      sidebarMenu(id = "menu",
                  sidebarMenuOutput("menu")
                  )
    }
  })
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      output$user<-renderUser({
        dashboardUser(
        name=input$userName,
        image='user_image.png',
        footer=p('Beyond Infinity',class='text-centre'),
        subtitle = a(icon("user"), "LOG OUT", href = login.page)
      )})
      tabItems(
        tabItem(tabName = 't_item1',
                fluidRow(
                  column(12,
                  titlePanel('DATA FOR A SINGLE CROP BASED ON TYPE'),
                  output$page1<-renderUI({
                    tabsetPanel(
                      tabPanel(title='Crops Data',icon=icon('table'),
                               titlePanel('Data Table'),
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="choose_category",
                                               label= "TYPE OF EXPORTS",
                                               multiple = FALSE,
                                               choices = unique(exports1$type),
                                               selected= unique(exports1$type)[1]),
                                   output$cat_choice<-renderUI({
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
                                     tags$style(
                                       HTML(".shiny-output-error-validation {color: #ff0000;font-weight: bold;}")))
                                 ),
                                 mainPanel(
                                   textOutput('text1'),
                                   dataTableOutput('tb_chosen'))
                               )
                      ),
                      tabPanel(title='Graph',
                               icon=icon('chart-area'),
                               dateRangeInput(inputId = 'date4',
                                              label = strong('Period'),
                                              start= min(exports1$dates),
                                              end=max(exports1$dates),
                                              min=min(exports1$dates),
                                              max=max(exports1$dates)),
                               tags$head(
                                 tags$style(
                                   HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"))),
                               withSpinner(
                                 echarts4rOutput('graph1'),
                                 type=1,
                                 color='#fe9000',
                                 hide.ui=FALSE)
                                                     )
                    )
                  })
                  )
                )
        ),
        tabItem(tabName = "t_item2",
                fluidRow(
                  column(12,
                  titlePanel('COMPARISON DATA'),
                  output$page2 <-renderUI({
                    tabsetPanel(
                      tabPanel(title='Data Table',
                               icon=icon('table'),
                               titlePanel('VIEW DIFFERENT CROPS'),
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
                                 tags$style(
                                   HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"))),
                               textOutput('DateRange'),
                               dataTableOutput('table2')),
                      tabPanel(title='Visualise', icon=icon('eye'),
                               titlePanel('GRAPHS'),
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
                                        tags$style(
                                          HTML(".shiny-output-error-validation {color: #ff0000;font-weight: bold;}"))),
                                      tabsetPanel(
                                 tabPanel(title='Table',
                                            textOutput('DateRange2'),
                                            dataTableOutput('table22')),
                                   tabPanel(title='Graphs',
                                            titlePanel('Type of Graph'),
                                            radioButtons(inputId='graph_type',
                                                         label='Choose the Graph',
                                                         choices=c('Line','Bar','Comparative Bar','Pie'),
                                                         selected=NULL),
                                            textOutput('text2'),
                                            withSpinner(
                                             echarts4rOutput('graph2'), 
                                              type=1,
                                              color='#fe9000',
                                              hide.ui=FALSE)
                    ),
                    tabPanel(title='More Visuals',
                             titlePanel('Combined Bar Graph'),
                  tags$div(id="test", style="width:100%;height:400px;"),
                             deliverChart(div_id = "test")
                                 )
                      )
                    )
                    )
                  }))
                )
        ),
        tabItem(tabName = "t_item3",
                output$page3<-renderUI({
                  fluidRow(
                    column(12,
                    titlePanel('TRENDS'),
                           tabsetPanel(
                             tabPanel(title ='Graphs',
                                      icon = icon('chart-pie'),
                                      titlePanel('KENYA DEBT'),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput(inputId = "indicator",
                                                      label = strong("Type of Debt"),
                                                      choices = unique(kenya_status1$indicator),
                                                      selected = "domestic_debt"),
                                          dateRangeInput(inputId ="date",
                                                         label=strong("period"),
                                                         start = "1999-09-01",
                                                         end = "2021-06-01",
                                                         min = "1999-09-01", 
                                                         max = "2021-06-01")
                                          ),
                                        mainPanel(
                                          tags$head(
                                            tags$style(
                                              HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"))),
                                          tags$a(href = "https://www.centralbank.go.ke/public-debt/", "Source: Central Bank of Kenya",
                                                 target = "_blank"),
                                          withSpinner(echarts4rOutput('graph3'),
                                                      type=1,
                                                      color='#fe9000',
                                                      hide.ui=FALSE)
                                         )
                                        )
                                      ),
                    tabPanel(title ='Summary',
                             icon = icon('calculator'),
                             titlePanel('CONSTANTS'),
                    column(6,
                           withSpinner(echarts4rOutput('liquid'),
                                       type=1,
                                       color='#fe9000',
                                       hide.ui=FALSE)),
                    column(6,
                           withSpinner(echarts4rOutput('clock'),
                                       type=1,
                                       color='#fe9000',
                                       hide.ui=FALSE))
                    )
                    )))})),
        tabItem(tabName = 't_item4',
          fluidRow(
            column(12,
            output$page4<- renderUI({
              tabsetPanel(
                tabPanel('import Data',
                         fileInput('file1','Data',buttonLabel='Upload...',accept=c('.cvs','.tsv','.xls','.xlsx')),
                         numericInput('n','No. of Rows to preview',value=10,min=1,step=1),
                         tags$head(
                           tags$style(
                             HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"))
                         ),
                         dataTableOutput('head')
                         ),
                tabPanel('Set parameters'),
                tabPanel('visualise Results'
                ))
            })
            )
          )
        ),
        tabItem(tabName='t_item5',
                fluidRow(
                  column(12,
                  output$page5<- renderUI({
                    
                  })
                  )
                ))
      )
    } else {
      login
    }
  })
}
shinyApp(ui = ui, server = server)


