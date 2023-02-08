library(ECharts2Shiny)
library(waiter)
library(shiny)
library(plotly)
library(ggplot2)
library(shinyvalidate)
library(data.table)
library(purrr)
library(shinyjqui)
library(dplyr)
library(echarts4r)
library(readr)
library(readxl)
library(shiny)
library(shinycssloaders)
library(dashboardthemes)
library(bslib)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFeedback)
library(shinyalert)
library(shinyjs)
library(png)
library(later)
library(DT)
library(rhandsontable)
not_sel <- "Not Selected"
theme1 <- theme(
  axis.line = element_line(colour = 'grey50', linewidth = .75),
  panel.background = element_rect(fill='lightblue'),
  plot.background = element_rect(colour='darkblue'),
  panel.grid.major.y = element_line(colour = "grey50"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x =element_blank(),
  axis.text = element_text(colour = "darkblue"),
  axis.title = element_text(colour = "darkblue")
)
myToastOptions <- list(
  positionClass = "toast-top-right",
  progressBar = FALSE,
  timeOut = 3000,
  closeButton = TRUE,
  
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut"
)

exports<- openxlsx::read.xlsx('exports.xlsx',detectDates = TRUE)
exports1<- openxlsx::read.xlsx('exports1.xlsx',detectDates = TRUE)
kenya_status1<- openxlsx::read.xlsx('kenya_status1.xlsx',detectDates = TRUE)
login_details <- openxlsx::read.xlsx('login_details.xlsx',detectDates = TRUE)
user_results <- readr::read_csv('user_results.xlsx')
source("exports_modal_dialog.R")
source('debt_modal_dialog.R')
dataset<-c('Exports','Kenya Debt','Remittances',"Results")
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}
enableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Candara"
  ,appFontColor = "rgb(0, 0, 0)"
  ,primaryFontColor = "rgb(0, 0, 0)"
  ,infoFontColor = "rgb(8,58,68)"
  ,successFontColor = "rgb(0, 0, 70)"
  ,warningFontColor = "rgb(255,255,0)"
  ,dangerFontColor = "rgb(255,0,0)"
  ,bodyBackColor =cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(255, 240, 255)"
    ,colorMiddle = "rgb(255, 240, 200)"
    ,colorEnd = "rgb(255, 240,170)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  
  ### header
  ,logoBackColor = "rgb(200, 60, 0)"
  
  ,headerButtonBackColor = "rgb(255, 240, 255)"
  ,headerButtonIconColor = "rgb(0,0,100)"
  ,headerButtonBackColorHover ="rgb(50, 50, 50)"
  ,headerButtonIconColorHover = "rgb(0,255,255)"
  
  ,headerBackColor = "rgb(200, 60, 0)"
  ,headerBoxShadowColor ="rgb(100, 100, 100)"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(0, 0, 50)"
    ,colorMiddle = "rgb(0, 0, 80)"
    ,colorEnd = "rgb(0, 0, 120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100 
  ) 
  ,sidebarPadding = 0.5
  
  ,sidebarMenuBackColor = ""
  ,sidebarMenuPadding = 0.5
  ,sidebarMenuBorderRadius = 0.5
  
  ,sidebarShadowRadius = "2px 2px 2px"
  ,sidebarShadowColor = "rgb(100,100,100)"
  
  ,sidebarUserTextColor = "rgb(0,255,255)"
  
  ,sidebarSearchBackColor = "rgb(189,54,121)"
  ,sidebarSearchIconColor = "rgb(102,51,0)"
  ,sidebarSearchBorderColor = "rgb(102,255,255)"
  
  ,sidebarTabTextColor = "rgb(255, 255, 255)"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(0, 255, 255)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected =cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255, 240, 255)"
    ,colorMiddle = "rgb(255, 240, 200)"
    ,colorEnd = "rgb(255, 240,170)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0, 0, 0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255,150, 0)"
    ,colorMiddle = "rgb(225, 125, 0)"
    ,colorEnd = "rgb(200, 100,0)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(0,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(200, 60, 0)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,240,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "#ac001a"
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
  ,buttonBackColor = "rgb(255, 240, 255)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200, 60, 0)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover ="rgb(200, 60, 0)"
  ,buttonTextColorHover = "rgb(0,255,255)"
  ,buttonBorderColorHover = "rgb(118,118,102)"
  
  ,textboxBackColor = ""
  ,textboxBorderColor = "rgb(0,255,255)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = ""
  ,textboxBorderColorSelect = "rgb(0,200, 0)"
  
  ### tables
  ,tableBackColor = "rgb(240, 255, 255)"
  ,tableBorderColor =""
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1  
)
header <- dashboardHeader(dropdownMenuOutput('menu1'),
                          dropdownMenuOutput('menu2'),
                          dropdownMenuOutput('menu3'),
                          userOutput('user'),
                          title = tags$a(tags$img(src='logo.png',width='20%'),
                                         href='','INFINICALS',
                                         style = "color:yellow; 
                                        font-family:Candara;
                                        font-size:24px;
                                        font-weight: bold;")
)

sidebar <-dashboardSidebar(
  tags$h2(
    sidebarUserPanel(name = "Jefferson M. Ndeke",
                     subtitle = "Statistician/Data Analyst",
                     image ='user_image.png'
    ),
    tags$div(
      class ='p7',
      tags$b(
        tags$div(
          class = 'p8',
          actionLink("linkedin", 
                     label ="",
                     icon = icon("linkedin"),
                     onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')")
        )
      ),
      tags$b(
        tags$div(
          class = 'p8',
          actionLink("github",
                     label = "",
                     icon = icon("github"),
                     onclick = "window.open('https://github.com/ndeke254')"))
      ),
      tags$b(
        tags$div(
          class = 'p8',
          actionLink("twitter",
                     label = "",
                     icon = icon("twitter"),
                     onclick = "window.open('https://twitter.com/jefferson_ndeke')"))
      )
    ),
    sidebarMenu(
      menuItem('Dashboard',
               tabName='t_item1',
               icon=icon('tachometer-alt'),
               selected= TRUE
      ),
      menuItem("Database",
               tabName = "t_item2",
               icon = icon("database"),
               menuSubItem('By Crop',
                           tabName = "t_item21",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('By Type',
                           tabName = "t_item22",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('Update',
                           tabName = "t_item23",
                           icon = shiny::icon("angle-double-right")
               )
      ),
      menuItem("Economic Variables",
               tabName = "t_item3",
               icon = icon("chart-line"),
               menuSubItem('Public Debt',
                           tabName = "t_item31",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('Inflation',
                           tabName = "t_item32",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('Exchange Rate',
                           tabName = "t_item33",
                           icon = shiny::icon("angle-double-right")
               )
      ),
      menuItem('Your data',
               tabName='t_item4',
               icon = icon('file-import'),
               menuSubItem('Import',
                           tabName = "t_item41",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('Visualise',
                           tabName = "t_item42",
                           icon = shiny::icon("angle-double-right")
               ),
               menuSubItem('Statistical Summary',
                           tabName = "t_item43",
                           icon = shiny::icon("angle-double-right")
               )
      ),
      menuItem('Settings',
               tabName='t_item5',
               icon = icon('hammer')
      ),
      menuItem('About',
               tabName ='t_item6',
               icon= icon('address-card')
      )
    )
  )
)

body<- dashboardBody(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    ')
  )),
  useShinyFeedback(),
  useShinyjs(),
  tags$head(includeScript('returnClick.js')),customTheme,
  tags$head(tags$link(rel='stylesheet',type='text/css',
                      href='styles.css')
  ),
  tabItems(
    tabItem(tabName = 't_item1',
            fluidRow(
              titlePanel(tags$h3('INFO')),
              box(tags$a(
                tags$img(src='logo.png',width="50%")
              ),
              width=4, solidHeader=TRUE,
              title=tags$h2('OUR MOTTO'),
              footer=tags$h4('Beyond Infinity'),
              background='aqua',
              status='primary'
              ),
              infoBox('DAY OF THE WEEK',
                      weekdays(Sys.Date())%>% toupper()),
              br(),
              valueBox(format(Sys.Date(), format = "%d"),subtitle='DATE'),br(),
              infoBox('MONTH',
                      months(Sys.Date())%>%toupper()),
              br(),
              valueBox(format(Sys.Date(), format = "20%y"),subtitle='YEAR'),
              box(width=4, solidHeader=TRUE,
                  footer=tags$h4(textOutput("state")),
                  background='aqua',
                  status='primary',textOutput('quote'),
                  title=tags$h2('QUOTE OF THE DAY'))
            )            
            
    ),
    tabItem(tabName = "t_item21",
            fluidRow(
              column(12,
                     titlePanel(tags$h3('COMPANY DATA')),
                     tabsetPanel(
                       tabPanel(title='Data Table',
                                icon=icon('table'),
                                titlePanel('Ranked from latest date of record'),br(),
                                sidebarLayout(
                                  sidebarPanel(
                                    tags$style(".well {background-color: #f0ffff;}"),
                                    selectInput(inputId="col_view",                           
                                                label="Select Crops to view",
                                                multiple = TRUE,
                                                selected= unique(exports$dates),
                                                choices = unique(colnames(exports))
                                    ),
                                    dateRangeInput('datum',strong('Period'),
                                                   start= min(exports $dates),
                                                   end=max(exports $dates),
                                                   min=min(exports$dates),
                                                   max=max(exports$dates)
                                    )
                                  ),
                                  mainPanel(
                                    tags$head(tags$style("table { background-color: #ffff97; }", media="screen", type="text/css")),
                                    
                                    tags$head(
                                      tags$style(
                                        HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"
                                        ))),
                                    textOutput('DateRange'),
                                    DT::DTOutput('table2'
                                    )
                                  )
                                )
                       ),
                       tabPanel(title='Visualise',
                                icon=icon('eye'),
                                titlePanel('Graphs'),
                                sidebarLayout(
                                  sidebarPanel(
                                    pickerInput(
                                      inputId = 'select_crop',
                                      label = strong('Select Crop'),
                                      multiple = TRUE,
                                      options=list(`max-options`=3),
                                      selected ='',   
                                      choices =unique(exports1$crop)
                                    ),
                                    dateRangeInput(inputId = 'date2',
                                                   label = strong('Period'),
                                                   start= min(exports1$dates),
                                                   end=max(exports1$dates),
                                                   min=min(exports1$dates),
                                                   max=max(exports1$dates)
                                    )
                                  ),
                                  mainPanel(
                                    tags$head(
                                      tags$style(
                                        HTML(".shiny-output-error-validation {color: #ff0000;font-weight: bold;}"
                                        ))),
                                    tabsetPanel(
                                      tabPanel(
                                        title='Table', 
                                        br(),
                                        textOutput('DateRange2'),
                                        DT::DTOutput('table22')
                                      ),
                                      tabPanel(
                                        title='Graphs',
                                        titlePanel('Type of Graph'),
                                        radioButtons(inputId='graph_type',
                                                     label='Choose the Graph',
                                                     choices= c('Line','Normal Bar','Comparative Bar','Cumulative Bar','Pie'),
                                                     inline = TRUE , 
                                                     selected=NULL),
                                        conditionalPanel(
                                          condition= "input.graph_type=='Pie'",
                                          dateInput(inputId = 'date5',
                                                    label = strong('Pie Date'),
                                                    min=min(exports1$dates),
                                                    max=max(exports1$dates),
                                                    value ='2000-12-01'
                                          )
                                        ),
                                        textOutput('text2'),
                                        conditionalPanel(
                                          condition ="input.graph_type!=
                                          'Cumulative Bar'",
                                          withSpinner(
                                            echarts4rOutput('graph2'), 
                                            type=1,
                                            color="#b33e48",
                                            hide.ui=FALSE)
                                        ),
                                        conditionalPanel(
                                          condition= "input.graph_type=='Pie'",
                                          column(12,
                                                 tags$div(id="test1", 
                                                          style="width:100%;height:300px;"), 
                                                 deliverChart(div_id = "test1")
                                          )
                                        ),
                                        conditionalPanel(
                                          condition= "input.graph_type=='Cumulative Bar'",
                                          fluidRow(
                                            column(12,
                                                   tags$div(id="test", 
                                                            style="width:100%;height:400px;"),
                                                   deliverChart(div_id = "test")
                                            ))
                                        )
                                      )
                                    )
                                  )
                                )
                       ),
                       tabPanel(
                         title='Visuals page',
                         icon=icon('eye'),
                         titlePanel('Different Graphs'),
                         fluidRow(
                           column(12,
                                  box(
                                    solidHeader = FALSE,
                                    title = "Control",
                                    status = "primary",
                                    pickerInput(
                                      inputId = 'select_crop1',
                                      label = strong('Select Crop'),
                                      multiple = TRUE,
                                      options=list(`max-options`=3),
                                      selected ='',                  
                                      choices =unique(exports1$crop)
                                    ),
                                    dateRangeInput(inputId = 'date21',
                                                   label = strong('Period'),
                                                   start= min(exports1$dates),
                                                   end=max(exports1$dates),
                                                   min=min(exports1$dates),
                                                   max=max(exports1$dates)
                                    ),
                                    dateInput(inputId = 'date51',
                                              label = strong(' % Pie Date'),
                                              min=min(exports1$dates),
                                              max=max(exports1$dates),
                                              value ='2000-12-01'
                                    )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6, 
                                  withSpinner(
                                    echarts4rOutput('grapha'), 
                                    type=1,
                                    color="#b33e48",
                                    hide.ui=FALSE)
                           ),
                           column(width = 6, 
                                  withSpinner(
                                    echarts4rOutput('graphb'), 
                                    type=1,
                                    color="#b33e48",
                                    hide.ui=FALSE)
                           )              
                         ),
                         fluidRow(
                           column(width = 6, 
                                  withSpinner(
                                    echarts4rOutput('graphc'), 
                                    type=1,
                                    color="#b33e48",
                                    hide.ui=FALSE)
                           ),
                           column(width = 6, 
                                  tags$div(id="test11", 
                                           style="width:100%;height:300px;"),
                                  deliverChart(div_id = "test11")
                           )
                         ),
                         fluidRow(
                           column(width = 6, 
                                  withSpinner(
                                    echarts4rOutput('graphd'), 
                                    type=1,
                                    color="#b33e48",
                                    hide.ui=FALSE)
                           ),
                           column(width = 6, 
                                  tags$div(id="test12", 
                                           style="width:100%;height:400px;"),
                                  deliverChart(div_id = "test12"))
                         )
                       )
                     )
              )
            )
    ),
    tabItem(tabName ='t_item22',
            fluidRow(
              column(12,
                     titlePanel(tags$h3('COMPANY DATA')),
                     tabsetPanel(
                       tabPanel(title='data Table',
                                icon=icon('table'),
                                titlePanel('View by type of Crop'),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId="choose_category",
                                                label= "TYPE OF EXPORTS",                   
                                                multiple = FALSE,
                                                choices = unique(exports1$type),
                                                selected= unique(exports1$type)[1]
                                    ),
                                    selectInput(inputId="choose_item",
                                                label="Select Crop",
                                                multiple= FALSE,
                                                selecte='',                                            
                                                choices =c(not_sel) 
                                    ),  
                                    dateRangeInput(inputId = 'date3',                                                   
                                                   label = strong('Period'),                                          
                                                   start= min(exports1$dates),
                                                   end=max(exports1$dates), 
                                                   min=min(exports1$dates),
                                                   max=max(exports1$dates)
                                    ),
                                    tags$head(
                                      tags$style(
                                        HTML(".shiny-output-error-validation {color: #ff0000;font-weight: bold;}")))
                                  ),
                                  
                                  mainPanel(
                                    textOutput('text1'),
                                    DT::DTOutput('tb_chosen')
                                  )
                                )
                       ),
                       tabPanel(title='Graph',
                                icon=icon('chart-area'),
                                titlePanel('A Line graph'),
                                br(),
                                box(solidHeader = FALSE,
                                    title = "Period Range",
                                    status = "success",
                                    dateRangeInput(inputId = 'date4',
                                                   label = strong('Enter the Period'),
                                                   start= min(exports1$dates),
                                                   end=max(exports1$dates),
                                                   min=min(exports1$dates),
                                                   max=max(exports1$dates)
                                    )
                                ),
                                br(), br(),br(), br(),br(),
                                br(),br(),br(),
                                box(solidHeader = FALSE,
                                    title = "LINE GRAPH",
                                    status = "success",
                                    width=12,
                                    tags$head(
                                      tags$style(
                                        HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}"))),
                                    withSpinner(
                                      echarts4rOutput('graph1'),
                                      type=1,
                                      color="#b33e48",
                                      hide.ui=FALSE)
                                )
                       )
                     )
              )
            )
    ),
    tabItem(tabName ='t_item23',
            titlePanel(tags$h3('COMPANY DATA')),
            tabsetPanel(id='tabs',
                        tabPanel(
                          title ='View Available Data',
                          icon=icon('eye'), 
                          tags$h2(paste(
                            'The available data as at',Sys.time(),'is:',sep=' ')),
                          tags$h2('1. Debt in Kenya'),tags$h2('2. Exports'),
                          tags$h2('3. inflation'),
                          tags$h2('Please note that the available data remains under review to expand the Database'),tags$h2('Any changes in this Tab effectively affects the whole application')
                        ),
                        tabPanel(title='Authenticate',
                                 icon=icon('key'),    titlePanel('Administrator'),                       actionButton('go','OPEN DATABASE'),
                                 br(),br(),
                                 textOutput("base")
                        ),
                        tabPanel(title ='data',icon=icon('table'),                                        
                                 titlePanel('Add/Edit the records'),                             
                                 selectInput(inputId='dataset',
                                             label= "Select the Dataset to Update",
                                             choices = dataset),
                                 div(
                                   class = "container",
                                   div(
                                     shiny::actionButton(
                                       inputId = "add_export",     
                                       label = "Add Export",
                                       icon = shiny::icon("plus"),
                                       class = "btn-success"
                                     ),
                                     shiny::actionButton(
                                       inputId = "update",
                                       label = "Update",
                                       icon = shiny::icon("database"),
                                       class = "btn-success"
                                     )
                                   )
                                 ),
                                 br(),
                                 DT::DTOutput(outputId = "dt_table"),
                                 conditionalPanel(
                                   condition="input.dataset=='Results'",
                                   shinyjs::hidden(tags$div(
                                     id="cols_name",
                                     selectInput(inputId="choose_cols",
                                                 label="Select Column",
                                                 multiple= FALSE,
                                                 selected='', 
                                                 choices =unique(
                                                   colnames(user_results))
                                     ), 
                                     textInput("new_col_name",
                                               tags$h2("New Col name:"),
                                               placeholder="Column"),
                                     actionButton('change',"Save",
                                                  icon=icon("brush")))
                                   ),
                                   withSpinner(
                                     rHandsontableOutput(
                                       'results_table'),
                                     type=1,color="#b33e48")),
                                 shiny::includeScript("script.js"),
                        ),
                        tabPanel(title='User Results',
                                 icon=icon('folder-open'),  
                                 titlePanel('Results')
                        )
            )
    ),
    tabItem(tabName = "t_item31",
            fluidRow(
              column(12,
                     titlePanel(tags$h3('TREND ANALYSIS')
                     ),
                     tabsetPanel(
                       tabPanel(
                         title ='Public Debt',
                         icon = icon('coins'),
                         titlePanel('Kenya Debt'),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "indicator1",
                                         label = strong("Type of Debt"),
                                         choices = unique(kenya_status1$indicator),
                                         selected = "domestic_debt"),
                             dateRangeInput(inputId ="date",
                                            label=strong("period"),
                                            start =min(kenya_status1$date),
                                            end = max(kenya_status1$date),
                                            min = min(kenya_status1$date), 
                                            max = max(kenya_status1$date)
                             ),
                             checkboxInput(inputId='comp',
                                           label='Compare the Trends')
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel('Line Graphs',
                                        icon=icon('chart-line'),
                                        titlePanel('Growth of Debt against time'),
                                        tags$head( tags$style(
                                          HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}")
                                        )
                                        ),
                                        tags$a(href = "https://www.centralbank.go.ke/public-debt/", "Source: Central Bank of Kenya",
                                               target = "_blank"),
                                        withSpinner(echarts4rOutput('graph3'),
                                                    type=1,
                                                    color="#b33e48",   
                                                    hide.ui=FALSE)                       ),
                               tabPanel(
                                 title ='Summary',
                                 icon = icon('calculator'),          
                                 titlePanel('Constants'),
                                 column(6,
                                        withSpinner(echarts4rOutput('liquid'),
                                                    type=1,
                                                    color="#b33e48",
                                                    hide.ui=FALSE)
                                 ),
                                 column(6,
                                        withSpinner(echarts4rOutput('clock'),
                                                    type=1,     
                                                    color="#b33e48",
                                                    hide.ui=FALSE)
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
              )
            )
    ),
    tabItem(tabName ='t_item32',
            titlePanel('Purchasing Power')
    ),
    tabItem(tabName ='t_item33',
            titlePanel('Shilling Strength')
    ),
    tabItem(tabName = 't_item41',
            fluidRow(
              column(12,
                     titlePanel(tags$h3('PREPARE YOUR DATA')),
                     tabsetPanel(
                       tabPanel('Import Data',
                                icon=icon('upload'),
                                titlePanel('Upload'),
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput('file1','data',buttonLabel='Upload...',accept=c('.cvs','.tsv','.xls','.xlsx')
                                    ),
                                    numericInput('n','No. of Rows to preview',value=10,min=1,max=10,step=1),
                                    radioButtons("disp", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head"),
                                    actionButton("preview", "Preview")
                                  ),
                                  mainPanel(
                                    tags$head(
                                      tags$style(
                                        HTML(".shiny-output-error-validation{color: #ff0000;font-weight: bold;}")
                                      )
                                    ), 
                                    conditionalPanel(
                                      condition = "input.preview",
                                      div(
                                        class = "text-left",
                                        div(
                                          style = "display: inline-block;", 
                                          selectInput(inputId='typeof',                                                      label= "Numeric Columns:", choices = c(not_sel)
                                          ),
                                          textOutput("typeof1"),
                                          tags$h2("Click cell to edit"),
                                          tags$h2("Left click to edit rows,columns
                                                and download a csv file")
                                        )
                                      )
                                    ),br(),
                                    withSpinner(rHandsontableOutput('hot'),type=1,color="#b33e48")
                                  )
                                )
                       )
                     )
              )
            )
    ),
    tabItem(tabName= 't_item42',
            fluidRow(
              column(12,
                     titlePanel(tags$h3('PLOT YOUR DATA')),
                     tabsetPanel(
                       tabPanel(
                         title ='Data Plots',
                         icon = icon('cloud'),
                         titlePanel('Control your Plot'),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId='axis1',
                                         label= "X-Axis",
                                         choices = c(not_sel)
                             ),
                             selectInput(inputId='axis2',
                                         label= "Y-Axis",
                                         choices = c(not_sel)
                             ),
                             selectInput(inputId='group',
                                         label= "Group by",
                                         choices = c(not_sel)
                             ),
                             tags$h2('NB:FOR REGRESSION MODEL'),
                             checkboxInput(inputId='tick',
                                           label='Line of Best fit'),
                             checkboxInput("se", 
                                           "Add confidence interval around the regression line",FALSE),
                             br(),
                             actionButton("run_button", "Run Analysis", 
                                          icon = icon("play")
                             )
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel('Regular Plot',
                                        icon = icon('expeditedssl'),
                                        tags$h2("Choose only numeric data for axis!"),
                                        br(),
                                        withSpinner(
                                          plotlyOutput('graph4'), 
                                          type=1,
                                          color="#b33e48",
                                          hide.ui=FALSE)),
                               tabPanel('Regression Model',
                                        icon=icon('layer-group'),
                                        titlePanel('Plot'),
                                        withMathJax(),
                                        tags$b("Regression plot:"),
                                        uiOutput("results"),
                                        numericInput('predict',
                                                     'Predict',
                                                     value=0), 
                                        verbatimTextOutput("value1"),
                                        br(),
                                        withSpinner(
                                          plotlyOutput('graph5'),
                                          type=1,
                                          color="#b33e48",
                                          hide.ui=FALSE),
                                        tags$b("Interpretation:"),
                                        uiOutput("interpretation")
                                        
                               )
                             )
                             
                           )
                         )))
              )
            )
    ),
    tabItem(tabName = 't_item43',
            titlePanel(tags$h3('STATISTICS')),
            tags$h2('Summary Tables'),
            fluidRow(
              column(width = 4, strong(textOutput("num_var_1_title"))
              ),
              column(width = 4, strong(textOutput("num_var_2_title"))
              ),
              column(width = 4, strong(textOutput("fact_var_title"))
              )
            ),
            fluidRow(
              column(width = 4, tableOutput("num_var_1_summary_table")),
              column(width = 4, tableOutput("num_var_2_summary_table")),
              column(width = 4, tableOutput("fact_var_summary_table"))
            ),
            fluidRow(
              column(width = 6, strong("Combined Statistics")),
              column(width = 6, strong("Regression Summary"))
            ),
            fluidRow(
              column(width = 6, tableOutput("combined_summary_table")),
              column(width = 6, tableOutput("regression_summary_table"))
            )
    ),
    tabItem(tabName='t_item5',
            fluidRow(
              column(12
              )
              
            )
    ), 
    tabItem(tabName='t_item6',
            titlePanel(tags$h3('ABOUT AUTHOR')),
            fluidRow(
              column(12, br(),
                     img(src ="user_image.png", 
                         width = "17%", 
                         style = "display: block; margin-left: auto; margin-right: auto;")
              ),
              fluidRow(
                tags$h2("JEFFERSON NDEKE", 
                        style = "text-align: center"),
                tags$h5("musumbijefferson@gmail.com", 
                        style = "text-align: center")
              ),
              hr(),
              fluidRow(column(5, ""),
                       column(
                         3,
                         tags$h3(
                           HTML('&nbsp;'),
                           HTML('&nbsp;'),
                           HTML('&nbsp;'),
                           tags$a(
                             href = 'https://www.linkedin.com/in/jefferson-ndeke-027062202/',
                             img(
                               src = 'LinkedIn.png',
                               height = "50px"
                             )
                           ),
                           HTML('&nbsp;'),
                           tags$a(
                             href = 'https://twitter.com/jefferson_ndeke',
                             img(
                               src = 'twitter.png',
                               height = "50px"
                             )
                           ),
                           HTML('&nbsp;'),
                           tags$a(href = 'https://github.com/ndeke254', img(
                             src = 'github.jpg',
                             height = "50px"
                           ))
                         )
                       )),
              fluidRow(
                column(2, ""),
                column(
                  1,
                  h3(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
                  br(),
                  br(),
                  h3(icon("globe"), style = "text-align: right; line-height: 200%"),
                  br(),
                  h3(icon("heart"), style = "text-align: right; line-height: 170%;")
                ),
                column(
                  6,
                  h6(
                    "Currently a finalist college student at the University of Nairobi pursuing a Bachelors degree in Economics and Statistics. Open to any remote/online job or assignment.",
                    style = "text-align: left; line-height: 150%;"
                  ),
                  br(),
                  h6(
                    "Data communicates correctly when handled the right way; From collection to presentation.
              Coupling with academic training, data science remains a powerful tool for me in the current dynamic world to analyse and predict trends.
              ",
                    style = "text-align: left; line-height: 150%;"
                  ),
                  br(),
                  h6(
                    "Passionate about public speaking and presentations. A believer of an opportunity to prove. A lover of God and humanity. Travelling. Hiking. Reading philosophical publications. ",
                    style = "text-align: left; line-height: 150%;"
                  )
                )
              )
            )
    )
  )
)
ui <-tabsetPanel(
  id = "panels",
  type = "hidden",
  selected = "landing_page",
  tabPanelBody(value = "landing_page",
               tags$div(
                 class = "landing_page_container", 
                 tags$div(
                   class = "landing_page_header",
                   tags$div(
                     class = "landing_page_logo",
                     tags$a(
                       tags$img(src='logo.png',width="50%")
                     ),
                     style= "float:left;"
                   ),
                   tags$div(
                     class = "select_something", 
                     actionLink('portal','USER PORTAL',
                                icon = icon("users")
                     )
                   )
                 ),
                 tags$div(
                   class = "landing_page_body", 
                   tags$div(
                     class = "landing_page_content",
                     tags$div(
                       class = "text_content", 
                       tags$div(
                         class ="some_text",
                         tags$h1(
                           tags$strong(
                             "INFINICALS", 
                             style = "font-size: 200%")
                         )
                       ), 
                       tags$div(
                         class ="more_text", 
                         tags$h2(
                           "Beyond Infinity",
                           style =
                             "color:#FFFFFF;")
                       )
                     )
                   )
                 )
               )
  ),
  tabPanelBody(
    value = "login_page",
    tags$div(id = "div_login",
             class= 'login',
             box(align='left', 
                        title = tags$a("SIGN IN TO INFINICALS",
                        style = "font-size:20px; 
                        font-weight: bold;"),
                        status="warning",
                        solidHeader= TRUE,
                        textInput(
                          "userName",
                          label = div(icon("user-plus",
                                           style = "color:#c83c00;"),
                                      'Username')
                          ),
                        passwordInput(
                          'passwd',
                          label = div(icon("key",
                                           style ="color:#c83c00;"),
                                      "Password")
                          ),
                        actionButton("Login", "Log in"),
                        footer= div(class= 'pull-right-container',
                                    tagList(
                                      actionLink("reset",
                                                        label = "Forgot Password"),
                                             br(),
                                             actionLink("new_user",
                                                        label = "New User")
                                         )
                             )
                         
               
             )
    )
  ),
  tabPanelBody(
    value = "dashboard",
    tags$div(id = "div_dashboard",
             dashboardPage(         
               header = header, 
               sidebar = sidebar, 
               body = body,
               options = list(sidebarExpandOnHover = TRUE)
             )
    )
  )
)
register<- modalDialog(
  fluidRow(
    column(12,align="center",
           tags$div(id="part1",
                    tags$a(tags$img(src='logo.png',width='10%',
                                    tags$h2("INFINICALS"))),
                    textInput("reset_name",
                              tags$h2("Enter your username:"),
                              placeholder="My Username"),
                    textInput("reset_email",
                              tags$h2("Enter your email:"),
                              placeholder="My Email"),
                    actionButton('search',"Next",icon=icon("forward"))),
           shinyjs::hidden(tags$div(id="part2",
                                    tags$a(tags$img(src='logo.png',width='10%',
                                                    tags$h2("INFINICALS"))),
                                    passwordInput("new_pass1",
                                                  tags$h2("Enter new Password:"),
                                                  placeholder="My Password"),
                                    passwordInput("new_pass2",
                                                  tags$h2("Repeat the Password:"),
                                                  placeholder="Repeated Password"),
                                    loadingButton('search2','Finish', 
                                                  class   = "btn-info",
                                                  loadingLabel = "Reseting..."))
           ),textOutput('found'))),
  title=tags$div(
    style="margin: 0; text-align: center;",
    tags$h3('RESET YOUR PASSWORD',
            style="align-text: center;")),
  footer=NULL,
  size = 'l',
  easyClose = TRUE
)
register2<- modalDialog(
  fluidRow(
    column(12,align="center",
           tags$div(id="part1b",
                    tags$a(tags$img(src='logo.png',width='10%',
                                    tags$h2("INFINICALS"))),
                    textInput("your_name",
                              tags$h2("Enter your username:"),
                              placeholder="My Username"),
                    textInput("your_email",
                              tags$h2("Enter your email:"),
                              placeholder="My Email"),
                    passwordInput("my_pass1",
                                  tags$h2("Enter Password:"),
                                  placeholder="My Password"),
                    passwordInput("my_pass2",
                                  tags$h2("Repeat Password:"),
                                  placeholder="Repeated Password"),
                    loadingButton('user_save','Save', 
                                  class   = "btn-info",
                                  loadingLabel = "Saving..."),
                    textOutput('found1')
           )
    )
  ),
  title=tags$div(
    style="margin: 0; text-align: center;",
    tags$h3('ADD NEW USER',
            style="align-text: center;")),
  footer=NULL,
  size = 'l',
  easyClose = TRUE
)
draw_plot<- function(upload, axis1, axis2, group){
  if(group!=not_sel){
    upload[,(group):= as.factor(upload[,get(group)])]
  }
  if(axis1 == not_sel & axis2 == not_sel & group== not_sel){
    return(NULL)
  }
  else if(axis1 != not_sel & axis2 != not_sel & group!= not_sel){
    ggplot(data = upload,
           aes_string(x = axis1, y = axis2, color = group)) +
      geom_point()+ theme1
  }
  else if(axis1 != not_sel & axis2 != not_sel & group == not_sel){
    ggplot(data = upload,
           aes_string(x = axis1, y = axis2)) +
      geom_point()+theme1
  }
  else if(axis1!= not_sel & axis2 == not_sel & group != not_sel){
    ggplot(data = upload,
           aes_string(x = group, y = axis1)) +
      geom_violin()+theme1
  }
  else if(axis1 == not_sel & axis2 != not_sel & group != not_sel){
    ggplot(data = upload,
           aes_string(x = group, y = axis2)) +
      geom_violin()+theme1
  }
  else if(axis1!= not_sel & axis2 == not_sel & group == not_sel){
    ggplot(data = upload,
           aes_string(x = axis1)) +
      geom_histogram()+theme1
  }
  else if(axis1 == not_sel & axis2 != not_sel & group== not_sel){
    ggplot(data = upload,
           aes_string(x = axis2)) +
      geom_histogram()+theme1
  }
  else if(axis1 == not_sel & axis2 == not_sel & group != not_sel){
    ggplot(data = upload,
           aes_string(x = group)) +
      geom_bar()+theme1
  }
}
create_num_var_table <- function(upload, num_var){
  if(num_var != not_sel){
    col <- upload[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value)
    data.table(statistic, value)
  }else{
    return()
  }
}
create_fact_var_table <- function(upload, group){
  if(group != not_sel){
    freq_tbl <- upload[,.N, by = get(group)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }else{
    return()
  }
}
create_combined_table <- function(upload, axis1, axis2, group){
  if(group != not_sel){
    if(axis1 != not_sel & axis2 != not_sel){
      res_tbl <- upload[,.(correlation = cor(get(axis1), get(axis2))), by = group]
    }
    else if(axis1 != not_sel & axis2 == not_sel){
      res_tbl <- upload[,.(mean = mean(get(axis1))), by = group]
    }
    else if(axis1 == not_sel & axis2 != not_sel){
      res_tbl <- upload[,.(mean = mean(get(axis2))), by = group]
    }
    else if(axis1 == not_sel & axis2 == not_sel){
      res_tbl <- return()
    }
  }
  else if(axis1 != not_sel & axis2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        upload[,get(axis1)],
        upload[,get(axis2)])))
  }
  else{return()}
  return(res_tbl)
}
create_regression_table <- function(upload, axis1, axis2){
  if(axis1 != not_sel & axis2 != not_sel){
    x<-  upload[,get(axis1)]
    y <- upload[,get(axis2)]
    fit <- lm(y~x)
    res_tbl <- data.table(
      parameter =c("Formular","Intercept",paste(axis2, "Gradient"),
                   'Adj. R-Squared','P-Value'),
      value = c(paste(axis2 ,'~', axis1),
                round(fit$coef[[1]], 3),
                round(fit$coef[[2]], 3),
                round(summary(fit)$adj.r.squared, 3),
                signif(summary(fit)$coef[2, 4], 3)
      ))
    return(res_tbl)
  } else{
    return()
  }
}
regression_statement <- function(upload, axis1 ,axis2){
  if(axis1 !=not_sel & axis2 !=not_sel){
    x <- upload [,get(axis1)]
    y <- upload [,get(axis2)]
    fit<- lm(y ~ x) 
    withMathJax(
      br(),
      paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
    )
  } else {
    return()
  }
}

create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                     ))
}
x <- create_btns(1:nrow(exports1))
y <- create_btns(1:nrow(kenya_status1))
mtcars <- exports1%>%
  dplyr::bind_cols(tibble("Buttons" = x))
mtcars1<-kenya_status1%>%
  dplyr::bind_cols(tibble("Buttons" = y))

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("dates", sv_required())
  iv$add_rule("datex", sv_required())
  iv$add_rule("weight", sv_required())
  iv$add_rule("value", sv_required())
  iv$add_rule("n", sv_required())
  iv$add_rule("your_name", sv_required())
  iv$add_rule("your_email", sv_required())
  iv$add_rule("my_pass1", sv_required())
  iv$add_rule("my_pass2", sv_required())
  iv$add_rule("new_pass1", sv_required())
  iv$add_rule("new_pass2", sv_required())
  iv$add_rule("reset_email", sv_required())
  iv$add_rule("reset_name", sv_required())
  
  output$state<- renderText({
    time <- substr(Sys.time(),12,16)
    if(time >"00:01" & time < "12:00"){
      salutation <- 'Good Morning'} else if(time >"12:00" & time <"16:00"){
        salutation <- 'Good Afternoon'} else if (time >"16:00" & time<"24:59"){ salutation <-"Good Evening"}else {
          salutation <-NULL}
    salutation1 <-  paste(salutation,input$userName,sep=" ")
    salutation1
  })
  
  output$quote<- renderText({
    a <- 'This is a new dawn for a new season. Wake up and work.'
    b<- 'Walk with confidence for you alone understand yourself.'
    c<-'Prove yourself right that you can win indeed.'
    d <-"We live in a world where goodness isn't often celebrated. Don't mind it."
    e <- 'Poverty is the worst kind of slavery. Resist it.'
    f<-"If you have to fail, do so with decorum."
    group <- c(a,b,c,d,e,f)
    quote<- sample(group,1)
    quote
    
  })
  results <- shiny::reactiveFileReader(1000,session,'user_results.xlsx',readFunc = function(filePath){ 
    openxlsx::read.xlsx(filePath,detectDates = TRUE)      })
  details <- shiny::reactiveFileReader(1000,session,'login_details.xlsx',readFunc = function(filePath){ 
    openxlsx::read.xlsx(filePath,detectDates = TRUE)
  })
  debt <- shiny::reactiveFileReader(1000,session,'kenya_status1.xlsx',readFunc = function(filePath){ 
    openxlsx::read.xlsx(filePath,detectDates = TRUE)
  })
  expos <- shiny::reactiveFileReader(1000,session, filePath = 'exports1.xlsx',readFunc = function(filePath){ 
    openxlsx::read.xlsx(filePath,detectDates = TRUE)
  })
  expot <- shiny::reactiveFileReader(1000,session,'exports.xlsx',readFunc = function(filePath){ 
    openxlsx::read.xlsx(filePath,detectDates = TRUE)
  })
  
  observe({
    updateDateRangeInput(session, "date",
                         start = min(debt()$date),
                         end = max(debt()$date),
                         min = min(debt()$date),
                         max = max(debt()$date)
    )
  })
  observe({
    updateDateRangeInput(session, "date2",
                         start = min(expos()$dates),
                         end = max(expos()$dates),
                         min = min(expos()$dates),
                         max = max(expos()$dates)
    )
  })
  observe({
    updateDateRangeInput(session, "date3",
                         start = min(expos()$dates),
                         end = max(expos()$dates),
                         min = min(expos()$dates),
                         max = max(expos()$dates)
    )
  })
  observe({
    updateDateRangeInput(session, "date4",
                         start = min(expos()$dates),
                         end = max(expos()$dates),
                         min = min(expos()$dates),
                         max = max(expos()$dates)
    )
  })
  observe({
    updateDateRangeInput(session, "datum",
                         start = min(expot()$dates),
                         end = max(expot()$dates),
                         min = min(expot()$dates),
                         max = max(expot()$dates)
    )
  })
  

  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if (nrow(details()[details()$user == Username & 
                             details()$pswd == Password,]) >= 1)                        {
            USER$Logged <- TRUE
          }
        }
      }
    }
  })
  
  
  # hide dashboard by default:
  jqui_hide(
    ui = "#div_dashboard", 
    effect = "blind"
  )
  #hide login_page by default
  jqui_hide(
    ui = "#div_login", 
    effect = "blind"
  )
  observeEvent(input$portal,{
    updateTabsetPanel(
      session = session, 
      inputId = "panels", 
      selected = "login_page"
    )
    #show login contents
    jqui_show(
      ui = "#div_login", 
      effect = "blind", 
      duration = 500
    )
  })
  observeEvent(input$Login, { 
    if(USER$Logged!=TRUE){
      shinyFeedback::showFeedbackDanger(inputId = 'passwd',
                                        text = 'INVALID DETAILS!')
      shinyFeedback::showFeedbackDanger(inputId = 'userName',text = '')
    } else {
      hideFeedback('passwd')
      hideFeedback('userName')
      shinyalert('PLEASE WAIT', "Signing in...", 
                 type = "info", 
                 timer = 800,
                 showConfirmButton = FALSE,
                 animation = "pop"
      )
      updateTabsetPanel(
        session = session, 
        inputId = "panels", 
        selected = "dashboard"
      )
      jqui_show(
        ui = "#div_dashboard", 
        effect = "blind", 
        duration =500
      )
      showNotification(ui=paste('Dear', input$userName,",Welcome to Infinicals",sep = ' '),
                       type='message',
                       closeButton=FALSE,
                       duration = 3
      )
    }
  })
  
  output$menu1 <- renderMenu({
    dropdownMenu(type ="notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("users"), status = "info",
                                  "5 new members joined today!"),
                 notificationItem(icon = icon("broom"), status = "danger",
                                  "scan your device for virus!"),
                 notificationItem(icon = icon("gamepad"), status = "success",
                                  "You are in level 5!")
    )
  })
  output$menu2<- renderMenu({
    dropdownMenu(type='messages',badgeStatus='info',
                 messageItem(from='@JoeAgeyo',
                             message='You have been appointed the Director!',
                             icon=icon('signature'),
                             time='Yesterday'),
                 messageItem(from='@AgnesKavindu',
                             message='Your Business Proposal has been approved',
                             icon=icon('briefcase'),
                             time='just now'),
                 messageItem(from='@Dad',
                             message='Hi,Could you pass by Naivas and have some cereals for me, thanks',
                             icon=icon('user-circle'),
                             time='2022-02-27 17:09')
                 
    )
  })
  output$menu3 <- renderMenu({
    dropdownMenu(type='tasks',badgeStatus='success',
                 taskItem('XET 301 Assignment',
                          value=70,
                          color='teal'),
                 taskItem('STA 222 Assignment',
                          value=45,
                          color='maroon'),
                 taskItem('STA 402 Assignment',
                          value=94,
                          color='lime'),
                 taskItem('XEA 102 Assignment',
                          value=12,
                          color='green')
    )
  })
  output$user <-renderUser({
    dashboardUser(
      name=input$userName,
      image='user_image.png',
      footer=p('Beyond Infinity'),
      subtitle = a(icon("user"),"LOG OUT", href='#panels')
    )
  })
  output$text1<-renderText({
    req(input$date3)
    validate(need(!is.na(input$date3[1]) & !is.na(input$date3[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date3[1] < input$date3[2], "Error: Start date should be earlier than end date."))
  })
  
  shiny::observeEvent(input$choose_category, {
    x<- c(not_sel,unique(expos() [
      expos()$type%in%input$choose_category,"crop"])
    )
    shiny::updateSelectInput(session,
                             'choose_item',
                             label=paste('Select',
                                         input$choose_category,sep = ' '),
                             choices=x,
                             selected= not_sel
    )
  })
  output$tb_chosen <- DT::renderDT (
    expos()%>% 
      filter(
        expos()$type%in%input$choose_category &
          expos()$crop%in%input$choose_item & dates > as.POSIXct(input$date3[1]) & dates < as.POSIXct(input$date3[2]))
  )
  datae2<-reactive({
    req(input$choose_item)
    req(input$date4)
    validate(need(!is.na(input$date4[1]) & !is.na(input$date4[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date4[1] < input$date4[2], "Error: Start date should be earlier than end date."))
    expos() %>%filter(
      expos() $type%in%input$choose_category &
        expos()$crop%in%input$choose_item & dates > as.POSIXct(input$date4[1]) & dates < as.POSIXct(input$date4[2]))
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
  output$table2<- DT::renderDT(
    expot() %>% select(dates,input$col_view)%>% filter(
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
  output$table22<- DT::renderDT(
    expos() %>% select(dates,crop,weight)%>% filter(expos()$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])))
  datae3<-reactive({
    validate(need(!is.na(input$select_crop), "You have not selected any crop to view"))
    expos() %>% select(dates,crop,weight)%>% filter(expos()$crop%in% input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2]))
  })
  
  data <- reactive({
    validate(need(!is.na(input$select_crop), "You have not selected any crop to view"))
    validate(need(!is.na(input$date5), "Pie Date required*"))
    total <- expos()%>% filter(expos()$crop%in%input$select_crop,expos()$dates%in%input$date5) %>% select(weight)%>% sum()
    weight1<- expos()%>%filter(expos()$crop%in%input$select_crop[1],expos()$dates%in%input$date5)%>%select(weight)%>%sum()
    weight2<- expos()%>%filter(expos()$crop%in%input$select_crop[2],expos()$dates%in%input$date5)%>%select(weight)%>%sum()
    weight3<-expos()%>%filter(expos()$crop%in%input$select_crop[3],expos()$dates%in%input$date5)%>%select(weight)%>%sum()
    percent1 <- round((weight1/total)*100,0)
    percent2 <-round((weight2/total)*100,0)
    percent3 <-round((weight3/total)*100,0)
    data.frame(dates=c(input$date5,input$date5,input$date5),
               crop= c(input$select_crop[1],input$select_crop[2],input$select_crop[3]),
               weight= c(percent1,percent2,percent3) )
    
  }) 
  datay <- reactive({
    validate(need(!is.na(input$select_crop1), "You have not selected any crop to view"))
    validate(need(!is.na(input$date51), "Pie Date required*"))
    total <- expos()%>% filter(expos()$crop%in%input$select_crop1,expos()$dates%in%input$date51) %>% select(weight)%>% sum()
    weight1<- expos()%>%filter(expos()$crop%in%input$select_crop1[1],expos()$dates%in%input$date51)%>%select(weight)%>%sum()
    weight2<- expos()%>%filter(expos()$crop%in%input$select_crop1[2],expos()$dates%in%input$date51)%>%select(weight)%>%sum()
    weight3<-expos()%>%filter(expos()$crop%in%input$select_crop1[3],expos()$dates%in%input$date51)%>%select(weight)%>%sum()
    percent1 <- round((weight1/total)*100,0)
    percent2 <-round((weight2/total)*100,0)
    percent3 <-round((weight3/total)*100,0)
    data.frame(dates=c(input$date51,input$date51,input$date51),
               crop= c(input$select_crop1[1],input$select_crop1[2],input$select_crop1[3]),
               weight= c(percent1,percent2,percent3) )
    
  }) 
  style <- reactive({
    input$graph_type
  })
  
  my_colors<-c("#b33e48", "#08b582", "#4c5138 ")
  
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
           'Normal Bar'= datae3()|> 
             group_by(crop)|>
             e_charts(dates,timeline=TRUE)|>
             e_bar(weight)|>
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
           'Pie'=  data() |>
             group_by(dates)|>
             e_charts(crop)|>
             e_pie(weight)|>
             e_tooltip() |>
             e_animation(duration = 4000)|>
             e_title(paste('A pie Graph % weight of',
                           input$select_crop[1],',',input$select_crop[2],
                           'and',input$select_crop[3],'on',input$date5)) |>
             e_toolbox_feature(feature = "saveAsImage")|>
             e_legend(orient = 'vertical', 
                      right = '5', top = '15%')|>
             e_color(my_colors)
    )
  })
  datam  <- reactive({
    req(input$date2)
    validate(need(!is.na(input$select_crop),
                  "You have not selected any crop"))
    expot() %>% 
      select(dates,input$select_crop)%>% 
      filter(dates > as.POSIXct(input$date2[1]) &
               dates < as.POSIXct(input$date2[2])) %>%
      data.frame(row.names = 1)
  })
  datam1  <- reactive({
    req(input$date21)
    validate(need(!is.na(input$select_crop1),
                  "You have not selected any crop"))
    expot() %>% 
      select(dates,input$select_crop1)%>% 
      filter(dates > as.POSIXct(input$date21[1]) &
               dates < as.POSIXct(input$date21[2])) %>%
      data.frame(row.names = 1)
  })
  
  observe({
    if(input$graph_type=='Cumulative Bar'){
      req(input$date2)
      validate(need(!is.na(input$select_crop),
                    "You have not selected any crop to view"))
      renderBarChart(div_id = "test", stack_plot=TRUE,
                     axis.x.name="Dates", 
                     axis.y.name="Weight exported in kgs",
                     data=datam()
      )
    } else {
      plottype()
    }
  })
  observe({
    req(input$date21)
    validate(need(!is.na(input$select_crop1),
                  "You have not selected any crop to view"))
    renderBarChart(div_id = "test11",stack_plot=TRUE,
                   axis.x.name="Dates", 
                   axis.y.name="Weight exported in kgs",
                   data=datam1()
    )
  })
  output$graph2<-renderEcharts4r({
    plottype() 
  })
  datas <- reactive({
    validate(need(!is.na(input$select_crop), 
                  "You have not selected any crop to view"))
    validate(need(!is.na(input$select_crop[1]), "You have not selected any Column"))
    validate(need(!is.na(input$select_crop[2]), "You have not selected any Column"))
    validate(need(!is.na(input$select_crop[3]), "You have not selected any Column"))
    validate(need(!is.na(input$date2), "Pie Date required*"))
    total <- expos()%>% filter(expos()$crop%in%input$select_crop,dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])) %>% select(weight)%>%sum()
    weight1 <- expos()%>% filter(expos()$crop%in%input$select_crop[1],dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])) %>% select(weight)%>%sum()
    weight2<- expos()%>% filter(expos()$crop%in%input$select_crop[2],dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])) %>% select(weight)%>%sum()
    weight3<-expos()%>% filter(expos()$crop%in%input$select_crop[3],dates > as.POSIXct(input$date2[1]) & dates < as.POSIXct(input$date2[2])) %>% select(weight)%>%sum()
    values <- c(
      rep(input$select_crop[1],weight1),
      rep(input$select_crop[2],weight2),
      rep(input$select_crop[3],weight3)
    )
  })
  dataw <- reactive({
    validate(need(!is.na(input$select_crop1), 
                  "You have not selected any crop to view"))
    validate(need(!is.na(input$select_crop1[1]), "You have not selected any Column"))
    validate(need(!is.na(input$select_crop1[2]), "You have not selected any Column"))
    validate(need(!is.na(input$select_crop1[3]), "You have not selected any Column"))
    validate(need(!is.na(input$date21), "Pie Date required*"))
    total <- expos()%>% filter(expos()$crop%in%input$select_crop1,dates > as.POSIXct(input$date21[1]) & dates < as.POSIXct(input$date21[2])) %>% select(weight)%>%sum()
    weight1 <- expos()%>% filter(expos()$crop%in%input$select_crop1[1],dates > as.POSIXct(input$date21[1]) & dates < as.POSIXct(input$date21[2])) %>% select(weight)%>%sum()
    weight2<- expos()%>% filter(expos()$crop%in%input$select_crop1[2],dates > as.POSIXct(input$date21[1]) & dates < as.POSIXct(input$date21[2])) %>% select(weight)%>%sum()
    weight3<-expos()%>% filter(expos()$crop%in%input$select_crop1[3],dates > as.POSIXct(input$date21[1]) & dates < as.POSIXct(input$date21[2])) %>% select(weight)%>%sum()
    values <- c(
      rep(input$select_crop1[1],weight1),
      rep(input$select_crop1[2],weight2),
      rep(input$select_crop1[3],weight3)
    )
  })
  observe({
    req(input$date2[1])
    req(input$date2[2])
    req(input$select_crop[1])
    req(input$select_crop[2])
    req(input$select_crop[3])
    renderPieChart(div_id = "test1",
                   data = datas(),
                   radius = "70%",center_x = "50%",
                   center_y = "50%")
  })
  observe({
    req(input$date21[1])
    req(input$date21[2])
    req(input$select_crop1[1])
    req(input$select_crop1[2])
    req(input$select_crop1[3])
    renderPieChart(div_id = "test12",
                   data = dataw(),
                   radius = "70%",center_x = "50%", center_y = "50%")
  })
  datae <-reactive({
    req(input$date)
    debt() %>%
      filter(
        indicator%in%input$indicator1,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
      )
  })
  datae4<-reactive({
    req(input$date)
    debt() %>%
      filter(
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
      )
  })
  
  output$graph3<-renderEcharts4r({
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    if(input$comp==TRUE){
      datae4()|> 
        group_by(indicator)|>
        e_charts(date)|>
        e_line(value,symbol='none')|> 
        e_animation(duration = 4000)|>
        e_tooltip(trigger='axis')|>
        e_axis_labels(x='Years',y = 'Level of debt in Million Ksh.')|>
        e_title(paste('A Comparative Graph of Domestic and External Debt in Kenya'),
                left='center',top=10)|>
        e_toolbox_feature(feature = "saveAsImage")|>
        e_legend(orient = 'vertical',right = '5', top = '15%')|>
        e_color(my_colors)
    }else{
      datae()|>
        e_charts(date)|>
        e_line(value,symbol='none')|>
        e_animation(duration = 4000)|>
        e_tooltip(trigger='axis')|>
        e_axis_labels(x='Years',y = 'Level of debt in Million Ksh.')|>
        e_title(paste('Trend in',input$indicator1,'in Kenya'),
                left='center',top=10)|>
        e_toolbox_feature(feature = "saveAsImage")|>
        e_color(my_colors)|>
        e_legend(orient = 'vertical', 
                 right = '5', top = '15%')
    }
  })
  dataz<-reactive({
    validate(need(!is.na(input$select_crop1), "You have not selected any crop to view"))
    expos() %>% select(dates,crop,weight)%>% filter(expos()$crop%in% input$select_crop1,dates > as.POSIXct(input$date21[1]) & dates < as.POSIXct(input$date21[2]))
  })
  output$grapha <- renderEcharts4r({
    dataz()|> 
      group_by(crop)|>
      e_charts(dates)|>
      e_line(weight,symbol='none')|> 
      e_animation(duration = 4000)|>
      e_tooltip(trigger='axis')|>
      e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
      e_title(paste('A Line','Graph of',input$select_crop1[1],',',input$select_crop1[2],'and',input$select_crop1[3]),
              left='center',top=10)|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_legend(orient = 'vertical',right = '5', top = '15%')|>
      e_color(my_colors)
  })
  output$graphb <- renderEcharts4r({
    dataz()|> 
      group_by(crop)|>
      e_charts(dates,timeline=TRUE)|>
      e_bar(weight)|>
      e_animation(duration = 4000)|>
      e_timeline_opts(autoPlay = TRUE, top = "55")|>
      e_tooltip(trigger='axis')|>
      e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
      e_title(paste('A Bar Graph of',input$select_crop1[1],',',input$select_crop1[2],'and',input$select_crop1[3]),
              left='center',top=10)|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_legend(orient = 'vertical', 
               right = '5', top = '15%')|>
      e_color(my_colors) 
  })
  output$graphc <- renderEcharts4r({
    dataz()|> 
      group_by(crop)|>
      e_charts(dates)|>
      e_bar(weight)|>
      e_animation(duration = 4000)|>
      e_tooltip(trigger='axis')|>
      e_axis_labels(x='Dates',y = 'Weight Exported in Kgs.')|> 
      e_title(paste('A Comparative Bar Graph of',input$select_crop1[1],',',input$select_crop1[2],'and',input$select_crop1[3]),
              left='center',top=10)|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_legend(orient = 'vertical', 
               right = '5', top = '15%')|>
      e_color(my_colors)
  })
  output$graphd <- renderEcharts4r({
    datay() |>
      group_by(dates)|>
      e_charts(crop)|>
      e_pie(weight)|>
      e_tooltip() |>
      e_animation(duration = 4000)|>
      e_title(paste('A Pie Graph % weight on',
                    input$date51)) |>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_legend(orient = 'vertical', 
               right = '5', top = '15%')|>
      e_color(my_colors)
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
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  upload <- reactive({
    req(input$file1)
    myfile <- input$file1
    ext <- tools::file_ext(myfile$name)
    datas <-switch(ext,
                   csv = read.csv(file =myfile$datapath),
                   tsv = read.tsv(file=myfile$datapath),
                   xls = readxl::read_xls(path=myfile$datapath),
                   xlsx = readxl::read_xlsx(path=myfile$datapath),
                   validate("Invalid file: Please upload a .csv , .tsv , .xls or .xlsx file!")
    )
    write.csv(datas,"logs.csv")
    final_file <- read.csv("logs.csv")
    unlink("logs.csv")
    data.table::as.data.table(final_file)
  })
  
  df1 <- eventReactive(input$preview,{
    validate(need(input$n< 11,"Error: Preview a Max of 10 rows!"))
    validate(need(input$n > 1,"Error: Preview from 1 row!"))
    validate(need(!is.na(input$file1), "Error: You have not uploaded any file!"))
    validate(need(!is.na(input$n), "Error: Number of rows can't be empty!"))
    withProgress(message = 'Previewing...', value = 0, {
      n<- input$n
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Loading",(i/n)*100,'%'))
        Sys.sleep(0.04)
      }
    })
    if(input$disp == "head") {
      return(head(upload(),input$n))
    }
    else {
      return(upload())
    }
  })
  observeEvent(input$n, {
    shinyFeedback::feedbackWarning('n',input$n > 10,"*Exceeded limit")
    shinyFeedback::feedbackWarning('n',input$n < 1,"*Exceeded limit")
  })
  output$hot <- rhandsontable::renderRHandsontable (
    data <- rhandsontable::rhandsontable(df1(),width = 700, height = 610,
                                         stretchH = "all",useTypes=FALSE)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))
  )
  observeEvent(upload(),{
    showNotification(ui="A comma-deliminated csv data is recommended for better analysis",
                     type='message',
                     duration= 10
    )
    choices <- c(not_sel,names(upload()))
    
    updateSelectInput(session, "axis1", choices = choices)
    updateSelectInput(session, "axis2", choices = choices)
    updateSelectInput(session, "group", choices = choices)
    updateSelectInput(session, "typeof", choices = choices)
    
  })
  axis1 <- eventReactive(input$run_button,input$axis1 )
  axis2 <- eventReactive(input$run_button,input$axis2 )
  group <- eventReactive(input$run_button, input$group )
  graphty<- eventReactive(input$run_button,{
    validate(need(!is.na(input$file1), "Error: You have not uploaded any file!"))
    draw_plot(upload(), axis1(),axis2(), group())
  })
  graphty2<- eventReactive(input$run_button,{
    validate(need(!is.na(input$file1), "Waiting for an Upload to load..."))
    if(input$axis1 !=not_sel & input$axis2 !=not_sel){
      k<- ggplot(data = upload(),
                 aes_string(x = input$axis1, y = input$axis2)) +
        geom_point()+ theme1
      if (input$se==TRUE & input$tick==TRUE){
        k+stat_smooth(method='lm', col='yellow',se=TRUE,size=1,linetype=1)
      }
      else if(input$tick==TRUE){
        k+ stat_smooth(method='lm',col='yellow',se=FALSE,size=1,linetype=1)
      }
      else if(input$se==TRUE){
        k+stat_smooth(method='lm',se=input$se, col='yellow',size=1,linetype=1)
      }
      else {
        return(k)
      }
    }else {
      return()
    }
  })
  output$graph4 <- renderPlotly(graphty())
  statement_results <- eventReactive(input$run_button, {
    regression_statement(upload(), axis1(), axis2())
  })
  output$results<- renderUI (statement_results())
  
  output$graph5 <- renderPlotly(graphty2())
  output$num_var_1_title <- renderText(paste("X Variable:",axis1()))
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(upload(), axis1())
  })
  output$num_var_1_summary_table <- renderTable({
    num_var_1_summary_table() })
  
  output$num_var_2_title <- renderText(paste("Y Variable:",axis2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(upload(), axis2())
  })
  
  output$num_var_2_summary_table <- renderTable({
    num_var_2_summary_table() })
  
  output$fact_var_title <- renderText(paste("Factor Variable:",group()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(upload(), group())
  })
  
  output$fact_var_summary_table <- renderTable(
    fact_var_summary_table(),colnames = FALSE)
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(upload(), axis1(), axis2(), group())
  })
  
  output$combined_summary_table <- renderTable({
    combined_summary_table()
  })
  regression_table  <- eventReactive(input$run_button, {
    create_regression_table(upload(), axis1(), axis2())
  })
  output$regression_summary_table <- renderTable({
    regression_table()
  })
  
  output$interpretation <- renderUI({
    if(axis1() !=not_sel & axis2() !=not_sel){
      if(input$axis1%in%"Not Selected" | input$axis2%in%"Not Selected"){ return( )}else {
        y<- upload()%>%select(input$axis2)%>%as_vector()
        x<- upload()%>%select(input$axis1)%>%as_vector()
        fit<- lm(y~x)  
        if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
          withMathJax(
            paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
            br(),
            paste0("For a (hypothetical) value of ", input$axis1, " = 0, the mean of ", input$axis2, " = ", round(fit$coef[[1]], 3), "."),
            br(),
            paste0("For an increase of one unit of ", input$axis1, ", ", input$axis2, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
          )
        } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
          withMathJax(
            paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
            br(),
            paste0("For a (hypothetical) value of ", input$axis1, " = 0, the mean of ", input$axis2, " = ", round(fit$coef[[1]], 3), "."),
            br(),
            paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$axis1, " and ", input$axis2, ".")
          )
        } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
          withMathJax(
            paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
            br(),
            paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$axis1, " = 0, the mean of ", input$axis2, " is not significantly different from 0."),
            br(),
            paste0("For an increase of one unit of ", input$axis1, ", ", input$axis2, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
          )
        } else {
          withMathJax(
            paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
            br(),
            paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$axis2, " is not significantly different from 0.")
          )}
      }
    }else{
      return()
    }
  })
  output$value1  <- renderText({
    if(axis1() !=not_sel & axis2() !=not_sel){
      if(input$axis1%in%"Not Selected" | input$axis2%in%"Not Selected"){ return( )}else {
        y<- upload()%>%select(input$axis2)%>%as_vector()
        x<- upload()%>%select(input$axis1)%>%as_vector()
        fit<- lm(y~x)  
        value <- round(fit$coef[[1]], 3)+ 
          round(fit$coef[[2]], 3)*input$predict
        text<- paste('The predicted(approximated)', axis2() , 'at value', input$predict, 'of',axis1(), 'is', value )
      }
    }else {
      return()
    }
  })
  
  output$typeof1 <- renderText({
    upload1<- eventReactive(input$preview,upload())
    if(input$typeof%in%"Not Selected"){return()} else {
      cols <- eventReactive(input$preview, input$typeof)
      all(sapply(upload1()[,c(cols())],is.numeric))
    }
  })
  
  
  observeEvent(input$dataset,{
    if(input$dataset%in%'Exports'){
      shinyjqui::jqui_show("#dt_table","fade")
      updateActionButton(session, 'add_export',label='Add Export')
      output$dt_table <- renderDT(
        {
          shiny::isolate(rv$df)
        },
        escape = F,
        rownames = FALSE,
        options = list(processing = FALSE)
      )
    }else if(input$dataset%in% 'Kenya Debt'){
      shinyjqui::jqui_show("#dt_table","fade")
      updateActionButton(session, 'add_export',label='Add Debt Value')
      output$dt_table <- renderDT(
        {
          shiny::isolate(rv1$df)
        },
        escape = F,
        rownames = FALSE,
        options = list(processing = FALSE)
      )
    }else if(input$dataset%in% 'Remittances'){
      updateActionButton(session, 'add_export',label='Add Remittances Records')
      shinyjqui::jqui_show("#dt_table","fade")
      
    }else if(input$dataset%in% 'Results'){
      updateActionButton(session, 'add_export',label='Edit column name',icon=icon("highlighter"))
      shinyjqui::jqui_hide("#dt_table","fade")
      output$results_table <- rhandsontable::renderRHandsontable ({
        rhandsontable::rhandsontable(results(),
                                     width = 700,
                                     height = 300,
                                     stretchH = "all",
                                     useTypes=FALSE)%>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })
    }
    else{
      return()
    }
  })
  shiny::observe({
    x<- unique(expos()[expos()$type%in%input$type,"crop"])
    shiny::updateSelectInput(session,
                             'crop',
                             label=paste('Select',input$type,sep = ' '),
                             choices=x,
                             selected=x[1]
    )
  })
  shiny::observeEvent(input$add_export, {
    if(input$dataset%in%'Exports'){
      modal_dialog(
        dates = "", type = "", crop = "", weight= "", edit = FALSE
      )
      rv$add_or_edit <- 1
    }else if(input$dataset%in%'Kenya Debt'){
      modal_dialog1(
        date = "", indicators = "",value = "", edit = FALSE
      )
      rv1$add_or_edit <- 1
    }else if(input$dataset%in%'Results'){
      shinyjs::show('cols_name')
      choices_cols<- c(names(results()))
      updateSelectInput(session, inputId = "choose_cols",
                        choices =choices_cols 
      )
    }
    else{
      return()
    }
  })
  
  rv <- shiny::reactiveValues(
    df = mtcars,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(mtcars) + 1
  )
  rv1 <- shiny::reactiveValues(
    df = mtcars1,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(mtcars1) + 1
  )
  shiny::observeEvent(input$current_id, {
    if(input$dataset%in%'Exports'){
      shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))
      rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
      rv$df <- rv$df[-rv$dt_row, ]
      proxy <- DT::dataTableProxy("dt_table")
      DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
    }else if(input$dataset%in%'Kenya Debt'){
      shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))
      rv1$dt_row <- which(stringr::str_detect(rv1$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
      rv1$df <- rv1$df[-rv1$dt_row, ]
      proxy <- DT::dataTableProxy("dt_table")
      DT::replaceData(proxy, rv1$df, resetPaging = FALSE, rownames = FALSE)
    }else{
      return()
    }
  })
  shiny::observeEvent(input$current_id, {
    if(input$dataset%in%'Exports'){
      shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
      rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
      df <- rv$df[rv$dt_row, ]
      modal_dialog(
        dates= df$dates, type = df$type, crop = df$crop,  weight = df$weight, edit = TRUE
      )
      rv$add_or_edit <- NULL
    }else if(input$dataset%in%'Kenya Debt'){
      shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
      rv1$dt_row <- which(stringr::str_detect(rv1$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
      df <- rv1$df[rv1$dt_row, ]
      modal_dialog1(
        datex= df$date, indicators = df$indicator,value = df$value, edit = TRUE
      )
      rv1$add_or_edit <- NULL
    }else{
      return()
    }
  })
  shiny::observeEvent(input$final_edit, {
    req(input$weight)
    req(input$dates)
    req(input$weight!=0)
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit") & is.null(rv$add_or_edit))
    rv$edited_row <- dplyr::tibble(
      dates= input$dates,
      type = input$type,
      crop = input$crop, 
      weight = input$weight,
      Buttons = rv$df$Buttons[rv$dt_row]
    )
    rv$df[rv$dt_row, ] <- rv$edited_row
    
  })
  shiny::observeEvent(input$final_edit,{
    req(input$weight)
    req(input$dates)
    req(input$weight!=0)
    shiny::req(rv$add_or_edit == 1)
    add_row <- dplyr::tibble(
      dates= input$dates,
      type = input$type,
      crop = input$crop, 
      weight = input$weight,
      Buttons = create_btns(rv$keep_track_id)
    )
    rv$df <- add_row %>%
      dplyr::bind_rows(rv$df)
    rv$keep_track_id <- rv$keep_track_id + 1
    proxy <- DT::dataTableProxy("dt_table")
    shiny::observe({
      req(input$crop)
      req(input$dates)
      req(input$weight)
      DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
    })
  })
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  shiny::observeEvent(input$final_edit, {
    req(input$crop)
    req(input$weight)
    req(input$weight!=0)
    req(input$dates)
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$final_edit1, {
    req(input$value)
    req(input$value!=0)
    req(input$datex)
    req(input$indicators)
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit") & is.null(rv1$add_or_edit))
    rv1$edited_row <- dplyr::tibble(
      date= input$datex,
      indicator = input$indicators,
      value = input$value,
      Buttons = rv1$df$Buttons[rv1$dt_row]
    )
    rv1$df[rv1$dt_row, ] <- rv1$edited_row
  })
  shiny::observeEvent(input$final_edit1,{
    req(input$value!=0)
    req(input$value)
    req(input$datex)
    shiny::req(rv1$add_or_edit == 1)
    add_row <- dplyr::tibble(
      date= input$datex,
      indicator = input$indicators,
      value = input$value,
      Buttons = create_btns(rv1$keep_track_id)
    )
    rv1$df <- add_row %>%
      dplyr::bind_rows(rv1$df)
    rv1$keep_track_id <- rv1$keep_track_id + 1
    proxy <- DT::dataTableProxy("dt_table")
    shiny::observe({
      req(input$value)
      req(input$datex)
      req(input$indicators)
      DT::replaceData(proxy, rv1$df, resetPaging = FALSE, rownames = FALSE)
    })
  })
  shiny::observeEvent(input$dismiss_modal1, {
    shiny::removeModal()
  })
  shiny::observeEvent(input$final_edit1, {
    req(input$value)
    req(input$value!=0)
    req(input$datex)
    req(input$indicators)
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$update, {
    if(input$dataset%in%'Exports'){
      showModal(modalDialog(
        tagList('Updating overwrites the existing Exports Data'),
        title='Update Exports Data',
        footer=tagList(loadingButton('confirm','Update Data', 
                                     class   = "btn-info",
                                     loadingLabel = "Updating..."),
                       modalButton('Cancel')),
        size = 'm',
        easyClose = TRUE
      )
      )
    }else if(input$dataset%in%'Kenya Debt'){
      showModal(modalDialog(
        tagList('Updating overwrites the existing Kenya Debt Records'),
        title='Update Kenya Debt Records',
        footer=tagList(loadingButton('confirm1','Update Data', 
                                     class   = "btn-danger",
                                     loadingLabel = "Updating..."),
                       modalButton('Cancel')),
        
        size = 'm',
        easyClose = TRUE
      )
      )
    }else if(input$dataset%in%'Results'){
      showModal(modalDialog(
        tagList('Updating overwrites the existing Results Records'),
        title='Update Results Records',
        footer=tagList(loadingButton('confirm_save','Update Data', 
                                     class   = "btn-danger",
                                     loadingLabel = "Updating..."),
                       modalButton('Cancel')),
        
        size = 'm',
        easyClose = TRUE
      )
      )
    } else{
      return()
    }
  })
  
  observeEvent(input$confirm,{
    final<-subset(rv$df, select = -c(Buttons))
    openxlsx::write.xlsx(final,
                         file ='exports1.xlsx',
                         colNames = TRUE, borders = "columns")
    resetLoadingButton("confirm")
    showToast(
      "success", 
      "EXPORTS DATA UPDATED", 
      .options = myToastOptions
    )
    removeModal()
    
  })
  observeEvent(input$confirm1,{
    final1<-subset(rv1$df, select = -c(Buttons))
    openxlsx::write.xlsx(final1,
                         file = 'kenya_status1.xlsx',
                         colNames = TRUE, borders = "columns")
    resetLoadingButton("confirm1")
    showToast(
      "success", 
      "DEBT RECORDS UPDATED", 
      .options = myToastOptions
    )
    removeModal()
  })
  results_edited<- reactive({
    rhandsontable::hot_to_r(input$results_table)
  })
  observeEvent(input$confirm_save,{
    readr::write_csv(results_edited(),
                     file = 'user_results.xlsx')
    resetLoadingButton("confirm_save")
    showToast(
      "success", 
      "RESULTS UPDATED", 
      .options = myToastOptions
    )
    removeModal()
  })
  
  verify<- modalDialog(
    tags$h2('ARE YOU A DATABASE ADMINISTRATOR AT INFINICALS?'),
    title=tags$h3('DATA ADMINISTATOR'),
    footer=tagList(actionButton('accept','YES', class   = "btn-info"),
                   actionButton('decline','NO', class   = "btn-info")),
    size = 'm',
    easyClose = FALSE)
  
  observeEvent(input$tabs, {
    showModal(verify)
  }, once=TRUE,ignoreInit=TRUE
  )
  
  shiny::hideTab('tabs',target = 'Authenticate',session=session)
  
  observeEvent(input$decline,{
    shiny::hideTab('tabs',target = 'data',session=session)
    shiny::hideTab('tabs',target = 'View Available Data',session=session)
    shiny::showTab('tabs',target = 'Authenticate',session=session)
    shiny::removeModal()
  })
  observeEvent(input$accept,{
    shinyalert::shinyalert(html= TRUE, 
                           title='INFINICALS PASSCODE',
                           showConfirmButton = FALSE,
                           animation = "pop",
                           imageUrl= 'logo.png',
                           imageWidth = "50%",
                           text = tagList(tags$div(
                             passwordInput('passcode'
                                           ,'Passcode:',
                                           placeholder='Enter official passcode'),actionButton('ok','OK'))
                           ))
  })
  observeEvent(input$ok,{
    admin <- details()%>%filter(user%in%"Admin")
    pass <- admin$pswd
    if(input$passcode%in%pass){
      shinyalert::closeAlert()
      shiny::removeModal()
      shiny::showTab('tabs',target = 'data',session=session)
      shiny::hideTab('tabs',target = 'View Available Data',session=session)
      shiny::hideTab('tabs',target = 'Authenticate',session=session)
    } else{
      shinyalert::closeAlert()
      shiny::removeModal()
      shiny::hideTab('tabs',target = 'data',session=session)
      shiny::hideTab('tabs',target = 'View Available Data',session=session)
      shiny::showTab('tabs',target = 'Authenticate',session=session)
      output$base<- renderText({
        validate(need(input$passcode==2140, paste(input$userName, "is not Verified!. You are not allowed to access the Company Database.",sep=' ')))
      })
    }
  })
  
  observeEvent(input$go,{
    showModal(verify)
  })
  observeEvent(input$add_export, {
    if(input$dataset %in%'Kenya Debt'){
      req(input$datex)
      req(input$indicators)
      observeEvent(input$datex, {
        req(input$datex)
        req(input$indicators)
        day <-rv1$df %>% dplyr::filter(grepl(input$datex,date))%>%
          filter(stringr::str_detect(indicator,'domestic_debt'))
        day1 <- rv1$df %>% dplyr::filter(grepl(input$datex,date))%>%
          filter(stringr::str_detect(indicator,'external_debt'))
        day2<- rv1$df %>% dplyr::filter(grepl(input$datex,date))%>%
          filter(stringr::str_detect(indicator,'total'))
        if(nrow(day)==0 & nrow(day1)==0 & nrow(day2)==0){
          enableActionButton("final_edit1",session)
          updateSelectInput(inputId = "indicators",
                            choices =c('domestic_debt','external_debt'))
        }else if(nrow(day)==1 & nrow(day1)==0 & nrow(day2)==0)  {
          enableActionButton("final_edit1",session)
          updateSelectInput(inputId = "indicators",
                            choices ='external_debt')
        }else if(nrow(day)==0 & nrow(day1)==1 & nrow(day2)==0) {
          enableActionButton("final_edit1",session)
          updateSelectInput(inputId = "indicators",
                            choices ='domestic_debt')
        }else if(nrow(day)==1 & nrow(day1)==1 & nrow(day2)==0){
          enableActionButton("final_edit1",session)
          subs<- rv1$df$date
          subs1<-rv1$df%>%dplyr::filter(grepl(input$datex,date))%>%
            select(indicator,value)%>%filter(indicator%in%'domestic_debt')%>%select(value)
          s<- subs1[[1,1]]
          subs2<-rv1$df%>%dplyr::filter(grepl(input$datex,date))%>%
            select(indicator,value)%>%filter(indicator%in%'external_debt')%>%select(value)
          t<-subs2[[1]]
          total=s+t
          updateSelectInput(inputId = "indicators",
                            choices ='total')
          updateNumericInput(session = session,inputId='value',
                             value=total)
        }else if(nrow(day)==1 & nrow(day1)==1 & nrow(day2)==1){
          updateSelectInput(inputId = "indicators",
                            choices ='')
          disableActionButton("final_edit1",session)
        }else{
          return()
        }
      })
    }
  })
  observeEvent(input$reset, {
    showModal(register)
  })
  observeEvent(input$new_user, {
    showModal(register2)
  })
  observeEvent(input$search, {
    iv$enable()
    name <- input$reset_name
    email <- input$reset_email
    if(nchar(name)==0 |nchar(email)==0 ){
      output$found<- renderText({
        "Provide both Username and Email!"
      })
    }else{
      if(any(details()$user%in%name)) {
        user1<- details()%>%filter(user%in%name)
        if(user1$email%in%email){
          shinyjqui::jqui_hide("#part1","fade")
          shinyjs::show('part2')
          output$found<- renderText({
            return()
          })
        }else{
          output$found<- renderText({
            paste("The email did not match the user!")
          })
        }
      }else{
        output$found <- renderText({
          paste("Username not found!")
        })
      }
    }
  })
  observeEvent(input$search2, {
    pass1<- input$new_pass1
    pass2<-input$new_pass2
    if(nchar(pass1)==0 |nchar(pass2)==0 ){
      output$found <- renderText({
        paste("Fill both inputs!")
      })
      resetLoadingButton("search2")
    } else {
      if (pass1%in%pass2){
        name <-input$reset_name
        login_details <- details()%>%
          mutate_at("pswd", ~replace(.,user ==name,pass1))
        openxlsx::write.xlsx(login_details,
                             file ='login_details.xlsx',
                             colNames = TRUE, borders = "columns")
        resetLoadingButton("search2")
        removeModal()
        showToast(
          "success", 
          "Password changed successfully!", 
          .options = myToastOptions
        )
      }else {
        output$found <- renderText({
          paste("The Passwords did not match!")
        })
        resetLoadingButton("search2")
      }
    }
  })
  observeEvent(input$user_save, {
    iv$enable()
    
    email1 <- input$your_email
    name1  <- input$your_name
    pass3 <- input$my_pass1
    pass4 <- input$my_pass2
    if(nchar(pass3)==0 |nchar(pass4)==0 |nchar(email1)==0| 
       nchar(name1)==0  ){
      output$found1<- renderText({
        paste("Fill all the inputs!")
      })
      resetLoadingButton("user_save")
    }else {
      if(pass3%in%pass4){
        new_row <- c(name1,pass3,email1)
        login_details_1<- rbind(login_details,new_row)
        openxlsx::write.xlsx(login_details_1,
                             file ='login_details.xlsx',
                             colNames = TRUE, borders = "columns")
        resetLoadingButton("user_save")
        removeModal()
        showToast(
          "success", 
          "User added successfully!", 
          .options = myToastOptions
        )
      }else {
        output$found1<- renderText({
          paste("Passwords did not match!")
        })
        resetLoadingButton("user_save")
      }
    }
    
  })
  observeEvent(input$change, {
    col_name<- input$new_col_name
    chosen<- input$choose_cols
    if(nchar(col_name)==0){
      return()
    }else {
      shinyjs::hide('cols_name')
      rename(results(), chosen =col_name)
    }
  })
  
}

shinyApp(ui = ui, server = server)
