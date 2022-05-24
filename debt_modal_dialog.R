modal_dialog1 <- function(date,indicator,value,edit) {
  kenya_status1<-read.csv("~/Programming/R/DATA/kenya_status1.csv")
  if(edit) {
    x <- "Submit Debt Edits"
  } else {
    x <- "Save New Record"
  }
  if(edit) {
    y<- 'Edit Debt Records'
  } else {
    y<-'Add New Debt Record'
  }
  shiny::modalDialog(
    title = y,
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::dateInput (inputId = "date",
                          label = "Date",
                          value = NULL, 
                          width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(inputId = "indicator",
                           label = "Debt type", 
                           width = "200px",
                           choices =c('domestic_debt','external_debt'))
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "value",
                            label = "Debt Value",
                            value = value, 
                            width = "200px")
      )
    ),
    size = 'm',
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(inputId = "final_edit1",
                          label   = x,
                          icon = shiny::icon("edit"),
                          class = "btn-info"),
      shiny::actionButton(inputId = "dismiss_modal1",
                          label   = "Close",
                          class   = "btn-danger")
    )
  ) %>% shiny::showModal()
  
}
