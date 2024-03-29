modal_dialog <- function(dates,type,crop, weight,edit) {
  exports1<- openxlsx::read.xlsx('exports1.xlsx',detectDates = TRUE)
  not_sel <- "Not Selected"
  if(edit) {
    x <- "Submit Edits"
  } else {
    x <-"Save New Export"
  }
  if(edit) {
    y<- tags$h2('Edit Export')
  } else {
    y<- tags$h2('Add new Export')
  }
  shiny::modalDialog(
    title = y,
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        tags$style(type = "text/css", ".datepicker { z-index: 99999 !important; }"),
        shiny::dateInput (inputId = "dates",
                          label = "Date",
                          value = NULL, 
                          width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(inputId = "type",
                           label = "Type", 
                           width = "200px",
                           selected =unique(exports1$type)[1],
                           choices = unique(exports1$type))
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput( inputId = "crop", 
                            label = "Crop Exported", 
                            width = "200px",
                            selected =crop,
                            choices =c(not_sel)
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "weight",
                            label = "Weight Exported",
                            value = weight, 
                            width = "200px")
      )
    ),
    size = 'm',
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(inputId = "final_edit",
                          label   = x,
                          icon = shiny::icon("edit"),
                          class = "btn-info"),
      shiny::actionButton(inputId = "dismiss_modal",
                          label   = "Close",
                          class   = "btn-danger")
    )
  ) %>% shiny::showModal()
  
}
