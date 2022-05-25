modal_dialog <- function(dates,type,crop, weight,edit) {
  exports1<- readxl::read_xlsx(path = "~/Programming/R/DATA/exports1.xlsx")
  if(edit) {
    x <- "Submit Edits"
  } else {
    x <- "Save New Export"
  }
  if(edit) {
    y<- 'Edit Export'
  } else {
    y<-'Add new Export'
  }
  shiny::modalDialog(
    title = y,
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
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
                           selected =type,
                           choices = unique(exports1$type))
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput( inputId = "crop", 
                            label = "Crop Exported", 
                            width = "200px",
                            selected =crop,
                            choices =''
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
