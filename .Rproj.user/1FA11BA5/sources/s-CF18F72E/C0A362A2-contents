library(shiny)
library(magrittr)
library(tibble)
library(dplyr)

status_setting <- tribble(
  ~widget               , ~arguments,
  'NA'                  , ''        ,
  'actionButton'        , 'label = "", icon = NULL, width = NULL, ...',
  'actionLink'          , 'label = "", icon = NULL, width = NULL, ...',
  'checkboxGroupInput'  , 'label = "", choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL',
  'checkboxInput'       , 'label = "", value = FALSE, width = NULL',
  'dateInput'           , 'label = "", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL, autoclose = TRUE, datesdisabled = NULL, daysofweekdisabled = NULL',
  'dateRangeInput'      , 'label = "", start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL, autoclose = TRUE',
  'fileInput'           , 'label = "", multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"',
  'numericInput'        , 'label = "", value, min = NA, max = NA, step = NA, width = NULL',
  'passwordInput'       , 'label = "", value = "", width = NULL, placeholder = NULL',
  'radioButtons'        , 'label = "", choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL',
  'selectInput'         , 'label = "", choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL',
  'sliderInput'         , 'label = "", min, max, value, step = NULL, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE',
  'textInput'           , 'label = "", value = "", width = NULL, placeholder = NULL',
  'imageOutput'         , '',
  'plotOutput'          , '',
  'tableOutput'         , '',
  'textOutput'          , '',
  'uiOutput'            , ''
)

cellModule <- function(id,widget, labelid, arguments, width = 1, offset = 0){
  ns <- NS(id)
  
  ui_widget   <- selectInput(inputId = ns("type"), label="Widget", choices = status_setting$widget, selected = widget)
  ui_labelid  <- textInput(inputId = ns("labelid"), label = "id")
  
  column(
    width = width, 
    offset = offset, 
    style='border-style: solid; border-width: 1px;', 
    div(ui_widget, ui_labelid)
  )
}


cell <- function(input, output, session){
  
  return(
    list(
      widget  = reactive({ input$type }),
      labelid = reactive({ input$labelid }),
      argument= reactive({
        status_setting %>%
          filter(widget == input$type) %>%
          pull(arguments)
      })
    )
  )
  
}