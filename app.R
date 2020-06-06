library(shiny)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)
library(tibble)

source("cellModule.R")

ui <- fluidPage(
    fluidRow(
        column(width= 3,
               #numericInput("desired_rows","Desired Rows", value = 3, min = 1, max = 10, step = 1),
               actionButton("show_script", "Show Script")
        )
    ),
    uiOutput("table"),
    hr(),
    tableOutput("selected_result"),
    tableOutput("result_table")
)


server <- function(input, output) {
    desired_rows <- 5
  
    hyou <- reactive({
        crossing(x = 1:12, y = 1:desired_rows) %>% 
            mutate(widget = "",
                   labelid = "",
                   arguments = "",
                   width = 1,
                   offset = 0)
    })

    output$table <- renderUI({
        map(1:desired_rows, ~{
            current_row <- .
            fluidRow(
                map(1:12,~{
                    current_col <- .
                    
                    
                    current_data      <- hyou() %>% filter(x == current_col, y == current_row)
                    current_widget    <- current_data %>% pull(widget)
                    current_labelid   <- current_data %>% pull(labelid)
                    current_arguments <- current_data %>% pull(arguments)
                    current_width     <- current_data %>% pull(width)
                    current_offset    <- current_data %>% pull(offset)
                    
                    
                                        
                    cellModule(
                        id        = str_c("cell_",current_col,"_",current_row),
                        widget    = current_widget,
                        labelid   = current_labelid,
                        arguments = current_arguments,
                        width     = current_width, 
                        offset    = current_offset
                    )
                })
            )
            
        })
    })
    
    res <- reactive({
        res <- crossing(x = 1:12, y = 1:desired_rows) %>% 
            mutate(res = map2(x,y, ~{callModule(cell,str_c("cell_",.x,"_",.y))}))
        
        
        res <- res %>% 
            mutate(widget = map_chr(res, ~{.$widget()})) %>% 
            mutate(labelid = map_chr(res, ~{.$labelid()})) %>% 
            mutate(argument = map_chr(res, ~{.$argument()})) %>% 
            select(-res)
        
        
        res <- res %>% 
            filter(widget != "NA")
        
        return(res)
    })
    
    
   output$result_table <- renderTable({
       
       return(res())
   })
   
   
   observeEvent(input$show_script,{
      
       dat <- res()
       
       if(nrow(dat)==0){return()}
       
       
       dat <- dat %>% 
           group_by(y) %>% 
           nest()
       
       
       dat2 <- dat %>% 
           mutate(script = map(data,~{
               
               adat <- .
               
               min_x <- adat$x %>% min()
               
               if(min_x == 1){
                   init_offset <- 0
               }else{
                   init_offset <- min_x - 1
               }
               
               
               ascript <- adat %>% 
                   arrange(x) %>%
                   
                   mutate(input_output = str_extract(widget,"Input$|Output$")) %>%
                   mutate(input_output = if_else(input_output=="Output","outputId","inputId")) %>% 
                   
                   mutate(offset = if_else(x == min_x, init_offset, 0)) %>% 
                   
                   mutate(width = lead(x) - x) %>% 
                   mutate(width = if_else(is.na(width), as.integer(12-x+1), width)) %>% 
                   
                   mutate(col_script = str_c("column(width = ", width, ", offset = ", offset,", ")) %>% 
                   mutate(frame_script = str_c(col_script,"ui_",labelid,")")) %>%
                   
                   mutate(argument = if_else(input_output == "inputId", str_c(",",argument),argument)) %>% 
                   
                   mutate(widget_script = str_c("ui_",labelid," <- ",widget,"(",input_output," = '",labelid, "'", argument,")"))
               
               return(ascript)
          })) %>% 
           
           mutate(
               frame_script = map_chr(script, ~{
                   str_c("\tfluidRow(\n\t\t", 
                         str_c(.$frame_script,collapse = ",\n\t\t"),
                         "\n\t)", 
                         collapse = "")
               })
           ) %>% 
           
           mutate(
               ui_frame_script = map_chr(script, ~{
                   str_c(.$widget_script,collapse = "\n")
               })
           )
       
       
       
       ui_frame_script <- str_c(
           "ui <- fluidPage(\n",
           str_c(dat2$frame_script,collapse = ",\n"),
           "\n)",
           collapse = "\n"
       )
       
       ui_widgets_script <- str_c(
           str_c(dat2$ui_frame_script,collapse = "\n")
       )
       
      cat(ui_frame_script)
      cat(ui_widgets_script)
      
      #Server section
      temp <- dat2 %>% select(script) %>% unnest(script)
      
      inputids <- temp %>% filter(input_output == "inputId") %>% pull(labelid) %>% str_c("input$",.)
      
      outputdat <- temp %>% ungroup() %>%  filter(input_output == "outputId") %>% 
          mutate(renderthis = str_extract(widget,"^.+(?=Output)")) %>% 
          select(labelid, renderthis) %>% 
          mutate(Firstletter = str_to_upper(str_sub(renderthis,1,1))) %>% 
          mutate(remainingletters = str_sub(renderthis,2,nchar(renderthis))) %>% 
          mutate(Renderthis = str_c(Firstletter, remainingletters)) %>% 
          mutate(output_list_script = str_c("\toutput$", labelid, "<- render", Renderthis, "({\n\t})"))
       
      if(length(inputids)>0){
          input_list_script <- str_c("\t#Available Ids for input$ are:", str_c(inputids,collapse = ", "))    
      }else{
          input_list_script <- ""
      }
      
      if(nrow(outputdat)>0){
          output_list_script <- outputdat$output_list_script %>% str_c(collapse="\n")
      }else{
          output_list_script <- ""
      }
      
      final_script <- str_c(collapse="\n",c(
          "library(shiny)",
          "",
          #Widgets in ui------------",
          ui_widgets_script,
          "",
          "#UI:",
          ui_frame_script,
          "",
          "#Server",
          "server <- function(input, output) {",
          input_list_script,
          "",
          output_list_script,
          "}",
          "",
          "shinyApp(ui = ui, server = server)"
          )
      )
      
      
      cat(final_script)
      
      #make modal to copy!
      
      showModal(modalDialog(
          title = "Skelton for your shiny app",
          span(style = "white-space: pre-wrap", final_script)
      ))
      
      
   })
}


shinyApp(ui = ui, server = server)
