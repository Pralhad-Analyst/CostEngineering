data_tracksheet <- data.frame(
  GDMS = as.factor(c("Yes", "Yes", "Yes", "Yes")),
  PN_No = as.character(""),
  Rev = as.character(""),
  Descr1 = as.character(""),
  Descr2 = as.character(""),
  Project = as.character(""),
  Group = as.character(""),
  Commodity = as.character(""),
  Sub_Commodity = as.character(""),
  Requestor = as.character(""),
  Owner = as.character(""),
  Request_Date = as.character(""),
  Estimated_Delivery = as.character(""),
  Actual_Delivery = as.character(""),
  Lead_Time = as.numeric(""),
  Delays = as.numeric(""),
  GDMS_Ear = as.numeric(""),
  Calc_Ear = as.numeric(""),
  Plant = as.character(""),
  CA_No = as.character(""),
  Supplier_Region = as.character(""),
  Supplier_Exworks = as.numeric(""),
  Supplier_Currency = as.character(""),
  Supplier_Logistics = as.numeric(""),
  Supplier_LandedCost = as.numeric(""),
  Supplier_LandedCurrency = as.character(""),
  Supplier_Name = as.character(""),
  Calc_Region1 = as.character(""),
  Calc_Exworks1 = as.numeric(""),
  Calc_Currency1 = as.character(""),
  Calc_Logistics1 = as.numeric(""),
  Calc_LandedCost1 = as.numeric(""),
  Calc_LandedCurrency1 = as.character(""),
  Analysis_Type = as.character(""),
  Calc_Region2 = as.character(""),
  Calc_Exworks2 = as.numeric(""),
  Calc_Currency2 = as.character(""),
  Calc_Logistics2 = as.numeric(""),
  Calc_LandedCost2 = as.numeric(""),
  Calc_LandedCurrency2 = as.character(""),
  Calc_Region3 = as.character(""),
  Calc_Exworks3 = as.numeric(""),
  Calc_Currency3 = as.character(""),
  Calc_Logistics3 = as.numeric(""),
  Calc_LandedCost3 = as.numeric(""),
  Calc_LandedCurrency3 = as.character(""),
  Project_Comment = as.character(""),
  Project_status = as.character(""),
  Potential_Savings = as.numeric(""),
  Potential_Currency = as.character("")
)

gdms <- readRDS("D:/Pralhad/git_CostEngineering/gdms_data.RDS")

track_path <- file.path("D:", "Pralhad", "git_CostEngineering", "track_data", fsep = "/")

loadData <- function(path){
  files <- list.files(file.path(path), full.names = T)
  d <- purrr::map_df(files, readRDS)
  dataa <- dplyr::bind_rows(d)
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

saveData <- function(data, path){
  fileName <- sprintf("%s_%s.RDS", 
                      humanTime(),
                      digest::digest(data))
  
  saveRDS(data, file = file.path(path, fileName), ascii = T)
}


library(shiny)
library(rhandsontable)
library(digest)
library(shinyjs)
library(purrr)
library(DT)

ui <- navbarPage(
  "Cost Engineering",
  tabPanel("Tracksheet",
           
           wellPanel(
             
             fluidRow(
               column(12, 
                      h3("Tracksheet Fill-up Form"),
                      tags$hr(),
                      
                      fluidRow(
                        column(2, selectInput("gdms", "GDMS?", choices = c("Yes", "No"))),
                        column(2, textInput("project_requestor", "Requestor")),
                        column(2, textInput("project_owner", "Project Owner")),
                        column(2, textInput("project_status", "Status"))
                 
                      )),
               
               column(12,
                      h4("Part No. Table"),
                      fluidRow(
                        column(12, rHandsontableOutput("table_tracksheet"))
                      )),
               
               column(12,
                      actionButton("table_submit", "Submit", class = "btn-primary"))
             )
           ))
)

server <- function(input, output, session){
  
  dataValues <- reactiveValues(data = data_tracksheet)

  
  observe({
    
    data_tracksheet$GDMS <- as.character(input$gdms)
    data_tracksheet$Requestor <- as.character(input$project_requestor)
    data_tracksheet$Owner <- as.character(input$project_owner)
    data_tracksheet$Project_status <- as.character(input$project_status)
    
    output$table_tracksheet <- renderRHandsontable({
      
      rhandsontable(data_tracksheet) %>%
        hot_cols(colWidths = 150)
    })
  })
  
  observe({
    
    if(!is.null(input$table_tracksheet)){
      
      dataValues$data <- as.data.frame(hot_to_r(input$table_tracksheet))
      
      output$table_tracksheet <- renderRHandsontable({
        rhandsontable(dataValues$data, height = "100%")
      })
    }
  })
  

  
 
}

shinyApp(ui, server)

