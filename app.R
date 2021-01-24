library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rhandsontable)

data_gdms <- readRDS("D:/Pralhad/git_CostEngineering/gdms_data.RDS")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}



fieldsMandatory <- c("project_pn", "project_rev",  "project_status")

appCSS <- ".mandatory_star { color: red; }
          #error { color: red; }"

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


input_tracksheet_UI <- function(id) {
  ns = NS(id)
  
  wellPanel(style = "background: #ffffff",
    fluidRow(
      column(12,
             h3("Tracksheet Fillup Form"),
             tags$br(),
             
             fluidRow(
               column(1, selectInput(ns("gdms"), "GDMS?", choices = c("Yes", "No"))),
               column(1, textInput(ns("project_pn"), labelMandatory("Part No"))),
               column(1, textInput(ns("project_rev"), labelMandatory("Rev."))),
               column(1, textInput(ns("project_description1"), "Descr.1")),
               column(2, textInput(ns("project_description2"), "Descr.2")),
               column(1, textInput(ns("project"), "Project")),
               column(1, textInput(ns("project_group"), "Group Code")),
               column(1, textInput(ns("part_commodity"), "Commodity")),
               column(1, textInput(ns("part_subcommodity"), "Sub-Com.")),
               column(1, textInput(ns("project_requestor"), "Requestor")),
               column(1, textInput(ns("project_owner"), "Project Owner"))
             ),
             
             fluidRow(
               column(2, dateInput(ns("date_request"), labelMandatory("Request Date"), format = "yyyy-mm-dd")),
               column(2, dateInput(ns("date_estimate"), labelMandatory("Estimated Delivery"), format = "yyyy-mm-dd")),
               column(2, dateInput(ns("date_actual"), labelMandatory("Actual Delivery Date"), format = "yyyy-mm-dd")),
               column(1, numericInput(ns("leadtime"), "Lead Time", value = "")),
               column(1, numericInput(ns("delays"), "Delays", value = "")),
               column(1, numericInput(ns("gdms_ear"), "GDMS EAR", value = "")),
               column(1, numericInput(ns("calc_ear"), "Calc Ear", value = "")),
               column(1, textInput(ns("project_plant"), "Plant")),
               column(1, textInput(ns("ca_no"), "CA No."))
             ),
             
             fluidRow(
               column(1, textInput(ns("supplier_region"), "Supplier Reg.")),
               column(1, numericInput(ns("supplier_exworks"), "Supplier Exw.", value = "")),
               column(1, textInput(ns("supplier_currency"), "Currency")),
               column(1, numericInput(ns("supplier_logistics"), "Logistics", value = "")),
               column(2, numericInput(ns("supplier_landedcost"), "Supplier Landed Cost", value = "")),
               column(1, textInput(ns("supplier_landedcurrency"), "Currency")),
               column(2, textInput(ns("supplier_name"), "Supplier Name"))
             ),
             
             fluidRow(
               column(1, textInput(ns("region1"), "Calc Region1")),
               column(1, numericInput(ns("exworks1"), "Exwork Cost1", value = "")),
               column(1, textInput(ns("currency1"), "Currency1")),
               column(1, numericInput(ns("logistics1"), "Logistic Cost1", value = "")),
               column(2, numericInput(ns("landedcost1"), "Landed Cost1", value = "")),
               column(1, textInput(ns("landedcurrency1"), "Currency1")),
               column(2, textInput(ns("analysis_type"), "Analysis Type"))
             ),
             
             fluidRow(
               column(1, textInput(ns("region2"), "Calc Region2")),
               column(1, numericInput(ns("exworks2"), "Exwork Cost2", value = "")),
               column(1, textInput(ns("currency2"), "Currency2")),
               column(1, numericInput(ns("logistics2"), "Logistic Cost2", value = "")),
               column(2, numericInput(ns("landedcost2"), "Landed Cost2", value = "")),
               column(1, textInput(ns("landedcurrency2"), "Currency2"))
             ),
             
             fluidRow(
               column(1, textInput(ns("region3"), "Calc Region3")),
               column(1, numericInput(ns("exworks3"), "Exwork Cost3", value = "")),
               column(1, textInput(ns("currency3"), "Currency3")),
               column(1, numericInput(ns("logistics3"), "Logistic Cost3", value = "")),
               column(2, numericInput(ns("landedcost3"), "Landed Cost3", value = "")),
               column(1, textInput(ns("landedcurrency3"), "Currency3")),
               column(2, textAreaInput(ns("project_comment"), "Comments"))
             ),
             
             fluidRow(
               column(1, textInput(ns("project_status"), labelMandatory("Status"))),
               column(2, numericInput(ns("potential"), "Total Potential Savings", value = "")),
               column(2, textInput(ns("potential_currency"), "Potential Currency")),
               column(2, numericInput(ns("actual_saving"), "Actual Savings", value = "")),
               column(2, textInput(ns("actual_currency"), "Actual Currency"))
             ),
             
             fluidRow(
               
               column(1,
                      actionButton(ns("new"), "Add new Part No.", style = "color: #fff; background-color: #007BFF; border-color: #2e6da4")
               )
             ),
             tags$hr(),
             
             fluidRow(
               column(12, h4("Tracksheet Table"))),
               
             fluidRow(
               column(12, helpText("Note : Please Save the Changes before leaving the application"))),
               
               tags$br(),
             fluidRow(
               column(1, actionButton(ns("save"), "Save", style = "color: #fff; background-color: #26B99A; border-color: #26B99A"))
               
             ),
             
             tags$br(),
             
             fluidRow(column(12, addSpinner(rHandsontableOutput(ns("tracksheet")), spin = "bounce", color = "#bfbfbb"))),
             
             tags$hr(),
             
             fluidRow(column(2, offset = 10, h6("Cost Engineering Department"))
             )
      )
    )
  )
  
}

tracksheet_function <- function(input, output, session){
  
  observe({        
    
    shinyjs::toggleState(id = "new", !is.null(input$project_pn) && input$project_pn != "" && as.character(input$date_request) != "" && as.character(input$date_estimate) != "" && as.character(input$date_actual) != "" && !is.null(as.character(input$date_estimate)) && !is.null(as.character(input$date_request)) && !is.null(as.character(input$date_actual)) && !is.null(input$project_status) && input$project_status != "")
    shinyjs::toggleState(id = "update", !is.null(input$project_pn) && input$project_pn != "" && as.character(input$date_request) != "" && as.character(input$date_estimate) != "" && as.character(input$date_actual) != "" && !is.null(as.character(input$date_estimate)) && !is.null(as.character(input$date_request)) && !is.null(as.character(input$date_actual)) && !is.null(input$project_status) && input$project_status != "")
    
  })
  
  rec_master <- reactiveFileReader(2000, NULL, 'D:/Pralhad/git_CostEngineering/master.RDS', readRDS)
  
  
  reactValues_data <- reactiveVal(master)
  
  ## Reactive GDMS data
  
  reactive_data <- reactive({
    
    req(input$project_pn, input$project_rev)
    
    dplyr::filter(data_gdms, PartNumber == input$project_pn & Revision == input$project_rev)
  })
  
  
  observeEvent(reactive_data(), {
    
    gdms <- reactive_data()
    
    descr1 <- as.character(gdms$DescriptionLine1[1])
    
    updateTextInput(session, "project_description1", value = descr1)
    
    descr2 <- as.character(gdms$DescriptionLine2[1])
    
    updateTextInput(session, "project_description2", value = descr2)
    
    proj <- as.character(gdms$ReleasingProjectNumber[1])
    
    updateTextInput(session, "project", value = proj)
    
    group <- as.character(gdms$PSI_GroupCode[1])
    
    updateTextInput(session, "project_group", value = group)
    
    com <- as.character(gdms$PSI_Commodity[1])
    
    updateTextInput(session, "part_commodity", value = com)
    
    subcom <- as.character(gdms$PSI_SubCommodity[1])
    
    updateTextInput(session, "part_subcommodity", value = subcom)
    
    exwork1 <- as.character(gdms$PSI_ShouldCost1ExWorks[1])
    
    updateNumericInput(session, "exworks1", value = exwork1)
    
    curr1 <- as.character(gdms$PSI_ShouldCost1Currency[1])
    
    updateTextInput(session, "currency1", value = curr1)
    
    reg1 <- as.character(gdms$PSI_ShouldCost1Region[1])
    
    updateTextInput(session, "region1", value = reg1)
    
    exwork2 <- as.character(gdms$PSI_ShouldCost2ExWorks[1])
    
    updateNumericInput(session, "exworks2", value = exwork2)
    
    curr2 <- as.character(gdms$PSI_ShouldCost2Currency[1])
    
    updateTextInput(session, "currency2", value = curr2)
    
    reg2 <- as.character(gdms$PSI_ShouldCost2Region[1])
    
    updateTextInput(session, "region2", value = reg2)
    
    exwork3 <- as.character(gdms$PSI_ShouldCost3ExWorks[1])
    
    updateNumericInput(session, "exworks3", value = exwork3)
    
    curr3 <- as.character(gdms$PSI_ShouldCost3Currency[1])
    
    updateTextInput(session, "currency3", value = curr3)
    
    reg3 <- as.character(gdms$PSI_ShouldCost3Region[1])
    
    updateTextInput(session, "region3", value = reg3)
    
  })
  
  
  observeEvent(input$new, {
    
    tracksheet_neww <- data.frame(GDMS = as.factor(input$gdms),
                                  PN_No = as.character(input$project_pn),
                                  Rev = as.character(input$project_rev),
                                  Descr1 = as.character(input$project_description1),
                                  Descr2 = as.character(input$project_description2),
                                  Project = as.character(input$project),
                                  Group = as.character(input$project_group),
                                  Commodity = as.character(input$part_commodity),
                                  Sub_Commodity = as.character(input$part_subcommodity),
                                  Requestor = as.character(input$project_requestor),
                                  Owner = as.character(input$project_owner),
                                  Request_Date = input$date_request,
                                  Estimated_Delivery = input$date_estimate,
                                  Actual_Delivery = input$date_actual,
                                  Lead_Time = as.numeric(input$leadtime),
                                  Delays = as.numeric(input$delays),
                                  GDMS_Ear = as.numeric(input$gdms_ear),
                                  Calc_Ear = as.numeric(input$calc_ear),
                                  Plant = as.character(input$project_plant),
                                  CA_No = as.character(input$ca_no),
                                  Supplier_Region = as.character(input$supplier_region),
                                  Supplier_Exworks = as.numeric(input$supplier_exworks),
                                  Supplier_Currency = as.character(input$supplier_currency),
                                  Supplier_Logistics = as.numeric(input$supplier_logistics),
                                  Supplier_LandedCost = as.numeric(input$supplier_landedcost),
                                  Supplier_LandedCurrency = as.character(input$supplier_landedcurrency),
                                  Supplier_Name = as.character(input$supplier_name),
                                  Calc_Region1 = as.character(input$region1),
                                  Calc_Exworks1 = as.numeric(input$exworks1),
                                  Calc_Currency1 = as.character(input$currency1),
                                  Calc_Logistics1 = as.numeric(input$logistics1),
                                  Calc_LandedCost1 = as.numeric(input$landedcost1),
                                  Calc_LandedCurrency1 = as.character(input$landedcurrency1),
                                  Analysis_Type = as.character(input$analysis_type),
                                  Calc_Region2 = as.character(input$region2),
                                  Calc_Exworks2 = as.numeric(input$exworks2),
                                  Calc_Currency2 = as.character(input$currency2),
                                  Calc_Logistics2 = as.numeric(input$logistics2),
                                  Calc_LandedCost2 = as.numeric(input$landedcost2),
                                  Calc_LandedCurrency2 = as.character(input$landedcurrency2),
                                  Calc_Region3 = as.character(input$region3),
                                  Calc_Exworks3 = as.numeric(input$exworks3),
                                  Calc_Currency3 = as.character(input$currency3),
                                  Calc_Logistics3 = as.numeric(input$logistics3),
                                  Calc_LandedCost3 = as.numeric(input$landedcost3),
                                  Calc_LandedCurrency3 = as.character(input$landedcurrency3),
                                  Project_Comment = as.character(input$project_comment),
                                  Project_Status = as.character(input$project_status),
                                  Potential_Savings = as.numeric(input$potential),
                                  Potential_Currency = as.character(input$potential_currency),
                                  Actual_Savings = as.numeric(input$actual_saving),
                                  Actual_Currency = as.character(input$actual_currency),
                                  Time = humanTime()
    )
    
    reactValues_data <- rbind(tracksheet_neww, rec_master())
    
    saveRDS(reactValues_data, "D:/Pralhad/git_CostEngineering/master.RDS", ascii = T)
    
    showNotification("New Part No. Added", type = "warning")
    
  })
  
  output$tracksheet <- renderRHandsontable({
    
    rhandsontable(rec_master(), height = 600) %>%
      hot_col(col = "GDMS", type = 'dropdown', source = c("Yes", "No")) %>%
      hot_col(col = "PN_No", readOnly = T) %>%
      hot_col(col = "Rev", readOnly = T) %>%
      hot_col(col = "Time", readOnly = T) %>%
      hot_context_menu(allowRowEdit = F, allowColEdit = F) 
    
  })
  
   
   
   observeEvent(input$save, {
       
       d <- as.data.frame(hot_to_r(input$tracksheet)) 
       
       saveRDS(d, "D:/Pralhad/git_CostEngineering/master.RDS", ascii = T)
       
       showNotification("Changes Saved Successfully", type = "message")
    
     
   })
  
}



ui <- navbarPage(
  
  "Cost Engineering",
  
  shiny::tabPanel("Tracksheet",
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  input_tracksheet_UI("tk")
                  
  )
)

server <- function(input, output, session){
  
  callModule(tracksheet_function, "tk")
  
}

shinyApp(ui, server)