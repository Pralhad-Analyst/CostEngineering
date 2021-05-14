library(shiny)
library(rhandsontable)
library(shinyFeedback)
library(shinyBS)
library(shinyWidgets)
library(promises)
library(future)

plan(multisession)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }
          #error { color: red; }"

track_UI_1 <- function(id) {
  ns = NS(id)
  
  wellPanel(
    style = "background: #ffffff",
    fluidRow(
      column(12,
             
             fluidRow(column(3,
                             h3("Tracksheet Table")),
                      column(
                        1,
                        offset = 8,
                        dropdown(
                          icon = icon("plus"),
                          right = T,
                          label = "",
                          width = "350px",
                          selectInput(
                            ns("datatable"),
                            "Select Datatable",
                            choices = c(
                              "Group",
                              "Commodity",
                              "Category",
                              "Truck",
                              "Status",
                              "Requestor",
                              "Cost Analyst",
                              "Region",
                              "Landed Region",
                              "Supplier Name"
                            )
                          ),
                          textInput(ns("text"), "Add Value"),
                          actionButton(ns("add"), "Submit")
                        )
                      )),
             
             fluidRow(column(
               12,
               helpText("Guidelines :"),
               helpText(h6("1. Please save the changes before exiting application")),
               helpText(h6("2. Date format: YYYY/MM/DD")),
               helpText(h6("3. Please don't add blank rows"))
             )), 
             
             tags$hr(),
             
             fluidRow(
               column(
                 2,
                 fileInput(
                   ns("upload"),
                   "",
                   multiple = F,
                   accept = ".csv",
                   placeholder = ".csv format only",
                   buttonLabel = "Upload.."
                 )
               ),
               
               column(
                 1, 
                 numericInput(
                   ns("rows"), 
                   labelMandatory("#Rows"), 
                   value = 10
                 )),
               bsTooltip(
                 ns("rows"),
                 "Select no. of rows to be added from uploaded file..",
                 "bottom",
                 options = list(container = "body")
               ),
               
               column(
                 1,
                 actionButton(ns("submit"), "Submit", style = "color: #fff; background-color: #26B99A; margin : 16px;border-color: #26B99A")
               ),
               
               column(
                 1,
                 actionButton(ns("save"), "Save", style = "color: #fff; background-color: #007BFF; margin : 16px; border-color: #007BFF")
               )
             ), 
             
             
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             
             fluidRow(column(
               12,
               addSpinner(rHandsontableOutput(ns("cost_table")), spin = "bounce", color = "#bfbfbb")
             ))
      )
    ))
}


track_function <- function(input, output, session) {
  ns = session$ns
  
  observe({
    shinyjs::toggleState(id = "submit", !is.null(input$rows) &&
                           input$rows != "" && input$rows > 0)})
  
  reactive_data_tracksheet <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/cost_tracker1.RDS', readRDS)
  reactive_data_group <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_group.RDS', readRDS)
  reactive_data_commodity <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_commodity.RDS', readRDS)
  reactive_data_category <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_category.RDS', readRDS)
  reactive_data_Truck <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_Truck.RDS', readRDS)
  reactive_data_status <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_status.RDS', readRDS)
  reactive_data_req <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_req.RDS', readRDS)
  reactive_data_own <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_own.RDS', readRDS)
  reactive_data_reg <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_reg.RDS', readRDS)
  reactive_data_land <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_land.RDS', readRDS)
  reactive_data_sup <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_sup.RDS', readRDS)
  reactive_data_curr <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_currency.RDS', readRDS)
  reactive_data_dep <-
    reactiveFileReader(2000, session, '/srv/shiny-server/ce/database/data_depart.RDS', readRDS)
  
  
  observeEvent(input$add, {
    if (input$datatable == "Group") {
      data_group <- c(input$text, reactive_data_group())
      data_group <- unique(data_group)
      saveRDS(data_group, file = "/srv/shiny-server/ce/database/data_group.RDS")
    } else if (input$datatable == "Commodity") {
      data_commodity <- c(input$text, reactive_data_commodity())
      data_commodity <- unique(data_commodity)
      saveRDS(data_commodity, file = "/srv/shiny-server/ce/database/data_commodity.RDS")
    } else if (input$datatable == "Category") {
      data_category <- c(input$text, reactive_data_category())
      data_category <- unique(data_category)
      saveRDS(data_category, file = "/srv/shiny-server/ce/database/data_category.RDS")
    } else if (input$datatable == "Truck") {
      data_Truck <- c(input$text, reactive_data_Truck())
      data_Truck <- unique(data_Truck)
      saveRDS(data_Truck, file = "/srv/shiny-server/ce/database/data_Truck.RDS")
    } else if (input$datatable == "Status") {
      data_status <- c(input$text, reactive_data_status())
      data_status <- unique(data_status)
      saveRDS(data_status, file = "/srv/shiny-server/ce/database/data_status.RDS")
    } else if (input$datatable == "Requestor") {
      data_req <- c(input$text, reactive_data_req())
      data_req <- unique(data_req)
      saveRDS(data_req, file = "/srv/shiny-server/ce/database/data_req.RDS")
    } else if (input$datatable == "Cost Analyst") {
      data_own <- c(input$text, reactive_data_own())
      data_own <- unique(data_own)
      saveRDS(data_own, file = "/srv/shiny-server/ce/database/data_own.RDS")
    } else if (input$datatable == "Currency") {
      data_curr <- c(input$text, reactive_data_curr())
      data_curr <- unique(data_curr)
      saveRDS(data_curr, file = "/srv/shiny-server/ce/database/data_currency.RDS")
    } else if (input$datatable == "Region") {
      data_reg <- c(input$text, reactive_data_reg())
      data_reg <- unique(data_reg)
      saveRDS(data_reg, file = "/srv/shiny-server/ce/database/data_reg.RDS")
    } else if (input$datatable == "Landed Region") {
      data_land <- c(input$text, reactive_data_land())
      data_land <- unique(data_land)
      saveRDS(data_land, file = "/srv/shiny-server/ce/database/data_land.RDS")
    } else if (input$datatable == "Department") {
      data_dep <- c(input$text, reactive_data_dep())
      data_dep <- unique(data_dep)
      saveRDS(data_dep, file = "/srv/shiny-server/ce/database/data_depart.RDS")
    } else  {
      data_sup <- c(input$text, reactive_data_sup())
      data_sup <- unique(data_sup)
      saveRDS(data_sup, file = "/srv/shiny-server/ce/database/data_sup.RDS")
    }
  })
  
  output$cost_table <- renderRHandsontable({
    data_group <- unique(reactive_data_group())
    data_commodity <- unique(reactive_data_commodity())
    data_category <- unique(reactive_data_category())
    data_Truck <- unique(reactive_data_Truck())
    data_status <- unique(reactive_data_status())
    data_req <- unique(reactive_data_req())
    data_own <- unique(reactive_data_own())
    data_reg <- unique(reactive_data_reg())
    data_land <- unique(reactive_data_land())
    data_sup <- unique(reactive_data_sup())
    data_dep <- unique(reactive_data_dep())
    data_curr <- unique(reactive_data_curr())
    
    rhandsontable(reactive_data_tracksheet(),
                  height = 600,
                  search = T) %>%
      
      hot_col(col = "Department",
              type = 'dropdown',
              source = data_dep) %>%
      hot_col(col = "CA_No", type = "autocomplete") %>%
      hot_col(col = "PN_No", type = "autocomplete") %>%
      hot_col(col = "Rev", type = "autocomplete") %>%
      hot_col(col = "Description", type = "autocomplete") %>%
      hot_col(col = "Ear", type = "autocomplete") %>%
      hot_col(col = "Group",
              type = "dropdown",
              source = data_group) %>%
      hot_col(col = "Commodity",
              type = "dropdown",
              source = data_commodity) %>%
      hot_col(col = "Category",
              type = "dropdown",
              source = data_category) %>%
      hot_col(col = "Truck",
              type = "dropdown",
              source = data_Truck) %>%
      hot_col(col = "Status",
              type = "dropdown",
              source = data_status) %>%
      hot_col(col = "Requestor",
              type = "dropdown",
              source = data_req) %>%
      hot_col(col = "Cost_Analyst",
              type = "dropdown",
              source = data_own) %>%
      hot_col(col = "Remark", type = "autocomplete") %>%
      hot_col(col = "Curr1",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Region1",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Landed_Region1",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Land_Curr1",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Curr2",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Region2",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Landed_Region2",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Land_Curr2",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Region3",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Curr3",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Landed_Region3",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Land_Curr3",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Supplier1",
              type = "dropdown",
              source = data_sup) %>%
      hot_col(col = "Quoted_Curr1",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Sup_Region1",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Sup_Landed_Region1",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Sup_Land_Curr1",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Supplier2",
              type = "dropdown",
              source = data_sup) %>%
      hot_col(col = "Quoted_Curr2",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Sup_Region2",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Sup_Landed_Region2",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Sup_Land_Curr2",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Supplier3",
              type = "dropdown",
              source = data_sup) %>%
      hot_col(col = "Quoted_Curr3",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Sup_Region3",
              type = "dropdown",
              source = data_reg) %>%
      hot_col(col = "Sup_Landed_Region3",
              type = "dropdown",
              source = data_land) %>%
      hot_col(col = "Sup_Land_Curr3",
              type = "dropdown",
              source = data_curr) %>%
      hot_col(col = "Perceive_achievable", type = "autocomplete") %>%
      
      hot_context_menu(allowRowEdit = T, allowColEdit = F) %>%
      hot_cols(fixedColumnsLeft = 3)
  })
  
  
  
  uploaded_table <- reactive({
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    req(file)
    req(ext == "csv", cancelOutput = T)
    a <- read.csv(file$datapath, nrows = input$rows)
    req(identical(colnames(a), colnames(reactive_data_tracksheet())), cancelOutput = T)
    
    a <- data.frame(
      Department = factor(a$Department, levels = unique(reactive_data_dep())),
      CA_No =as.character(a$CA_No),
      PN_No = as.character(a$PN_No),
      Rev = as.character(a$Rev),
      Description = as.character(a$Description),
      Ear = as.numeric(a$Ear),
      Group = factor(a$Group, levels = unique(reactive_data_group())),
      Commodity = factor(a$Commodity, levels = unique(reactive_data_commodity())),
      Category = factor(a$Category, levels = unique(reactive_data_category())),
      Truck = factor(a$Truck, levels = unique(reactive_data_Truck())),
      Request_Date = as.Date(a$Request_Date),
      Actual_Delivery = as.Date(a$Actual_Delivery),
      Project_Code = as.numeric(a$Project_Code),
      Status = factor(a$Status, levels = unique(reactive_data_status())),
      Requestor = factor(a$Requestor, levels = unique(reactive_data_req())),
      Cost_Analyst = factor(a$Cost_Analyst, levels = unique(reactive_data_own())),
      Remark = as.character(a$Remark),
      Exwork_Cost1 = as.numeric(a$Exwork_Cost1),
      Curr1 = factor(a$Curr1, levels = unique(reactive_data_curr())),
      Region1 = factor(a$Region1, levels = unique(reactive_data_reg())),
      Landed_Region1 = factor(a$Landed_Region1, levels = unique(reactive_data_land())),
      Freight1 = as.character(a$Freight1),
      Landed_Cost1 = as.numeric(a$Landed_Cost1),
      Land_Curr1 = factor(a$Land_Curr1, levels = unique(reactive_data_curr())),
      Exwork_Cost2 = as.numeric(a$Exwork_Cost2),
      Curr2 = factor(a$Curr2, levels = unique(reactive_data_curr())),
      Region2 = factor(a$Region2, levels = unique(reactive_data_reg())),
      Landed_Region2 = factor(a$Landed_Region2, levels = unique(reactive_data_land())),
      Freight2 = as.character(a$Freight2),
      Landed_Cost2 = as.numeric(a$Landed_Cost2),
      Land_Curr2 = factor(a$Land_Curr2, levels = unique(reactive_data_curr())),
      Exwork_Cost3 = as.numeric(a$Exwork_Cost3),
      Curr3 = factor(a$Curr3, levels = unique(reactive_data_curr())),
      Region3 = factor(a$Region3, levels = unique(reactive_data_reg())),
      Landed_Region3 = factor(a$Landed_Region3, levels = unique(reactive_data_land())),
      Freight3 = as.character(a$Freight3),
      Landed_Cost3 = as.numeric(a$Landed_Cost3),
      Land_Curr3 = factor(a$Land_Curr3, levels = unique(reactive_data_curr())),
      Supplier1 = factor(a$Supplier1, levels = unique(reactive_data_sup())),
      Sup_Exwork_Cost1 = as.numeric(a$Sup_Exwork_Cost1),
      Quoted_Curr1 = factor(a$Quoted_Curr1, levels = unique(reactive_data_curr())),
      Sup_Region1 = factor(a$Sup_Region1, levels = unique(reactive_data_reg())),
      Sup_Landed_Region1 = factor(a$Sup_Landed_Region1, levels = unique(reactive_data_land())),
      Sup_Freight1 = as.character(a$Sup_Freight1),
      Sup_Landed_Cost1 = as.numeric(a$Sup_Landed_Cost1),
      Sup_Land_Curr1 = factor(a$Sup_Land_Curr1, levels = unique(reactive_data_curr())),
      Supplier2 = factor(a$Supplier2, levels = unique(reactive_data_sup())),
      Sup_Exwork_Cost2 = as.numeric(a$Sup_Exwork_Cost2),
      Quoted_Curr2 = factor(a$Quoted_Curr2, levels = unique(reactive_data_curr())),
      Sup_Region2 = factor(a$Sup_Region2, levels = unique(reactive_data_reg())),
      Sup_Landed_Region2 = factor(a$Sup_Landed_Region2, levels = unique(reactive_data_land())),
      Sup_Freight2 = as.character(a$Sup_Freight2),
      Sup_Landed_Cost2 = as.numeric(a$Sup_Landed_Cost2),
      Sup_Land_Curr2 = factor(a$Sup_Land_Curr2, levels = unique(reactive_data_curr())),
      Supplier3 = factor(a$Supplier3, levels = unique(reactive_data_sup())),
      Sup_Exwork_Cost3 = as.numeric(a$Sup_Exwork_Cost3),
      Quoted_Curr3 = factor(a$Quoted_Curr3, levels = unique(reactive_data_curr())),
      Sup_Region3 = factor(a$Sup_Region3, levels = unique(reactive_data_reg())),
      Sup_Landed_Region3 = factor(a$Sup_Landed_Region3, levels = unique(reactive_data_land())),
      Sup_Freight3 = as.character(a$Sup_Freight3),
      Sup_Landed_Cost3 = as.numeric(a$Sup_Landed_Cost3),
      Sup_Land_Curr3 = factor(a$Sup_Land_Curr3, levels = unique(reactive_data_curr())),
      Year = as.numeric(a$Year),
      Potential_Part_Level = as.numeric(a$Potential_Part_Level),
      Perceive_achievable = as.numeric(a$Perceive_achievable),
      Potential_Project_Level = as.numeric(a$Potential_Project_Level),
      Actual_Part_Level = as.numeric(a$Actual_Part_Level),
      Actual_Project_Level = as.numeric(a$Actual_Project_Level),
      No_of_Parts = as.numeric(a$No_of_Parts),
      stringsAsFactors = F
    )
    
  })
  
  observeEvent(input$submit, {
    req(input$upload, input$rows)
    updated_track <-
      rbind(uploaded_table(), reactive_data_tracksheet())
    
    future({
      saveRDS(updated_track, "/srv/shiny-server/ce/database/cost_tracker1.RDS")
    })
    showNotification("Uploading file..", type = "default", duration = 6)
    Sys.sleep(4)
    showNotification("New Part No. Added", type = "warning", duration = 5)
  })
  
  
  observeEvent(input$save, {
    d <- hot_to_r(input$cost_table)
    
    future({
      saveRDS(d, "/srv/shiny-server/ce/database/cost_tracker1.RDS", ascii = T)
    })
    
    showNotification("Storing the data...", type = "warning", duration = 6)
    Sys.sleep(4)
    showNotification("Saved Changes Successfully..", type = "message", duration = 5)
    
  })
  
}


ui <- navbarPage(
  "Cost Engineering",
  shiny::tabPanel(
    "Tracksheet",
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shinyjs::inlineCSS(appCSS),
    track_UI_1("tk")
  )
)

server <- function(input, output, session) {
  callModule(track_function, "tk")
}

shinyApp(ui, server)