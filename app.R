data_tracksheet <- data.frame(
  GDMS = as.factor(""),
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
  Request_Date = as.Date(""),
  Estimated_Delivery = as.Date(""),
  Actual_Delivery = as.Date(""),
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
  Project_Status = as.character(""),
  Potential_Savings = as.numeric(""),
  Potential_Currency = as.character(""),
  Actual_Savings = as.numeric(""),
  Actual_Currency = as.character(""),
  Time = humanTime()
  
)


data_gdms <- readRDS("D:/Pralhad/git_CostEngineering/gdms_data.RDS")

master <- readRDS("D:/Pralhad/git_CostEngineering/master_data/master.RDS")


track_path <- file.path("D:", "Pralhad", "git_CostEngineering", "track_data", fsep = "/")

epochTime <- function() {
  as.integer(Sys.time())
}

saveData <- function(data, path){
  fileName <- sprintf("%s_%s.RDS", 
                      humanTime(),
                      digest::digest(data))
  
  saveRDS(data, file = file.path(path, fileName), ascii = T)
}


loadData <- function(path){
  files <- list.files(file.path(path), full.names = T)
  d <- purrr::map_df(files, readRDS)
  e <- dplyr::bind_rows(d, master)
  file.remove(files)
  saveRDS(e, "D:/Pralhad/git_CostEngineering/master_data/master.RDS")
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


input_tracksheet_UI <- function(id) {
  ns = NS(id)
  
 wellPanel(
   fluidRow(
     column(12,
            h3("Tracksheet Fillup Form"),
            tags$br(),
            
            fluidRow(
              column(1, selectInput(ns("gdms"), "GDMS?", choices = c("Yes", "No"))),
              column(1, textInput(ns("project_pn"), "Part No")),
              column(1, textInput(ns("project_rev"), "Rev.")),
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
              column(2, dateInput(ns("date_request"), "Request Date")),
              column(2, dateInput(ns("date_estimate"), "Estimated Delivery")),
              column(2, dateInput(ns("date_actual"), "Actual Delivery Date")),
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
              column(1, textInput(ns("project_status"), "Status")),
              column(2, numericInput(ns("potential"), "Total Potential Savings", value = "")),
              column(2, textInput(ns("potential_currency"), "Potential Currency")),
              column(2, numericInput(ns("actual_saving"), "Actual Savings", value = "")),
              column(2, textInput(ns("actual_currency"), "Actual Currency"))
            ),
            
            fluidRow(
              
              column(1,
                     actionButton(ns("new"), "Add new Part No.", style = "color: #fff; background-color: #007BFF; border-color: #2e6da4")
              ),
              column(1,
                     offset = 4,
                     actionButton(ns("update"), "Update Existing Part No.", style = "color: #fff; background-color: #26B99A; border-color: #26B99A")
              )
              
            )
            )
   )
 )
}


table_tracksheet_UI <- function(id){
  
  ns = NS(id)
  
  fluidRow(
    box(
      width = 12,
      title = "Tracksheet Table",
      actionButton(ns("refresh"), "Refresh"),
      DT::dataTableOutput(ns("tracksheet"))
    )
  )
}
   
      

tracksheet_function <- function(input, output, session){
  
  
  ns <- session$ns
  
  ## Reactive GDMS data
  
  reactive_data <- reactive({
    
    req(input$project_pn, input$project_rev)
    
    dplyr::filter(data_gdms, PartNumber == input$project_pn & Revision == input$project_rev)
  })
  
  reactive_data1 <- reactive({
    
    req(input$project_pn, input$project_rev)
    
    dplyr::filter(master, PN_No == input$project_pn & Rev == input$project_rev)
  })
  
  observeEvent(reactive_data(), {
    
    tracksheet <- reactive_data1()
    
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
    
    reqs <- as.character(tracksheet$Requestor[1])
    
    updateTextInput(session, "project_requestor", value = reqs)
    
    own <- as.character(tracksheet$Owner[1])
    
    updateTextInput(session, "project_owner", value = own)
    
    reqDate <- tracksheet$Request_Date[1]
    
    updateDateInput(session, "date_request", value = reqDate)
    
    estDate <- tracksheet$Estimated_Delivery[1]
    
    updateDateInput(session, "date_estimate", value = estDate)
    
    actDate <- tracksheet$Actual_Delivery[1]
    
    updateDateInput(session, "date_actual", value = actDate)
    
    leadTime <- as.character(tracksheet$Lead_Time[1])
    
    updateNumericInput(session, "leadtime", value = leadTime)
    
    delay <- as.character(tracksheet$Delays[1])
    
    updateNumericInput(session, "delays", value = delay)
    
    gdmsEar <- as.character(tracksheet$GDMS_Ear[1])
    
    updateTextInput(session, "gdms_ear", value = gdmsEar)
    
    calcEar <- as.character(tracksheet$Calc_Ear[1])
    
    updateNumericInput(session, "calc_ear", value = calcEar)
    
    plant <- as.character(tracksheet$Plant[1])
    
    updateTextInput(session, "project_plant", value = plant)
    
    ca <- as.character(tracksheet$CA_No[1])
    
    updateTextInput(session, "ca_no", value = ca)
    
    s_region <- as.character(tracksheet$Supplier_Region[1])
    
    updateTextInput(session, "supplier_region", value = s_region)
    
    s_exwork <- as.character(tracksheet$Supplier_Exworks[1])
    
    updateNumericInput(session, "supplier_exworks", value = s_exwork)
    
    s_curr <- as.character(tracksheet$Supplier_Currency[1])
    
    updateTextInput(session, "supplier_currency", value = s_curr)
    
    s_log <- as.character(tracksheet$Supplier_Logistics[1])
    
    updateTextInput(session, "supplier_logistics", value = s_log)
    
    s_land <- as.character(tracksheet$Supplier_LandedCost[1])
    
    updateNumericInput(session, "supplier_landedcost", value = s_land)
    
    s_landCurr <- as.character(tracksheet$Supplier_LandedCurrency[1])
    
    updateTextInput(session, "supplier_landedcurrency", value = s_landCurr)
    
    s_name <- as.character(tracksheet$Supplier_Name[1])
    
    updateTextInput(session, "supplier_name", value = s_name)
    
    log1 <- as.character(tracksheet$Calc_Logistics1[1])
    
    updateNumericInput(session, "logistics1", value = log1)
    
    land1 <- as.character(tracksheet$Calc_LandedCost1[1])
    
    updateNumericInput(session, "landedcost1", value = land1)
    
    landCurr1 <- as.character(tracksheet$Calc_LandedCurrency1[1])
    
    updateTextInput(session, "landedcurrency1", value = landCurr1)
    
    ana <- as.character(tracksheet$Analysis_Type[1])
    
    updateTextInput(session, "analysis_type", value = ana)
    
    log2 <- as.character(tracksheet$Calc_Logistics2[1])
    
    updateNumericInput(session, "logistics2", value = log2)
    
    land2 <- as.character(tracksheet$Calc_LandedCost2[1])
    
    updateNumericInput(session, "landedcost2", value = land2)
    
    landCurr2 <- as.character(tracksheet$Calc_LandedCurrency2[1])
    
    updateTextInput(session, "landedcurrency2", value = landCurr2)
    
    log3 <- as.character(tracksheet$Calc_Logistics3[1])
    
    updateNumericInput(session, "logistics3", value = log3)
    
    land3 <- as.character(tracksheet$Calc_LandedCost3[1])
    
    updateNumericInput(session, "landedcost3", value = land3)
    
    landCurr3 <- as.character(tracksheet$Calc_LandedCurrency3[1])
    
    updateTextInput(session, "landedcurrency3", value = landCurr3)
    
    comt <- as.character(gdms$PSI_ShouldCostNotesAndAssumptions[1])
    
    updateTextAreaInput(session, "project_comment", value = comt)
    
    stat <- as.character(tracksheet$Project_Status[1])
    
    updateTextInput(session, "project_status", value = stat)
    
    pot <- as.character(tracksheet$Potential_Savings[1])
    
    updateNumericInput(session, "potential", value = pot)
    
    potCurr <- as.character(tracksheet$Potential_Currency[1])
    
    updateTextInput(session, "potential_currency", value = potCurr)
    
    actSaving <- as.character(tracksheet$Actual_Savings[1])
    
    updateNumericInput(session, "actual_saving", value = actSaving)
    
    actCurr <- as.character(tracksheet$Actual_Currency[1])
    
    updateTextInput(session, "actual_currency", value = actCurr)
    
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
    
    saveData(tracksheet_neww, track_path)
  })
  
  observeEvent(input$update, {
    
    master$GDMS[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$gdms)
    
    master$Descr1[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_description1)
    
    master$Descr2[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_description2)
    
    master$Project[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project)
    
    master$Group[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_group)
    
    master$Commodity[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$part_commodity)
    
    master$Sub.Commodity[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$part_subcommodity)
    
    master$Requestor[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_requestor)
    
    master$Owner[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_owner)
    
    master$Request.Date[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- input$date_request
    
    master$Estimated.Delivery[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- input$date_estimate
    
    ## missed parameter
    
    master$Lead.Time[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$leadtime)
    
    master$Delays[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$delays)
    
    master$GDMS.Ear[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$gdms_ear)
    
    master$Calc.Ear[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$calc_ear)
    
    master$Plant[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$project_plant)
    
    master$CA.No[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$ca_no)
    
    master$Supplier.Region[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_region)
    
    master$Supplier.Exworks[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_exworks)
    
    master$Supplier.Currency[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_currency)
    
    master$Supplier.Logistics[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_logistics)
    
    master$Supplier.Landed.Cost[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_landedcost)
    
    master$Supplier.Landed.Currency[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_landedcurrency)
    
    master$Supplier.Name[match(
      interaction(input$project_pn, input$project_rev),
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$supplier_name)
    
    master$Calc.Region1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$region1)
    
    master$Calc.Exworks1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$exworks1)
    
    master$Calc.Currency1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$currency1)
    
    master$Calc.Logistics1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$logistics1)
    
    master$Calc.Landed.Cost1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcost1)
    
    master$Calc.Landed.Currency1[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcurrency1)
    
    master$Analysis.Type[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$analysis_type)
    
    master$Calc.Region2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$region2)
    
    master$Calc.Exworks2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$exworks2)
    
    master$Calc.Currency2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$currency2)
    
    master$Calc.Logistics2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$logistics2)
    
    master$Calc.Landed.Cost2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcost2)
    
    master$Calc.Landed.Currency2[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcurrency2)
    
    master$Calc.Region3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$region3)
    
    master$Calc.Exworks3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$exworks3)
    
    master$Calc.Currency3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$currency3)
    
    master$Calc.Logistics3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$logistics3)
    
    master$Calc.Landed.Cost3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcost3)
    
    master$Calc.Landed.Currency3[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character(input$landedcurrency3)
    
    master$Project.Comment[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$project_comment)
    
    master$Project.Status[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$project_status)
    
    master$Potential.Savings[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$potential)
    
    master$Potential.Currency[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$potential_currency)
    
    master$Actual.Savings[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$actual_saving)
    
    master$Actual.Currency[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- as.character( input$actual_currency)
    
    master$Updated.Time[match(
      interaction(input$project_pn, input$project_rev) ,
      interaction(master$PN_No, master$Rev)
    )] <<- humanTime()
    
    
    
    saveRDS(master, "D:/Pralhad/git_CostEngineering/master_data/master.RDS")
    
  })
  
  observeEvent(input$refresh,{
    
    loadData(track_path)
    
    master <- readRDS("D:/Pralhad/git_CostEngineering/master_data/master.RDS")
    
    output$tracksheet <- DT::renderDataTable(
      master,
      options = list(scrollX = TRUE),
      class = 'cell-border stripe'
    )
    
  })
  
}


library(shiny)
library(digest)
library(shinyjs)
library(purrr)
library(shinyalert)
library(DT)

ui <- navbarPage(
  "Cost Engineering",
  
  shiny::tabPanel("Tracksheet",
                  input_tracksheet_UI("tk"),
                  table_tracksheet_UI("tk"))
)
           

server <- function(input, output, session){
  
  callModule(tracksheet_function, "tk")
 
}

shinyApp(ui, server)





