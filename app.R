library(shiny)
library(DT)
library(shinyjs)

## Functions
new.entry = function() {
  list(
    id = digest::digest(Sys.time(), algo = 'crc32'),
    name = 'Vacuum Pump',
    description = 'A pump.',
    amount = '2',
    location = list(
      room = NA,
      box = NA,
      pos = NA
    ),
    history = list()
  )
}

update.entry = function(entry, message) {
  entry$history = c(paste0(Sys.time(), ': ', message),
                    entry$history)
  
  entry
}

export = function(inventory) {
  res = data.frame(id = rep(NA, length(inventory)),
                   name = NA,
                   amount = NA,
                   history = NA
  )
  
  for (i in 1:length(inventory)) {
    res$id[i] = inventory[[i]]$id
    res$name[i] = inventory[[i]]$name
    res$amount[i] = inventory[[i]]$amount
    #res$history[i] = length(inventory[[i]]$history)
  }
  
  res
}

#Label mandatory fields
labelMandatory = function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS = ".mandatory_star { color: red; }"


## Setup (one time)
#entry = new.entry()
#entry = update.entry(entry, 'Init.')
#entry = update.entry(entry, 'Testing')
#saveRDS(entry, file = 'inventory.rds')





## ui
ui = fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      h2("Sidebar")
    ),
    mainPanel = mainPanel(
      column(
        width = 9,
        fluidRow(
          actionButton("add_button", "Add", icon("plus")),
          actionButton("edit_button", "Edit", icon("edit")),
          actionButton("copy_button", "Copy", icon("copy")),
          actionButton("delete_button", "Delete", icon("trash-alt"))
        ),
        br(),
        fluidRow(width="100%",
                 dataTableOutput("responses_table", width = "100%")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  #load responses_df and make reactive to inputs  
  responses_df <- reactive({
    
    #make reactive to
    input$submit
    input$submit_edit
    input$copy_button
    input$delete_button
    
    dbReadTable(pool, "responses_df")
    
  })  
  
  #List of mandatory fields for submission
  fieldsMandatory <- c("id", "name")
  
  #define which input fields are mandatory 
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  #Form for data entry
  entry_form <- function(button_id){
    
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("100px", "200px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("id", labelMandatory("ID"), placeholder = digest::digest(Sys.time(), algo = 'crc32')),
                  textInput("name", labelMandatory("Name"), placeholder = ""),
                  textInput("quantity", labelMandatory("Quantity"), placeholder = "")
                ),
                textAreaInput("note", "Note", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit"),
                htmlOutput('history')
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  inventory = reactive({
    invalidateLater(10*1e3)
    readRDS('inventory.rds')
  })
  
  #
  fieldsAll <- c("id", "name", "quantity", "note")
  
  #save form data into data_frame format
  formData <- reactive({
    
    formData <- data.frame(id = input$id,
                           name = input$name,
                           quantity = input$quantity,
                           note = input$note,
                           datetime = Sys.time(),
                           stringsAsFactors = FALSE)
    return(formData)
    
  })
  
  #Add data
  appendData <- function(data){
    
    new = new.entry()
    new$id = data$id
    new$name = data$name
    new$amount = data$quantity
    
    inv = inventory()
    inv[[length(inv)+1]] = new
    saveRDS(inv, 'inventory.rds')
  }
  
  observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
    
  })
  
  observeEvent(input$submit, priority = 20,{
    
    appendData(formData())
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  #delete data
  deleteData <- reactive({
    
    inv = inventory()
    inv[[input$responses_table_rows_selected]] = NULL
    saveRDS(inv, 'inventory.rds')
    
  })
  
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      deleteData()
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ), easyClose = TRUE
        )
      })
  })
  
  copyData <- reactive({
    
    inv = inventory()
    k = input$responses_table_rows_selected
    
    if (length(k) > 0) {
      for (kk in k) {
        inv[[length(inv) + 1]] = inv[[kk]] 
      }
      saveRDS(inv, 'inventory.rds')
    }
  })
  
  observeEvent(input$copy_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      copyData()
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
    
  })
  
  #edit data
  observeEvent(input$edit_button, priority = 20,{
    
    inv = inventory()
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })  
    
    if(length(input$responses_table_rows_selected) == 1 ){
      
      entry_form("submit_edit")
      
      updateTextInput(session, "id", value = inv[[input$responses_table_rows_selected]]$id)
      updateTextInput(session, "name", value = inv[[input$responses_table_rows_selected]]$name)
      updateTextInput(session, "quantity", value = inv[[input$responses_table_rows_selected]]$amount)
    }
    
  })
  
  observeEvent(input$submit_edit, priority = 20, {
    
    inv = inventory()
    inv[[input$responses_table_row_last_clicked]]$id = input$id
    inv[[input$responses_table_row_last_clicked]]$name = input$name
    inv[[input$responses_table_row_last_clicked]]$amount = input$quantity
    
    removeModal()
    
  })
  
  
  output$responses_table <- DT::renderDataTable({
    
    export(inventory())
    
  })
  
  output$history = renderUI({
    inv = inventory()
    message('UI')
    hist = inv[[input$responses_table_row_last_clicked]]$history
    message('UI2')
    HTML(paste0(hist, collapse = '<br />'))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
