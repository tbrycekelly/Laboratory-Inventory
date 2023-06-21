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
      room = '',
      box = '',
      pos = ''
    ),
    history = list()
  )
}

update.entry = function(entry, message) {
  entry$history = c(paste0(Sys.time(), ': ', message),
                    entry$history)
  
  entry
}

export = function(inv) {
  message(Sys.time(), ': Starting Export.')
  if (length(inv) == 0) {
    message(Sys.time(), ': Error: No inventory for Export!')
    inv = list(new.entry())
  }
  res = data.frame(id = rep(NA, length(inv)),
                   name = NA,
                   amount = NA,
                   location = NA,
                   history = NA
  )
  
  for (i in 1:length(inv)) {
    res$id[i] = inv[[i]]$id
    res$name[i] = inv[[i]]$name
    res$amount[i] = inv[[i]]$amount
    if (sum(!is.na(inv[[i]]$location))>0) {
      res$location[i] = paste(names(inv[[i]]$location)[!is.na(inv[[i]]$location)], inv[[i]]$location[!is.na(inv[[i]]$location)], collapse = '; ')
    }
    res$history[i] = length(inv[[i]]$history)
  }
  message(Sys.time(), ': Finishing Export.')
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
if (!file.exists('inventory.rds')) {
  entry = new.entry()
  entry = update.entry(entry, 'Init.')
  entry = update.entry(entry, 'Testing')
  saveRDS(list(entry, entry), file = 'inventory.rds')
}



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
          actionButton("edit_button", "Details", icon("edit")),
          actionButton("add_button", "Add", icon("plus")),
          actionButton("copy_button", "Copy", icon("copy")),
          actionButton("delete_button", "Delete", icon("trash-alt"))
        ),
        br(),
        fluidRow(width="100%", dataTableOutput("responses_table", width = "100%")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  #load responses_df and make reactive to inputs  
  responses_df <- reactive({
    message(Sys.time(), ': Update responses.')
    #make reactive to
    input$submit
    input$submit_edit
    input$copy_button
    input$delete_button
    
    export(inventory())
    
  })  
  
  #List of mandatory fields for submission
  fieldsMandatory <- c("id", 'name')
  
  #define which input fields are mandatory 
  observe({
    
    mandatoryFilled =
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
    Sys.sleep(0.2)
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:460px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("100px", "225px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("id", labelMandatory("ID"), value = digest::digest(Sys.time(), algo = 'crc32')),
                  textInput("name", labelMandatory("Name"), placeholder = ""),
                  textInput("quantity", "Quantity", placeholder = "")
                ),
                checkboxGroupInput('class', 'Category', c('Equipment', 'Consumable', 'Other'), inline = T),
                splitLayout(
                  cellWidths = c("100px", "150px", "150px"),
                  cellArgs = list(style = "vertical-align: top"),
                  selectInput('room', 'Location', c(NA, 'Irving2', 'Office', 'SMC', 'Field')),
                  textInput("box", 'Box', placeholder = ""),
                  textInput("pos", 'Position', placeholder = "")
                ),
                textAreaInput("desc", "Description", placeholder = "", height = 35, width = "354px"),
                textAreaInput("note", "Note", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit"),
                shiny::hr(),
                p('Revision history:'),
                htmlOutput('history')
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  inventory = reactive({
    message(Sys.time(), ': Loading inventory.')
    #invalidateLater(10*1e3)
    input$submit
    input$submit_edit
    input$copy_button
    input$delete_button
    input$add_button
    
    
    readRDS('inventory.rds')
  })
  
  #
  fieldsAll <- c("id", "name", "quantity", "note")
  
  #save form data into data_frame format
  formData <- reactive({
    message(Sys.time(), ': Received new data.')
    formData <- data.frame(id = input$id,
                           name = input$name,
                           quantity = input$quantity,
                           desc = input$desc,
                           note = input$note,
                           room = input$room,
                           box = input$box,
                           pos = input$pos,
                           datetime = Sys.time(),
                           stringsAsFactors = FALSE)
    return(formData)
    
  })
  
  #Add data
  appendData <- function(data){
    
    message(data$datetime, ': Appending data.')
    new = new.entry()
    new$id = data$id
    new$name = data$name
    new$description = data$desc
    new$amount = data$quantity
    new$location$room = data$room
    new$location$box = data$box
    new$location$pos = data$pos
    
    inv = inventory()
    inv[[length(inv)+1]] = new
    saveRDS(inv, 'inventory.rds')
  }
  
  observeEvent(input$add_button, priority = 20,{
    message(Sys.time(), ': Add Button.')
    entry_form("submit")
    
  })
  
  observeEvent(input$submit, priority = 20,{
    message(Sys.time(), ': Submit Button.')
    appendData(formData())
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  #delete data
  deleteData <- reactive({
    
    message(Sys.time(), ': Deleting data.')
    inv = inventory()
    inv[[input$responses_table_rows_selected]] = NULL
    saveRDS(inv, 'inventory.rds')
    
  })
  
  observeEvent(input$delete_button, priority = 20,{
    
    message(Sys.time(), ': Delete Button.')
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
    
    message(Sys.time(), ': Copying entry(s).')
    inv = inventory()
    k = input$responses_table_rows_selected
    
    if (length(k) > 0) {
      for (kk in k) {
        inv[[length(inv) + 1]] = inv[[kk]] 
        inv[[length(inv) + 1]]$id = digest::digest(Sys.time(), algo = 'crc32')
        inv[[kk]] = update.entry(inv[[kk]], paste0('Copied to new entry ', inv[[length(inv) + 1]]$id))
        inv[[length(inv) + 1]] = update.entry(inv[[length(inv) + 1]], paste0('Copied from old entry ', inv[[kk]]$id))
        
      }
      saveRDS(inv, 'inventory.rds')
    }
  })
  
  observeEvent(input$copy_button, priority = 20,{
    
    message(Sys.time(), ': Copy Button.')
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
    
    message(Sys.time(), ': Editing entry.')
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
      updateTextInput(session, "desc", value = inv[[input$responses_table_rows_selected]]$description)
      updateCheckboxGroupInput(session, 'class', selected = inv[[input$responses_table_rows_selected]]$class)
      
      updateSelectInput(session, "room", selected = inv[[input$responses_table_rows_selected]]$location$room)
      updateTextInput(session, "box", value = inv[[input$responses_table_rows_selected]]$location$box)
      updateTextInput(session, "pos", value = inv[[input$responses_table_rows_selected]]$location$pos)
    }
    
  })
  
  observeEvent(input$submit_edit, priority = 20, {
    message(Sys.time(), ': Submit Edit.')
    inv = inventory()
    if (inv[[input$responses_table_row_last_clicked]]$id != input$id) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed id from ', inv[[input$responses_table_row_last_clicked]]$id, ' to ', input$id))
      inv[[input$responses_table_row_last_clicked]]$id = input$id
    }
    if (inv[[input$responses_table_row_last_clicked]]$name != input$name) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed name from ', inv[[input$responses_table_row_last_clicked]]$name, ' to ', input$name))
      inv[[input$responses_table_row_last_clicked]]$name = input$name
    }
    if (inv[[input$responses_table_row_last_clicked]]$amount != input$quantity) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed amount from ', inv[[input$responses_table_row_last_clicked]]$amount, ' to ', input$quantity))
      inv[[input$responses_table_row_last_clicked]]$amount = input$quantity
    }
    if (inv[[input$responses_table_row_last_clicked]]$description != input$desc) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed description from ', inv[[input$responses_table_row_last_clicked]]$description, ' to ', input$desc))
      inv[[input$responses_table_row_last_clicked]]$description = input$desc
    }
    
    if (inv[[input$responses_table_row_last_clicked]]$location$room != input$room) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed description from ', inv[[input$responses_table_row_last_clicked]]$location$room, ' to ', input$room))
      inv[[input$responses_table_row_last_clicked]]$location$room = input$room
    }
    if (inv[[input$responses_table_row_last_clicked]]$location$box != input$box) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed description from ', inv[[input$responses_table_row_last_clicked]]$location$box, ' to ', input$box))
      inv[[input$responses_table_row_last_clicked]]$location$box = input$box
    }
    if (inv[[input$responses_table_row_last_clicked]]$location$pos != input$pos) {
      inv[[input$responses_table_row_last_clicked]] = update.entry(inv[[input$responses_table_row_last_clicked]], paste0('Changed description from ', inv[[input$responses_table_row_last_clicked]]$location$pos, ' to ', input$pos))
      inv[[input$responses_table_row_last_clicked]]$location$pos = input$pos
    }
    saveRDS(inv, 'inventory.rds')
    removeModal()
    
  })
  
  
  output$responses_table <- DT::renderDataTable({
    message(Sys.time(), ': Generting dataframe for inventory.')
    export(inventory())
    
  })
  
  output$history = renderUI({
    
    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$responses_table_rows_selected)) {
      inv = inventory()
      hist = inv[[input$responses_table_row_last_clicked]]$history
      return(HTML(paste0(hist, collapse = '<br />')))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
