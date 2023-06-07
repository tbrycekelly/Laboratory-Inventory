library(shiny)
library(DT)
library(jsonlite)
library(shinyBS)
library(DataEditR)


inventory = list()

load.inventory = function() {
  if (file.exists('data/inventory.rds')) {
    inventory = readRDS('data/inventory.rds')
  } else {
    message('No inventory file found, initalizing new inventory.')
    inventory = list(new.entry())
  }
}

backup.inventory = function(inventory) {
  stamp = gsub(':', '', Sys.time())
  saveRDS(inventory, paste0('data/inventory ', stamp, '.backup'))
  jsonlite::write_json(inventory, paste0('data/inventory ', stamp, '.json'))

  invisible()
}

new.entry = function() {
  list(
    id = 1,
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
    res$history[i] = length(inventory[[i]]$history)
  }
  
  res
}

print.item = function(item) {
  paste0('Item: ', item$name, '\n',
         '\tid:\t', item$id, '\n',
         '\tamount:\t', item$amount, '\n',
         '\thistory:', paste(item$history, collapse = '\n\t'), '\n')
}

inventory = load.inventory()

server = function(input, output, session) {
  
  current = reactive({
    message(str(inventory))
    export(inventory)
  })
  
  output$inventory.table = DT::renderDataTable({
    datatable(export(inventory), selection = 'single')
    }, server = FALSE)
  
  # highlight selected rows in the scatterplot
  output$item = DT::renderDT({
    s = input$inventory.table_rows_selected
    
    if (length(s) == 0) {
      entry = new.entry()
    } else {
      entry = inventory[[s[1]]]
    }
    
    output = data.frame(name = entry$name,
                        id = entry$id,
                        amount = entry$amount)
    
    DT::datatable(output, editable = T)
  })
  
  # server-side processing
  mtcars2 = mtcars[, 1:8]
  output$x3 = DT::renderDataTable(mtcars2, server = TRUE)
  
  # print the selected indices
  output$x4 = renderPrint({
    s = input$x3_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  
  data.edit = dataEditServer('edit',
                             data = current)
  
  dataOutputServer('data.output',
                   data = data.edit)
  
  
}