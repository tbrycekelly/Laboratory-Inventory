library(shiny)

ui = fluidPage(
  
  titlePanel('Laboratory Inventory System'),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      h2("Sidebar")
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        type = 'tabs',
        tabPanel(
          title = 'Overview',
          fluidRow(
            column(
              width = 4,
              h3('Inventory'),
              DT::dataTableOutput('inventory.table'),
              dataEditUI('edit'),
              dataOutputUI('data.output')
            ),
            column(
              width = 8,
              h3('Item Detail'),
              DT::DTOutput('item')
          )
        ),
        tabPanel(
          title = 'History'
        )
      )
    )
  )
)
)

