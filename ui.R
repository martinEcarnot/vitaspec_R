library(shiny)

ui <- fluidPage(
titlePanel("VITASPEC NIRS prediction"),
  
  fluidRow(
    column(6, tableOutput("table_biochimie")),
    column(6, uiOutput("distance_ui"))
  )
)
