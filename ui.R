library(shiny)

ui <- fluidPage(
  titlePanel("VITASPEC NIRS prediction"),
  
  # Ajout de l'encart d'informations sur les fichiers
  fluidRow(
    column(12,
           wellPanel(
             uiOutput("infos_fichiers_ui")
           )
    )
  ),
  
  # Les tableaux et la distance (inchangés)
  fluidRow(
    column(8, tableOutput("table_biochimie")), # J'ai passé la colonne à 8 pour laisser plus de place au grand tableau
    column(4, uiOutput("distance_ui"))
  )
)