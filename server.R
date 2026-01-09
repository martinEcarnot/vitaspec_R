server <- function(input, output, session) {
  
  dossier = "/home/ecarnot/Documents/INRA/Projets/VitaSPEC/test_TR/"
  resultats <- reactiveVal(NULL)
  fichiers_traites <- reactiveVal(character())
  
  library(rchemo)
  source("asd2vitaspec.R")
  load("Vitaspec_fruit_models")
  param=read.csv("Vitaspec_fruit_param.csv")
  
  load("vitaspec_meso_24-25")
  fm_PCA <- pcasvd(sp$x, nlv = 6)

  
    observe({
      invalidateLater(2000)
      
      fichiers <- list.files(dossier, pattern = "\\.asd$", full.names = TRUE)
      if (length(fichiers) > length(fichiers_traites())) {
        
        nouveau <- setdiff(fichiers, fichiers_traites())
        fichiers_traites(fichiers)
        resultats(asd2vitaspec(nouveau[1],fm_PCA,fm_all, id_model,param))
      }
    })
    
    
  output$table_biochimie <- renderTable({
    req(resultats())
    data.frame(
      Parametre = names(resultats()$biochimie),
      Valeur    = resultats()$biochimie,
      row.names = NULL
    )
  })
  
  
  output$distance_ui <- renderUI({
    req(resultats())
    d <- resultats()$distance
    
    couleur <- if (d > 1.5) "red"
    else if (d > 1) "orange"
    else "green"
    
    tags$div(
      style = paste0(
        "font-size: 30px; font-weight: bold; color: ", couleur
      ),
      paste("Distance  :", round(d, 3))
    )
  })
  
}
