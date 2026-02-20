server <- function(input, output, session) {
  
  d0 = "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/"
  dossier = paste0(d0,"test_TR/")
  
  resultats <- reactiveVal(NULL)
  fichiers_traites <- reactiveVal(character())
  
  fruit_en_cours <- reactiveVal("")
  historique_biochimie <- reactiveVal(data.frame())
  nom_fichier_actuel <- reactiveVal("")
  liste_fichiers_fruit <- reactiveVal(character())
  
  library(rchemo)
  source("asd2vitaspec.R")
  load("Vitaspec_fruit_models_2")
  param = read.csv("Vitaspec_fruit_param.csv")
  
  load("vitaspec_meso_24-25")
  fm_PCA <- pcasvd(sp$x, nlv = 6)
  
  observe({
    invalidateLater(2000)
    
    fichiers <- list.files(dossier, pattern = "\\.asd$", full.names = TRUE)
    
    # 1. On utilise isolate() pour lire les fichiers traités sans relancer l'observe
    traites <- isolate(fichiers_traites()) 
    
    if (length(fichiers) > length(traites)) {
      
      nouveau <- setdiff(fichiers, traites)
      
      # 2. On prend UNIQUEMENT le premier fichier de la file d'attente
      fichier_cible <- nouveau[1]
      
      # On marque SEULEMENT ce fichier comme traité (les autres attendront les prochaines 2 secondes)
      fichiers_traites(c(traites, fichier_cible))
      
      nom_fichier <- basename(fichier_cible)
      nom_fichier_actuel(nom_fichier)
      
      num_fruit <- strsplit(nom_fichier, "-")[[1]][1]
      
      # Prédiction
      res <- asd2vitaspec(fichier_cible, fm_PCA, fm_all, id_model, param)
      resultats(res)
      
      # 3. On isole la lecture de l'historique pour éviter les boucles infinies de réactivité
      fruit_actuel <- isolate(fruit_en_cours())
      hist_actuel <- isolate(historique_biochimie())
      liste_actuelle <- isolate(liste_fichiers_fruit())
      
      if (num_fruit != fruit_actuel) {
        # Nouveau fruit
        fruit_en_cours(num_fruit)
        historique_biochimie(as.data.frame(t(res$biochimie)))
        liste_fichiers_fruit(c(nom_fichier))
      } else {
        # Même fruit, on ajoute à l'historique
        historique_biochimie(rbind(hist_actuel, as.data.frame(t(res$biochimie))))
        liste_fichiers_fruit(c(liste_actuelle, nom_fichier))
      }
    }
  })
  
  output$infos_fichiers_ui <- renderUI({
    req(nom_fichier_actuel())
    
    fichiers <- liste_fichiers_fruit()
    nb_fichiers <- length(fichiers)
    
    HTML(paste0(
      "<div style='font-size: 16px;'>",
      "<b>Fruit analysé :</b> <span style='color: #007bc2;'>", fruit_en_cours(), "</span><br/>",
      "<b>Dernier fichier scanné :</b> ", nom_fichier_actuel(), "<br/>",
      "<b>Nombre de spectres inclus dans les stats :</b> ", nb_fichiers, "<br/>",
      "<b>Liste des fichiers du fruit :</b> <i>", paste(fichiers, collapse = ", "), "</i>",
      "</div>"
    ))
  })
  
  output$table_biochimie <- renderTable({
    req(resultats())
    req(nrow(historique_biochimie()) > 0)
    
    res_actuel <- resultats()$biochimie
    hist <- historique_biochimie()
    
    data.frame(
      Parametre   = names(res_actuel),
      Valeur      = res_actuel,
      Moyenne     = colMeans(hist, na.rm = TRUE),
      Ecart_type  = apply(hist, 2, function(x) if(length(x) > 1) sd(x, na.rm = TRUE) else 0),
      Max         = apply(hist, 2, max, na.rm = TRUE),
      Min         = apply(hist, 2, min, na.rm = TRUE),
      row.names   = NULL
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