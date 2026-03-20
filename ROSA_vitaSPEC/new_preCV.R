library(tidyverse)
library(tidymodels) # rsample (pour les blocs) et yardstick (pour les métriques)
library(pls)
library(writexl)



new_preCV = function(data,x,y,list_pre,ncomp,seg,titl,y_name,rep){
  res <- list()
  for (k in 1:rep){
    for (j in 1:length(list_pre)) {  
    
      xp = pre(x, list_pre[[j]])
    
      df_temp <- data.frame(y = y)
      df_temp$xp <- xp
    
      # mod <- plsr(
      #   y ~ xp,
      #   data = data,
      #   ncomp = ncomp,
      #   scale = TRUE,
      #   validation = "CV",
      #   segments = seg)
      # fitted(mod)
      # RMSEP(mod)
    
      # process complet de CV
      n <- nrow(data)
      segs <- cvsegments(n,seg)
      cvpred <- rep(0,n)
      
      for (k in 1:seg){
        cvmod = plsr(y~xp, data = df_temp[-segs[[k]],], ncomp = ncomp, scale = FALSE, validation = "CV", segments = seg)
        bestncomp = selectNcomp(cvmod,method = "onesigma")
        cvpred[segs[[k]]] = predict(cvmod, newdata = df_temp[segs[[k]],])[,,bestncomp]
      }
      
      PRESS.pls <- sum((y - cvpred)^2) 
      RMSEP_glo <- sqrt(PRESS.pls/n)
      
      ## recupère le nom du pretraitement
      mat <- list_pre[[j]]
      parts <- sapply(1:nrow(mat), function(i) {
        nom <- mat[i, 1][[1]]
        val <- mat[i, 2][[1]]
        if (length(val) > 1) {
          val_str <- paste0("c(", paste(val, collapse = ","), ")")
          return(paste0(nom, "_", val_str))
        } else if (val != "") {
          return(paste0(nom, "_", val))
        } else {
          return(nom)
        }
      })
      nom_pre <- paste0(paste(parts, collapse = "_"), "_")
      
      ## modèle prennant en compte la variabilité associée au choix du nb de VL
      mod_global <- plsr(y ~ xp, data = df_temp, ncomp = ncomp, scale = FALSE, validation = "CV", segments = segs)
      
      # stock valeurs tableau comparaison
      res_rmsep <- RMSEP(mod_global, estimate = "CV")$val[1, 1, -1] 
      res_r2 <- R2(mod_global, estimate = "CV")$val[1, 1, -1]    
      x_comps <- 1:(length(res_r2))
      
      ## vcteur nom
      noms_lv <- names(res_rmsep) 
      df_res <- data.frame(matrix(ncol = 0, nrow = 1))
      
      for(i in 1:length(res_rmsep)){
        lv_num <- i - 1
        df_res[1, paste0("LV", lv_num, "_RMSEP")] <- res_rmsep[i]
        df_res[1, paste0("LV", lv_num, "_R2")]    <- res_r2[i]
      }
      
      ## nom des coonnes des res
      df_res <- cbind(Pretraitement = nom_pre, Repetition = k, RMSEP = RMSEP_glo, df_res)
      
      res[[j]] <- df_res
  
      #graph
      marges <- par(mar = c(5, 4, 4, 4) + 0.1) 
      titre_graph <- paste0(titl, " - ", y_name)
      
      selectNcomp(mod_global, method = "onesigma", plot = TRUE, main = titre_graph,comps = 1:ncomp)
      
    
      par(new = TRUE) 
      plot(x_comps, res_r2, type = "b", col = "red", pch = 17, lty = 2,
           axes = FALSE, xlab = "", ylab = "") 
      
      axis(4, col = "red", col.axis = "red")
      mtext("R2", side = 4, line = 2.5, col = "red")
      
      mtext(nom_pre, side = 3, line = 0.5, cex = 0.85)
      
      legend("topleft", 
             inset = c(0.05, 0),
             legend = paste("RMSEP glo =", round(RMSEP_glo, 2)), 
             bty = "n", 
             text.col = "black", 
             text.font = 1,
             cex = 0.7)
  
      par(marges)
    }
      
      #tablea comparaison
      comparaison <- do.call(rbind, res)
      
  }
  
  nom_tableau <- paste0("Resultats_", y_name, ".xlsx")
  write_xlsx(comparaison, path = nom_tableau)
}




















# donnees <- mtcars 
# 
# CV
# set.seed(123) 
# mes_blocs <- vfold_cv(donnees, v = 5)
# 
# # 3. Création de la fonction de calibration et de prédiction
# # Cette fonction sera appliquée à chaque bloc
# evaluer_un_bloc <- function(un_split) {
#   
#   # a) Extraire les données d'entraînement (80%) et de test (20%) de ce bloc
#   donnees_calib <- analysis(un_split)
#   donnees_test <- assessment(un_split)
#   
#   # b) Calibrer le modèle sur les données d'entraînement
#   # ---> C'est ICI que vous mettez votre fonction LWPLSR ou PLS <---
#   modele <- lm(mpg ~ hp + wt, data = donnees_calib)
#   
#   # c) Faire les prédictions sur les 20% mis de côté
#   predictions <- predict(modele, newdata = donnees_test)
#   
#   # d) Retourner un tableau (tibble) avec les vraies valeurs et les prédictions
#   tibble(
#     vraie_valeur = donnees_test$mpg,
#     prediction = predictions
#   )
# }
# 
# # 4. La magie de dplyr et purrr : appliquer la fonction à tous les blocs
# resultats_cv <- mes_blocs %>%
#   # On applique notre fonction sur chaque "split" (bloc)
#   mutate(mes_predictions = map(splits, evaluer_un_bloc)) %>%
#   # On "déplie" les tableaux pour tout rassembler
#   unnest(mes_predictions)
# 
# # 5. Calculer l'erreur globale (RMSECV)
# # La fonction rmse() vient du package yardstick (inclus dans tidymodels)
# erreur_finale <- resultats_cv %>%
#   rmse(truth = vraie_valeur, estimate = prediction)
# 
# print(erreur_finale)