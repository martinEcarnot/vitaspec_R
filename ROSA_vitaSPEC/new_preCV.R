library(tidyverse)
library(tidymodels) # Contient rsample (pour les blocs) et yardstick (pour les métriques)
library(pls)


d0 <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/"
source(paste0(d0,"vitaspec_R/list_pre.R"))


new_preCV = function(x,y,fun,list_pre){
  
}




















donnees <- mtcars 

CV
set.seed(123) 
mes_blocs <- vfold_cv(donnees, v = 5)

# 3. Création de la fonction de calibration et de prédiction
# Cette fonction sera appliquée à chaque bloc
evaluer_un_bloc <- function(un_split) {
  
  # a) Extraire les données d'entraînement (80%) et de test (20%) de ce bloc
  donnees_calib <- analysis(un_split)
  donnees_test <- assessment(un_split)
  
  # b) Calibrer le modèle sur les données d'entraînement
  # ---> C'est ICI que vous mettez votre fonction LWPLSR ou PLS <---
  modele <- lm(mpg ~ hp + wt, data = donnees_calib)
  
  # c) Faire les prédictions sur les 20% mis de côté
  predictions <- predict(modele, newdata = donnees_test)
  
  # d) Retourner un tableau (tibble) avec les vraies valeurs et les prédictions
  tibble(
    vraie_valeur = donnees_test$mpg,
    prediction = predictions
  )
}

# 4. La magie de dplyr et purrr : appliquer la fonction à tous les blocs
resultats_cv <- mes_blocs %>%
  # On applique notre fonction sur chaque "split" (bloc)
  mutate(mes_predictions = map(splits, evaluer_un_bloc)) %>%
  # On "déplie" les tableaux pour tout rassembler
  unnest(mes_predictions)

# 5. Calculer l'erreur globale (RMSECV)
# La fonction rmse() vient du package yardstick (inclus dans tidymodels)
erreur_finale <- resultats_cv %>%
  rmse(truth = vraie_valeur, estimate = prediction)

print(erreur_finale)