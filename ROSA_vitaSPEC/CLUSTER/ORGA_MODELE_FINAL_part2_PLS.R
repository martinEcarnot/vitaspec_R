# ==============================================================================
# ENTRAINEMENT DES MODELES PLS DE PRODUCTION SUR 100% DU DATASET
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(pls)
  library(nirsextra)
  library(rchemo)  
})

d0 <- "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
chemin_catalogue <- file.path(d0, "PRODUCTION", "CATALOGUE_PRODUCTION_pls.csv")

if (!file.exists(chemin_catalogue)) stop("Catalogue PLS introuvable. Lancez le script Python d'abord !")

df_cat <- read.csv(chemin_catalogue, sep = ";", stringsAsFactors = FALSE)

nettoyer_matrice <- function(df_part, col_spec, nom_compos) {
  X_tmp <- df_part[, col_spec, drop = FALSE]
  X_tmp[] <- lapply(X_tmp, function(x) as.numeric(as.character(x)))
  X_mat <- as.matrix(X_tmp)
  
  bruit <- matrix(rnorm(length(X_mat), mean = 0, sd = 1e-6), nrow = nrow(X_mat), ncol = ncol(X_mat))
  X_mat <- X_mat + bruit
  
  X_mat[!is.finite(X_mat)] <- 1e-9
  X_mat[X_mat <= 0] <- 1e-9
  
  sds <- apply(X_mat, 1, sd)
  idx_valides <- which(sds > 1e-10)
  y_val <- as.numeric(df_part[[nom_compos]])
  
  if(length(idx_valides) < nrow(X_mat)) {
    X_mat <- X_mat[idx_valides, , drop = FALSE]
    y_val <- y_val[idx_valides]
  }
  return(list(X = X_mat, y = y_val))
}

cat("=========================================================\n")
cat("DEBUT ENTRAINEMENT PLS PRODUCTION (10 MODELES / 100% DATA)\n")
cat("=========================================================\n")

for (i in 1:nrow(df_cat)) {
  tissu <- df_cat$Tissu[i]
  compose <- df_cat$Compose[i]
  fichier_data <- df_cat$Fichier_Data[i]
  code_pre <- df_cat$Pretraitement[i]
  nvl <- as.numeric(df_cat$Variables_Latentes[i])
  dossier_dest <- file.path(d0, df_cat$Dossier_Pack_Modeles[i])
  
  # Securite absolue : on force la creation du dossier s'il n'existe pas
  if (!dir.exists(dossier_dest)) {
    dir.create(dossier_dest, recursive = TRUE, showWarnings = FALSE)
  }
  
  cat(sprintf("\nTraitement PLS : %s | %s\n", tissu, compose))
  
  df_data_full <- read.csv(file.path(d0, "commun", fichier_data), stringsAsFactors = FALSE)
  df_data_full[[compose]] <- suppressWarnings(as.numeric(df_data_full[[compose]]))
  df_propre <- df_data_full[!is.na(df_data_full[[compose]]), ]
  
  col_spectres <- grep("^x\\.", names(df_propre), value = TRUE)
  etapes_pre <- eval(parse(text = code_pre))
  
  nb_succes <- 0
  
  for (iter in 1:10) {
    seed <- sample(1:999999, 1)
    set.seed(seed)
    
    clean_data <- nettoyer_matrice(df_propre, col_spectres, compose)
    
    X_trans <- tryCatch({ 
      pre(clean_data$X, etapes_pre) 
    }, error = function(e) { 
      cat("    [!] Erreur pre() a l'iteration", iter, ":", conditionMessage(e), "\n")
      return(NULL) 
    })
    
    if (is.null(X_trans)) next
      
    df_pls <- data.frame(Y = clean_data$y)
    df_pls$X <- I(X_trans)
    
    mod_prod <- tryCatch({
      plsr(Y ~ X, ncomp = nvl, data = df_pls, method = "kernelpls")
    }, error = function(e) {
      cat("    [!] Erreur plsr() a l'iteration", iter, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (!is.null(mod_prod)) {
      chemin_save <- file.path(dossier_dest, paste0("modele_prod_iter_", iter, ".rds"))
      saveRDS(mod_prod, chemin_save)
      # On verifie que le fichier a BIEN ete cree sur le disque dur
      if(file.exists(chemin_save)) nb_succes <- nb_succes + 1
    }
  }
  
  # Le Vrai verdict
  if(nb_succes == 10) {
    cat("-> 10 modeles PLS sauvegardes avec succes.\n")
  } else if (nb_succes > 0) {
    cat("-> ATTENTION : Seulement", nb_succes, "modeles sauvegardes sur 10 !\n")
  } else {
    cat("-> ECHEC TOTAL : 0 modele sauvegarde pour", compose, "!\n")
  }
}
cat("\nProduction PLS terminee !\n")