# ==============================================================================
# SCRIPT DE VALIDATION FINALE PLS (10 ITERATIONS) - ARCHITECTURE HYBRIDE
# ==============================================================================

# %% IMPORTATIONS
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Erreur : manque des arg <compose> <fichier_data> <idparam>")
}

# clean arg
compose <- trimws(args[1])
fichier_data <- trimws(args[2])
idparam <- trimws(args[3])

suppressPackageStartupMessages({
  library(tidyverse)
  library(pls)
  library(rchemo)
  library(jsonlite)
  library(ggplot2)
  library(gridExtra)
  library(nirsextra)
})

# %% CHEMINS
d0 <- "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
dir_base_test <- file.path(d0, "pls", "test_final", idparam, compose)
dir.create(dir_base_test, recursive = TRUE, showWarnings = FALSE)

# %% CHARGEMENT DONNEES
path_data <- file.path(d0, "commun", fichier_data)
df_data <- read.csv(path_data, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

# clean
df_data <- df_data %>% select(-matches("^(campagne|source|etat)$"))
df_data[[compose]] <- suppressWarnings(as.numeric(df_data[[compose]]))
df_propre <- df_data %>% filter(!is.na(.data[[compose]]))

col_spectres <- grep("^x\\.[0-9]+", names(df_propre), value = TRUE)

if (length(col_spectres) == 0) stop("Erreur : 0 col de spectre (x. ...) detectee")

message("\n[", compose, "] ", nrow(df_propre), " ech / ", length(col_spectres), " longueurs d'ondes")

# %% LECTURE DU FICHIER PRETRAITEMENTS (VOTRE CODE ROBUSTE)
# On verifie le nouveau fichier d'abord, puis l'ancien si non trouve
path_params <- file.path(d0, "pls", "PARAM_OPTI_PLS.csv")
if (!file.exists(path_params)) {
    path_params <- file.path(d0, "commun", "meilleurs_parametres_pls.csv")
}

if (!file.exists(path_params)) stop("Pas de fichier de parametres PLS trouve.")

lignes <- readLines(path_params, warn = FALSE)
# Destruction des en-tetes si presents
if(grepl("Propriete|compose", lignes[1], ignore.case=TRUE)) lignes <- lignes[-1]
lignes <- lignes[trimws(lignes) != ""] # detruit des lignes vides

sep_char <- if(any(grepl(";", lignes))) ";" else "," # detection du separateur
liste_decoupee <- strsplit(lignes, split = sep_char) # decoupage ligne par ligne

# Securite : prise en charge du format a 4 col ou 5 col (selon votre version du fichier)
idx_comp <- if(length(liste_decoupee[[1]]) >= 5) 3 else 2
idx_pre  <- if(length(liste_decoupee[[1]]) >= 5) 4 else 3
idx_nvl  <- if(length(liste_decoupee[[1]]) >= 5) 5 else 4

df_params_all <- data.frame(
  idparam = sapply(liste_decoupee, function(x) x[1]),
  compose = sapply(liste_decoupee, function(x) x[idx_comp]),
  pretraitement = sapply(liste_decoupee, function(x) x[idx_pre]),
  ncomp = sapply(liste_decoupee, function(x) x[idx_nvl]),
  stringsAsFactors = FALSE
)

nettoyer_txt <- function(x) { tolower(trimws(gsub('["\']', '', as.character(x)))) } 

df_params_all$idparam_clean <- nettoyer_txt(df_params_all$idparam)
df_params_all$compose_clean <- nettoyer_txt(df_params_all$compose)

idparam_cible <- nettoyer_txt(idparam)
compose_cible <- nettoyer_txt(compose)

df_param_comp <- df_params_all %>% 
  filter(grepl(idparam_cible, idparam_clean) & compose_clean == compose_cible)

if (nrow(df_param_comp) == 0) {
  message("Le compose n'est pas dans la liste des parametres. Fin du script.")
  quit(status=0)
}

# Extraction Pretraitement
code_pretraitement_gagnant <- as.character(df_param_comp$pretraitement[1])
code_pretraitement_gagnant <- gsub('"', '', code_pretraitement_gagnant) 
code_pretraitement_gagnant <- trimws(code_pretraitement_gagnant)

# Extraction NVL
ncomp_brut <- as.character(df_param_comp$ncomp[1])
opt_ncomp <- as.integer(gsub("[^0-9]", "", ncomp_brut))

message("Pretraitement retenu : ", substr(code_pretraitement_gagnant, 1, 60), "...")
message("Variables Latentes : ", opt_ncomp)

# %% FONCTION DE NETTOYAGE MATRICE (VOTRE CODE)
nettoyer_matrice <- function(df_part, col_spec, nom_compos) {
  X_tmp <- df_part[, col_spec, drop = FALSE]
  X_tmp[] <- lapply(X_tmp, function(x) as.numeric(as.character(x)))
  X_mat <- as.matrix(X_tmp)
  
  # ajout d'un bruit
  bruit <- matrix(rnorm(length(X_mat), mean = 0, sd = 1e-6), 
                  nrow = nrow(X_mat), ncol = ncol(X_mat))
  X_mat <- X_mat + bruit
  
  X_mat[!is.finite(X_mat)] <- 1e-9
  X_mat[X_mat <= 0] <- 1e-9
  
  sds <- apply(X_mat, 1, sd)
  idx_valides <- which(sds > 1e-10)
  y_val <- as.numeric(df_part[[nom_compos]])
  
  if(length(idx_valides) < nrow(X_mat)) {
    message(nrow(X_mat) - length(idx_valides), " spectres morts exclus")
    X_mat <- X_mat[idx_valides, , drop = FALSE]
    y_val <- y_val[idx_valides]
    df_part <- df_part[idx_valides, , drop = FALSE]
  }
  return(list(X = X_mat, y = y_val, df = df_part))
}

# ==============================================================================
# BOUCLE DE VALIDATION (10 ITERATIONS)
# ==============================================================================

NB_ITERATIONS <- 10
bilan_validation <- data.frame()
etapes_pre <- eval(parse(text = code_pretraitement_gagnant))

for (iteration in 1:NB_ITERATIONS) {
  
  SEED_ITER <- sample(1:999999, 1)
  set.seed(SEED_ITER)
  
  cat(sprintf("\n--- ITERATION %d/%d - SEED: %d ---\n", iteration, NB_ITERATIONS, SEED_ITER))
  
  dossier_iter <- file.path(dir_base_test, paste0("iter_", iteration))
  dir.create(dossier_iter, recursive = TRUE, showWarnings = FALSE)
  
  # Split 30 ech
  test_indices <- sample(seq_len(nrow(df_propre)), 30)
  df_test_externe <- df_propre[test_indices, ]
  df_train_val <- df_propre[-test_indices, ]
  
  # Nettoyage matrices
  train_clean <- nettoyer_matrice(df_train_val, col_spectres, compose)
  X_train <- train_clean$X
  y_train <- train_clean$y
  df_train_val <- train_clean$df
  
  test_clean <- nettoyer_matrice(df_test_externe, col_spectres, compose)
  X_test <- test_clean$X
  y_test <- test_clean$y
  df_test_externe <- test_clean$df
  
  # Export Jeu Test Externe Brut
  write.csv(df_test_externe, file.path(dossier_iter, paste0("valid_externe_", compose, ".csv")), row.names = FALSE)
  
  # PRETRAITEMENT (nirsextra)
  tryCatch({
    X_train_trans <- pre(X_train, etapes_pre)
    X_test_trans  <- pre(X_test, etapes_pre)
  }, error = function(e) { stop(paste("Erreur fonction pre():", e$message)) })
  
  if(is.null(X_train_trans) || ncol(X_train_trans) == 0) stop("Le pretraitement a detruit la matrice.")
  
  # ENTRAINEMENT CV 5 FOLDS
  folds <- sample(rep(1:5, length.out = nrow(X_train_trans)))
  cv_predictions <- numeric(nrow(X_train_trans))
  
  for (f in 1:5) {
    idx_val <- which(folds == f)
    X_tr_f <- X_train_trans[-idx_val, , drop = FALSE]
    y_tr_f <- y_train[-idx_val]
    X_val_f <- X_train_trans[idx_val, , drop = FALSE]
    y_val_f <- y_train[idx_val]
    
    mod_fold <- pls::plsr(y_tr_f ~ X_tr_f, ncomp = opt_ncomp, method = "kernelpls")
    preds_val <- as.numeric(predict(mod_fold, newdata = X_val_f, ncomp = opt_ncomp))
    cv_predictions[idx_val] <- preds_val
    
    df_fold_export <- data.frame(
      Numero_Echantillon = idx_val,
      Identifiant_Ech = if("ech" %in% names(df_train_val)) df_train_val$ech[idx_val] else idx_val,
      Valeur_Mesuree = y_val_f,
      Valeur_Predite = preds_val,
      SEP = abs(y_val_f - preds_val)
    )
    write.csv2(df_fold_export, file.path(dossier_iter, paste0("DETAILS_CV_FOLD_", f, "_", compose, ".csv")), row.names = FALSE)
  }
  
  # CALCUL METRIQUES CV
  rmsecv <- sqrt(mean((y_train - cv_predictions)^2))
  r2cv <- 1 - (sum((y_train - cv_predictions)^2) / sum((y_train - mean(y_train))^2))
  
  # ENTRAINEMENT FINAL + PREDICTIONS TEST
  mod_final <- pls::plsr(y_train ~ X_train_trans, ncomp = opt_ncomp, method = "kernelpls")
  
  preds_ext <- as.numeric(predict(mod_final, newdata = X_test_trans, ncomp = opt_ncomp))
  rmsep_ext <- sqrt(mean((y_test - preds_ext)^2))
  r2p_ext <- 1 - (sum((y_test - preds_ext)^2) / sum((y_test - mean(y_test))^2))
  rpd_ext <- sd(y_test) / rmsep_ext
  
  cat(sprintf("-> RMSECV: %.4f | TEST R2p: %.4f | RMSEP: %.4f\n", rmsecv, r2p_ext, rmsep_ext))
  
  # AJOUT BILAN
  bilan_validation <- rbind(bilan_validation, data.frame(
    Iteration = iteration, Seed = SEED_ITER,
    Pretraitement = code_pretraitement_gagnant, Variables_Latentes = opt_ncomp,
    R2cv = round(r2cv, 4), RMSECV = round(rmsecv, 4),
    R2p_Externe = round(r2p_ext, 4), RMSEP_Externe = round(rmsep_ext, 4), RPD_Externe = round(rpd_ext, 4)
  ))
  
  # EXPORT TEST
  df_preds_ext_export <- data.frame(
    Echantillon = if("ech" %in% names(df_test_externe)) df_test_externe$ech else 1:nrow(df_test_externe),
    Vraie_Valeur = y_test, Valeur_Predite = preds_ext, SEP = abs(y_test - preds_ext)
  )
  write.csv2(df_preds_ext_export, file.path(dossier_iter, paste0("PREDICTIONS_EXTERNES_", compose, ".csv")), row.names = FALSE)
  saveRDS(mod_final, file.path(dossier_iter, paste0("modele_PLS_FINAL_", compose, ".rds")))
  
  # --- GRAPHIQUES ITERATION ---
  pdf(file.path(dossier_iter, paste0("RAPPORT_GRAPHIQUES_", compose, ".pdf")), width = 8, height = 6)
  
  g1 <- ggplot(df_preds_ext_export, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
    geom_point(shape = 1, size = 3, color = "black") +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE, linewidth = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("PLS Predictions vs Mesures :", compose),
         subtitle = sprintf("R2p = %.3f | RMSEp = %.3f | RPD = %.3f", r2p_ext, rmsep_ext, rpd_ext),
         x = "Valeurs predites", y = "Valeurs mesurees") +
    theme_minimal()
  print(g1)
  
  wavelen <- as.numeric(gsub("^[xX]\\.?", "", colnames(X_train_trans)))
  if(any(is.na(wavelen))) wavelen <- 1:ncol(X_train_trans)
  
  df_stem <- data.frame(Wavelength = wavelen, Coef = as.vector(coef(mod_final, ncomp = opt_ncomp, intercept = FALSE)))
  g2 <- ggplot(df_stem, aes(x = Wavelength, y = Coef)) +
    geom_segment(aes(x = Wavelength, xend = Wavelength, y = 0, yend = Coef), color = "#4169E1", linewidth = 0.4) +
    geom_point(color = "#4169E1", size = 1) +
    geom_hline(yintercept = 0, color = "gray") +
    labs(title = paste("PLS - Vecteur des coefficients :", compose),
         x = "Longueurs d'ondes (nm) ou Index", y = "Intensite du Coefficient") +
    theme_bw()
  print(g2)
  
  dev.off()
}

# ==============================================================================
# SAUVEGARDE DU BILAN GLOBAL ET BOXPLOTS FINAUX
# ==============================================================================
cat("\n============================================================\n")
cat("FIN DES ITERATIONS - SAUVEGARDE DU BILAN DE VALIDATION PLS\n")
cat("============================================================\n")

write.table(bilan_validation, file.path(dir_base_test, paste0("BILAN_FINAL_VALIDATION_", compose, ".csv")), sep = ";", row.names = FALSE)

tryCatch({
  pdf(file.path(dir_base_test, paste0("RAPPORT_FINAL_BOXPLOTS_", compose, ".pdf")), width = 8, height = 6)
  
  bp_r2 <- ggplot(bilan_validation, aes(x = compose, y = R2p_Externe)) +
    geom_boxplot(fill = '#21908C', color = "black", outlier.shape = NA, width = 0.3) +
    geom_jitter(width = 0.05, size = 1.5, color = "black", alpha = 0.8) +
    theme_bw() + labs(title = sprintf("Boxplot R2p (PLS) : %s", compose), y = "R2p", x = "")
  print(bp_r2)
  
  bp_rmse <- ggplot(bilan_validation, aes(x = compose, y = RMSEP_Externe)) +
    geom_boxplot(fill = '#21908C', color = "black", outlier.shape = NA, width = 0.3) +
    geom_jitter(width = 0.05, size = 1.5, color = "black", alpha = 0.8) +
    theme_bw() + labs(title = sprintf("Boxplot RMSEP (PLS) : %s", compose), y = "RMSEP", x = "")
  print(bp_rmse)
  
  dev.off()
}, error = function(e) { message("Erreur Boxplots: ", e$message) })

cat("Termine avec succes !\n")