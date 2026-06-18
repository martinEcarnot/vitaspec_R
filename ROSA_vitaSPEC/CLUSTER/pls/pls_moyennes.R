# %% IMPORTATIONS
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("manque des arg<compose> <fichier_data> <idparam>")
}

# clean arg
compose <- trimws(args[1])
fichier_data <- trimws(args[2])
idparam <- trimws(args[3])


suppressMessages({
  library(tidyverse)
  library(pls)
  library(rchemo)
  library(jsonlite)
  library(ggplot2)
  library(gridExtra)
  library(nirsextra)
})

SEED <- 42
set.seed(SEED)

## pathing
d0 <- "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
dir_results <- file.path(d0, "pls", "moyennes", "Results", idparam, compose)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

# %% CHARGEMENT DONNEES
path_data <- file.path(d0, "commun", fichier_data)
df_data <- read.csv(path_data, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

# clean
df_data <- df_data %>% select(-matches("^(campagne|source|etat)$"))
df_data[[compose]] <- suppressWarnings(as.numeric(df_data[[compose]]))
df_propre <- df_data %>% filter(!is.na(.data[[compose]]))

col_spectres <- grep("^x\\.[0-9]+", names(df_propre), value = TRUE)

if (length(col_spectres) == 0) {
  stop("error : 0 sol de spectre (x. ...) detectee")
}

message(nrow(df_propre), " ech /", length(col_spectres), " longueurs d'ondes")

## Lecture du fichier pretraitements optimaux (.csv)
path_params <- file.path(d0, "commun", "meilleurs_parametres_pls.csv")
if (!file.exists(path_params)) {
  stop(paste("pas de fichier pretrait optim"))
}
lignes <- readLines(path_params, warn = FALSE)
lignes <- lignes[trimws(lignes) != ""] # detruit des lignes vides

sep_char <- if(any(grepl(";", lignes))) ";" else "," # detection du separateur


liste_decoupee <- strsplit(lignes, split = sep_char) # decoupage ligne par ligne

# reconsruction
df_params_all <- data.frame(
  idparam = sapply(liste_decoupee, function(x) x[1]),
  compose = sapply(liste_decoupee, function(x) x[2]),
  pretraitement = sapply(liste_decoupee, function(x) x[3]),
  ncomp = sapply(liste_decoupee, function(x) x[4]),
  stringsAsFactors = FALSE
)

nettoyer_txt <- function(x) { tolower(trimws(gsub('["\']', '', as.character(x)))) } # ecrase espaces et guillemets

df_params_all$idparam_clean <- nettoyer_txt(df_params_all$idparam)
df_params_all$compose_clean <- nettoyer_txt(df_params_all$compose)

idparam_cible <- nettoyer_txt(idparam)
compose_cible <- nettoyer_txt(compose)

df_param_comp <- df_params_all %>% 
  filter(idparam_clean == idparam_cible & compose_clean == compose_cible)

if (nrow(df_param_comp) == 0) {
  stop("le compose existe pas")
}

# extraction  du pretrait
code_pretraitement_gagnant <- as.character(df_param_comp$pretraitement[1])
code_pretraitement_gagnant <- gsub('"', '', code_pretraitement_gagnant) 
code_pretraitement_gagnant <- trimws(code_pretraitement_gagnant)

if (is.na(code_pretraitement_gagnant) || code_pretraitement_gagnant == "") {
  stop("error : chaine de caractère vide")
}

# extraction du nVL
ncomp_brut <- as.character(df_param_comp$ncomp[1])
opt_ncomp <- as.integer(gsub("[^0-9]", "", ncomp_brut))

if (is.na(opt_ncomp) || opt_ncomp < 1) {
  stop(paste("ncomp invalide valeur =", ncomp_brut))
}

# %% CONFIG MOD
# on isole 30 ech
path_test_externe_rf <- file.path(d0, "random_forest", "jeux_test", idparam, paste0("valid_externe_", compose, ".csv"))

if (file.exists(path_test_externe_rf)) { # si jeux test existe deja 
  df_test_rf <- read.csv(path_test_externe_rf)
  df_test_externe <- df_propre %>% filter(ech %in% df_test_rf$ech)
  df_train_val <- df_propre %>% filter(!ech %in% df_test_rf$ech)
} else {                                 # sinon separe au hazard
  test_indices <- sample(seq_len(nrow(df_propre)), 30)
  df_test_externe <- df_propre[test_indices, ]
  df_train_val <- df_propre[-test_indices, ]
}

# %% CLEAN + ANTICRASH

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
    message(nrow(X_mat) - length(idx_valides), " spectres morts")
    X_mat <- X_mat[idx_valides, , drop = FALSE]
    y_val <- y_val[idx_valides]
    df_part <- df_part[idx_valides, , drop = FALSE]
  }
  return(list(X = X_mat, y = y_val, df = df_part))
}

# TRAIN/TEST clean
train_clean <- nettoyer_matrice(df_train_val, col_spectres, compose)
X_train <- train_clean$X
y_train <- train_clean$y
df_train_val <- train_clean$df

test_clean <- nettoyer_matrice(df_test_externe, col_spectres, compose)
X_test <- test_clean$X
y_test <- test_clean$y
df_test_externe <- test_clean$df

# %% PRETRAITEMENT UNIQUE 
message("pretraitement =", code_pretraitement_gagnant)
message("ncomp =", opt_ncomp)

etapes_pre <- eval(parse(text = code_pretraitement_gagnant))

tryCatch({
  X_train_trans <- pre(X_train, etapes_pre)
  X_test_trans  <- pre(X_test, etapes_pre)
}, error = function(e) {
  stop(paste("error : fonction pre() probleme", e$message))
})

if(is.null(X_train_trans) || ncol(X_train_trans) == 0) {
  stop("pretrait a supprimer toute la matrice")
}

message("Dim X_train (apres pretraitement) : ", nrow(X_train_trans), " lignes x ", ncol(X_train_trans), " colonnes")
message("Dim X_test  (apres pretraitement) : ", nrow(X_test_trans), " lignes x ", ncol(X_test_trans), " colonnes")
message("--------------------------------\n")

# %% VALIDATION CROISEE 5-FOLDS
set.seed(SEED)
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
    Ecart_Mesure = y_val_f - preds_val,
    SEP = abs(y_val_f - preds_val)
  )
  write.csv2(df_fold_export, file.path(dir_results, paste0("DETAILS_CV_FOLD_", f, "_", compose, ".csv")), row.names = FALSE)
}

# extraction + metriques
mod_final <- pls::plsr(y_train ~ X_train_trans, ncomp = opt_ncomp, method = "kernelpls")
preds_cal <- as.numeric(predict(mod_final, newdata = X_train_trans, ncomp = opt_ncomp))

rmsec <- sqrt(mean((y_train - preds_cal)^2))
rmsecv <- sqrt(mean((y_train - cv_predictions)^2))
rc <- cor(y_train, preds_cal)^2
r2cv <- 1 - (sum((y_train - cv_predictions)^2) / sum((y_train - mean(y_train))^2))

# %% EVALUATION SUR LE JEU TEST INDEPENDANT
preds_ext <- as.numeric(predict(mod_final, newdata = X_test_trans, ncomp = opt_ncomp))
rmsep_ext <- sqrt(mean((y_test - preds_ext)^2))
r2p_ext <- 1 - (sum((y_test - preds_ext)^2) / sum((y_test - mean(y_test))^2))
rpd_ext <- sd(y_test) / rmsep_ext

message(paste0("\nTEST  R2p: ", round(r2p_ext, 4), " | RMSEP: ", round(rmsep_ext, 4)))

# %% SAVE
df_metrics <- data.frame(
  Compose = compose,
  Pretraitement_Gagnant = code_pretraitement_gagnant,
  Nombre_VL = opt_ncomp,
  Rc = round(rc, 4),
  R2cv = round(r2cv, 4),
  RMSEC = round(rmsec, 4),
  RMSECV = round(rmsecv, 4),
  R2p_Externe = round(r2p_ext, 4),
  RMSEP_Externe = round(rmsep_ext, 4),
  RPD_Externe = round(rpd_ext, 4)
)
write.csv2(df_metrics, file.path(dir_results, paste0("PLS_DETAILS_", compose, ".csv")), row.names = FALSE)

df_preds_ext_export <- data.frame(
  Echantillon = if("ech" %in% names(df_test_externe)) df_test_externe$ech else 1:nrow(df_test_externe),
  Vraie_Valeur = y_test,
  Valeur_Predite = preds_ext,
  SEP = abs(y_test - preds_ext)
)
write.csv2(df_preds_ext_export, file.path(dir_results, paste0("PREDICTIONS_TEST_", compose, ".csv")), row.names = FALSE)

saveRDS(mod_final, file.path(dir_results, paste0("modele_PLS_", compose, ".rds")))

rapport_champion <- list(
  Compose = compose,
  Pretraitement_Gagnant = code_pretraitement_gagnant,
  Nombre_VL_Optimal = opt_ncomp,
  Metriques_Internes = list(Rc = round(rc, 4), R2cv = round(r2cv, 4), RMSEC = round(rmsec, 4), RMSECV = round(rmsecv, 4)),
  Crash_Test_Externe = list(R2p_Externe = round(r2p_ext, 4), RMSEP_Externe = round(rmsep_ext, 4), RPD_Externe = round(rpd_ext, 4))
)
write_json(rapport_champion, file.path(dir_results, paste0("rapport_A_", compose, ".json")), auto_unbox = TRUE, pretty = TRUE)

# %% GRAPHS

tryCatch({
  
  # longueur d'ondes
  noms_cols <- colnames(X_train_trans)
  if(is.null(noms_cols)) {
    # Si la fonction a detruit les noms, on fait un axe 1 a N
    wavelen <- 1:ncol(X_train_trans) 
  } else {
    # On nettoie le nom pour ne garder que le chiffre
    wavelen <- as.numeric(gsub("^[xX]\\.?", "", noms_cols))
    # Securite ultime
    if(any(is.na(wavelen))) wavelen <- 1:ncol(X_train_trans)
  }
  
  coef_values <- as.vector(coef(mod_final, ncomp = opt_ncomp, intercept = FALSE))
  
  g1 <- ggplot(df_preds_ext_export, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
    geom_point(shape = 1, size = 3, color = "black") +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE, linewidth = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("PLS Predictions vs Mesures :", compose),
         subtitle = paste0("R2p = ", round(r2p_ext, 3), " | RMSEp = ", round(rmsep_ext, 3), " | RPD = ", round(rpd_ext, 3)),
         x = "Valeurs predites", y = "Valeurs mesurees") +
    theme_minimal()
  
  df_stem <- data.frame(Wavelength = wavelen, Coef = coef_values)
  g2 <- ggplot(df_stem, aes(x = Wavelength, y = Coef)) +
    geom_segment(aes(x = Wavelength, xend = Wavelength, y = 0, yend = Coef), color = "#4169E1", linewidth = 0.4) +
    geom_point(color = "#4169E1", size = 1) +
    geom_hline(yintercept = 0, color = "gray") +
    labs(title = paste("PLS - Vecteur des coefficients :", compose),
         x = "Longueurs d'ondes (nm) ou Index", y = "Intensite du Coefficient") +
    theme_bw()
  
  # PDF
  pdf(file.path(dir_results, paste0("RAPPORT_GRAPHIQUES_", compose, ".pdf")), width = 8, height = 6)
  print(g1)
  print(g2)
  dev.off()
  
  # PNG
  png(file.path(dir_results, paste0("RAPPORT_GRAPHIQUES_", compose, ".png")), width = 800, height = 1100, res = 100)
  grid.arrange(g1, g2, ncol = 1)
  dev.off()
  
}, error = function(e) {
  message("error generation graphs : ", e$message)
})
