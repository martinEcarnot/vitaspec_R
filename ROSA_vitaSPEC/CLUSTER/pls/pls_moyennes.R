# %% [IMPORTANT] COMMAND LINE ARGUMENTS & PACKAGES
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Erreur : Il manque des arguments. Usage: Rscript PLS_moyennes.R <compose> <fichier_data> <idparam>")
}

# Nettoyage brutal des arguments bash (retire les espaces parasites)
compose <- trimws(args[1])
fichier_data <- trimws(args[2])
idparam <- trimws(args[3])

message(paste0("\n=== EXECUTION PLS POUR : ", compose, " ==="))

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

# %% CHEMINS ABSOLUS
d0 <- "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
dir_results <- file.path(d0, "pls", "moyennes", "Results", idparam, compose)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

# %% CHARGEMENT & NETTOYAGE DES DONNEES
path_data <- file.path(d0, "commun", fichier_data)
df_data <- read.csv(path_data, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

# Equivalent du nettoyage Python
df_data <- df_data %>% select(-matches("^(campagne|source|etat)$"))
df_data[[compose]] <- as.numeric(df_data[[compose]])
df_propre <- df_data %>% filter(!is.na(.data[[compose]]))

col_spectres <- grep("^x\\.[0-9]+", names(df_propre), value = TRUE)

if (length(col_spectres) == 0) {
  stop("ERREUR CRITIQUE : Aucune colonne spectrale (x. ...) detectee. Verifiez le format.")
}

message(nrow(df_propre), " echantillons et ", length(col_spectres), " longueurs d'ondes valides trouves.")

# %% CHARGEMENT DES PARAMETRES OPTIMAUX (A L'EPREUVE DES BALLES)
path_params <- file.path(d0, "commun", "meilleurs_parametres_pls.csv")
if (!file.exists(path_params)) {
  stop(paste("Fichier des parametres introuvable a l'adresse :", path_params))
}

# Lecture ligne par ligne pour court-circuiter les problemes d'encodage
lignes <- readLines(path_params, warn = FALSE)
lignes <- lignes[trimws(lignes) != ""] # Retirer les lignes vides

# Detection du separateur
sep_char <- if(grepl(";", lignes[1])) ";" else ","

# === LA MAGIE EST ICI : quote = "" et comment.char = "" ===
# On interdit a R d'interpreter les guillemets. Il va couper a chaque point-virgule, un point c'est tout.
df_params_all <- read.table(text = lignes, sep = sep_char, header = TRUE, 
                            stringsAsFactors = FALSE, check.names = FALSE, 
                            quote = "", comment.char = "")

# Rattrapage d'urgence si echec du separateur
if(ncol(df_params_all) < 4) {
  sep_char <- if(sep_char == ";") "," else ";"
  df_params_all <- read.table(text = lignes, sep = sep_char, header = TRUE, 
                              stringsAsFactors = FALSE, check.names = FALSE, 
                              quote = "", comment.char = "")
}

if(ncol(df_params_all) < 4) {
  stop(paste("ERREUR CSV : 4 colonnes attendues, mais seulement", ncol(df_params_all), "trouvee(s). Le separateur n'est pas reconnu."))
}

# Forcage des noms pour detruire tout caractere BOM invisible
names(df_params_all)[1:4] <- c("idparam", "compose", "pretraitement", "ncomp")

# === LE NETTOYEUR EXTREME ===
# Cette fonction ecrase la casse, les espaces et les guillemets pour garantir la correspondance
nettoyer_txt <- function(x) { tolower(trimws(gsub('["\']', '', as.character(x)))) }

df_params_all$idparam_clean <- nettoyer_txt(df_params_all$idparam)
df_params_all$compose_clean <- nettoyer_txt(df_params_all$compose)

idparam_cible <- nettoyer_txt(idparam)
compose_cible <- nettoyer_txt(compose)

df_param_comp <- df_params_all %>% 
  filter(idparam_clean == idparam_cible & compose_clean == compose_cible)

if (nrow(df_param_comp) == 0) {
  message("\n--- DEBUG FATAL CSV ---")
  message("L'algorithme cherchait exactement : idparam='", idparam_cible, "' | compose='", compose_cible, "'")
  message("\nVoici les valeurs reellement lues dans la colonne 'compose' du CSV :")
  message(paste(unique(df_params_all$compose_clean), collapse=", "))
  stop("Echec de l'association CSV. Comparez les noms affiches ci-dessus.")
}

code_pretraitement_gagnant <- as.character(df_param_comp$pretraitement[1])
code_pretraitement_gagnant <- trimws(code_pretraitement_gagnant)

# Nettoyage des doubles guillemets qui auraient pu fuiter
code_pretraitement_gagnant <- gsub('^"|"$', '', code_pretraitement_gagnant) 

opt_ncomp <- as.integer(df_param_comp$ncomp[1])

if (is.na(code_pretraitement_gagnant) || code_pretraitement_gagnant == "") {
  stop("ERREUR CRITIQUE : La chaine de pretraitement extraite du CSV est vide.")
}

# %% ISOLOIR TRAIN / TEST
path_test_externe_rf <- file.path(d0, "random_forest", "jeux_test", idparam, paste0("valid_externe_", compose, ".csv"))

if (file.exists(path_test_externe_rf)) {
  message("Jeu de test externe RF detecte. Importation pour alignement strict...")
  df_test_rf <- read.csv(path_test_externe_rf)
  df_test_externe <- df_propre %>% filter(ech %in% df_test_rf$ech)
  df_train_val <- df_propre %>% filter(!ech %in% df_test_rf$ech)
} else {
  message("Aucun jeu de test externe RF trouve. Separation native (seed 42)...")
  test_indices <- sample(seq_len(nrow(df_propre)), 30)
  df_test_externe <- df_propre[test_indices, ]
  df_train_val <- df_propre[-test_indices, ]
}

# %% FORCAGE NUMERIQUE ET BOUCLIER ANTI-CRASH
message("\nActivation des boucliers de donnees (Bruit, Flatlines & NAs)...")

nettoyer_matrice <- function(df_part, col_spec, nom_compos) {
  X_tmp <- df_part[, col_spec, drop = FALSE]
  X_tmp[] <- lapply(X_tmp, function(x) as.numeric(as.character(x)))
  X_mat <- as.matrix(X_tmp)
  
  # Jitter leger (10^-6)
  bruit <- matrix(rnorm(length(X_mat), mean = 0, sd = 1e-6), 
                  nrow = nrow(X_mat), ncol = ncol(X_mat))
  X_mat <- X_mat + bruit
  
  # NAs et Infs
  X_mat[!is.finite(X_mat)] <- 1e-9
  X_mat[X_mat <= 0] <- 1e-9
  
  # Flatlines
  sds <- apply(X_mat, 1, sd)
  idx_valides <- which(sds > 1e-10)
  y_val <- as.numeric(df_part[[nom_compos]])
  
  if(length(idx_valides) < nrow(X_mat)) {
    message(" -> BOUCLIER : ", nrow(X_mat) - length(idx_valides), " spectres morts supprimes.")
    X_mat <- X_mat[idx_valides, , drop = FALSE]
    y_val <- y_val[idx_valides]
    df_part <- df_part[idx_valides, , drop = FALSE]
  }
  return(list(X = X_mat, y = y_val, df = df_part))
}

train_clean <- nettoyer_matrice(df_train_val, col_spectres, compose)
X_train <- train_clean$X
y_train <- train_clean$y
df_train_val <- train_clean$df

test_clean <- nettoyer_matrice(df_test_externe, col_spectres, compose)
X_test <- test_clean$X
y_test <- test_clean$y
df_test_externe <- test_clean$df

# %% PRETRAITEMENT UNIQUE 
message("\n--- TELEMETRIE PRETRAITEMENT ---")
message("Chaine lue depuis CSV : ", code_pretraitement_gagnant)

# Execution securisee
etapes_pre <- eval(parse(text = code_pretraitement_gagnant))

message("Dim X_train : ", nrow(X_train), " lignes x ", ncol(X_train), " colonnes")
message("Dim X_test  : ", nrow(X_test), " lignes x ", ncol(X_test), " colonnes")
message("--------------------------------\n")

tryCatch({
  X_train_trans <- pre(X_train, etapes_pre)
  X_test_trans  <- pre(X_test, etapes_pre)
}, error = function(e) {
  stop(paste("Erreur critique (la fonction pre() a rejete la matrice) :", e$message))
})

if(is.null(X_train_trans) || ncol(X_train_trans) == 0) {
  stop("Le pretraitement a detruit la matrice.")
}

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

# Calcul des metriques internes
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

message(paste0("\nResultats Externe -> R2p: ", round(r2p_ext, 4), " | RMSEP: ", round(rmsep_ext, 4)))

# %% ENREGISTREMENT DES RAPPORTS ET TABLEAUX
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
write.csv2(df_preds_ext_export, file.path(dir_results, paste0("PREDICTIONS_EXTERNES_", compose, ".csv")), row.names = FALSE)

saveRDS(mod_final, file.path(dir_results, paste0("modele_PLS_", compose, ".rds")))

rapport_champion <- list(
  Compose = compose,
  Pretraitement_Gagnant = code_pretraitement_gagnant,
  Nombre_VL_Optimal = opt_ncomp,
  Metriques_Internes = list(Rc = round(rc, 4), R2cv = round(r2cv, 4), RMSEC = round(rmsec, 4), RMSECV = round(rmsecv, 4)),
  Crash_Test_Externe = list(R2p_Externe = round(r2p_ext, 4), RMSEP_Externe = round(rmsep_ext, 4), RPD_Externe = round(rpd_ext, 4))
)
write_json(rapport_champion, file.path(dir_results, paste0("rapport_A_", compose, ".json")), auto_unbox = TRUE, pretty = TRUE)

# %% GENERATION DES GRAPHES
wavelen <- as.numeric(gsub("^x\\.", "", col_spectres))
coef_values <- as.vector(coef(mod_final, ncomp = opt_ncomp, intercept = FALSE))

g1 <- ggplot(df_preds_ext_export, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
  geom_point(shape = 1, size = 3, color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = paste("PLS Predictions vs Mesures :", compose),
       subtitle = paste0("R2p = ", round(r2p_ext, 3), " | RMSEp = ", round(rmsep_ext, 3), " | RPD = ", round(rpd_ext, 3)),
       x = "Valeurs predites", y = "Valeurs mesurees") +
  theme_minimal()

df_stem <- data.frame(Wavelength = wavelen, Coef = coef_values)
g2 <- ggplot(df_stem, aes(x = Wavelength, y = Coef)) +
  geom_segment(aes(x = Wavelength, xend = Wavelength, y = 0, yend = Coef), color = "#4169E1", size = 0.4) +
  geom_point(color = "#4169E1", size = 1) +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = paste("PLS - Vecteur des coefficients :", compose),
       x = "Longueurs d'ondes (nm)", y = "Intensite du Coefficient") +
  theme_bw() +
  theme(text = element_text(family = "serif"))

ggsave(file.path(dir_results, paste0("RAPPORT_GRAPHIQUES_", compose, ".pdf")), 
       plot = marrangeGrob(list(g1, g2), nrow=1, ncol=1), width = 8, height = 6)
ggsave(file.path(dir_results, paste0("RAPPORT_GRAPHIQUES_", compose, ".png")), 
       plot = grid.arrange(g1, g2, ncol = 1), width = 8, height = 11)

message(paste0("\nFin de l'execution pour ", compose, ". Tous les fichiers ont ete enregistres."))