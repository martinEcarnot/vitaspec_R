# ==============================================================================
# SCRIPT DE PREDICTION PLS EN PRODUCTION PAR LOT (DOSSIER COMPLET) - LOCAL
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Erreur : arguments manquants. Exemple : Rscript prediction_dossier_pls.R \"C:/Dossier/FRAIS\" frais")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(pls)
  library(nirsextra)
  library(rchemo)
})

d0 <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER"
dossier_cible <- args[1]
type_tissu_user <- tolower(args[2])

if (!dir.exists(dossier_cible)) {
  stop(paste("Erreur : Le dossier est introuvable :", dossier_cible))
}

# Mapping du mot-cle utilisateur vers le tissu exact
map_tissu <- c("frais" = "meso_frais", "sec" = "meso_silica", "hr" = "HR")
if (!(type_tissu_user %in% names(map_tissu))) {
  stop("Erreur : Le tissu n'est pas reconnu. Utilisez 'frais', 'sec' ou 'hr'.")
}
tissu_exact <- map_tissu[[type_tissu_user]]

fichiers_csv <- list.files(dossier_cible, pattern = "\\.csv$", full.names = TRUE)
if (length(fichiers_csv) == 0) {
  stop("Aucun fichier .csv trouve dans le dossier indique.")
}

cat("=========================================================\n")
cat("PREDICTIONS EN LOT PLS | TISSU :", tissu_exact, "\n")
cat("DOSSIER CIBLE :", basename(dossier_cible), "(", length(fichiers_csv), "fichiers trouves )\n")
cat("=========================================================\n")

# --- PRE-CHARGEMENT DU CATALOGUE ---
chemin_cat <- file.path(d0, "PRODUCTION", "CATALOGUE_PRODUCTION_pls.csv")
if (!file.exists(chemin_cat)) stop("Catalogue PLS introuvable.")
df_cat <- read.csv(chemin_cat, sep = ";", stringsAsFactors = FALSE)

# Filtrage imediat pour ne garder que le tissu souhaite
df_cat_tissu <- df_cat[df_cat$Tissu == tissu_exact, ]

if (nrow(df_cat_tissu) == 0) {
  stop("Aucun modele PLS trouve pour ce tissu dans le catalogue.")
}

# --- BOUCLE SUR CHAQUE MATRICE CSV ---
for (fichier in fichiers_csv) {
  nom_matrice <- tools::file_path_sans_ext(basename(fichier))
  dossier_out <- file.path(d0, "predictions", nom_matrice)
  dir.create(dossier_out, recursive = TRUE, showWarnings = FALSE)
  
  cat(sprintf("\n>>> Traitement de la matrice : %s.csv <<<\n", nom_matrice))
  
  df_inconnu <- read.csv(fichier, stringsAsFactors = FALSE)
  if (!"ech" %in% names(df_inconnu)) {
    df_inconnu$ech <- paste0("Inconnu_", 1:nrow(df_inconnu))
  }
  
  col_spectres <- grep("^x\\.", names(df_inconnu), value = TRUE)
  X_brut <- as.matrix(df_inconnu[, col_spectres])
  X_brut[X_brut <= 0] <- 1e-9
  
  df_resultats <- data.frame(ech = df_inconnu$ech)
  
  for (i in 1:nrow(df_cat_tissu)) {
    compose <- df_cat_tissu$Compose[i]
    code_pre <- df_cat_tissu$Pretraitement[i]
    nvl <- as.numeric(df_cat_tissu$Variables_Latentes[i])
    chemin_pack <- file.path(d0, df_cat_tissu$Dossier_Pack_Modeles[i])
    
    cat(" -> Prédiction", compose, "...\n")
    
    etapes_pre <- eval(parse(text = code_pre))
    X_trans <- tryCatch({ pre(X_brut, etapes_pre) }, error = function(e) { NULL })
    
    if (!is.null(X_trans)) {
      preds_10 <- matrix(NA, nrow = nrow(X_trans), ncol = 10)
      
      for (iter in 1:10) {
        fichier_modele <- file.path(chemin_pack, paste0("modele_prod_iter_", iter, ".rds"))
        if (file.exists(fichier_modele)) {
          mod <- readRDS(fichier_modele)
          preds_10[, iter] <- as.numeric(predict(mod, newdata = X_trans, ncomp = nvl))
        }
      }
      df_resultats[[compose]] <- round(rowMeans(preds_10, na.rm = TRUE), 4)
    }
  }
  
  # Sauvegarde
  chemin_xlsx <- file.path(dossier_out, paste0("PLS_", tissu_exact, ".xlsx"))
  chemin_csv <- file.path(dossier_out, paste0("PLS_", tissu_exact, ".csv"))
  
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df_resultats, chemin_xlsx)
    cat("  [OK] Sauvegarde :", basename(chemin_xlsx), "\n")
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df_resultats, chemin_xlsx)
    cat("  [OK] Sauvegarde :", basename(chemin_xlsx), "\n")
  } else {
    write.table(df_resultats, chemin_csv, sep = ";", row.names = FALSE)
    cat("  [OK] Sauvegarde CSV :", basename(chemin_csv), "\n")
  }
}

cat("\nScript R par lot termine.\n")