# ==============================================================================
# SCRIPT R : Génération automatique des graphiques Pred vs Mesure (PLS)
# ==============================================================================

library(ggplot2)

# --- 1. CONFIGURATION LOCALE ---
root_path <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER"
dossier_test_final <- file.path(root_path, "pls", "test_final")

# --- 2. RECHERCHE RÉCURSIVE DE TOUS LES FICHIERS DE PRÉDICTION ---
fichiers_csv <- list.files(
  path = dossier_test_final, 
  pattern = "\\.csv$", 
  recursive = TRUE, 
  full.names = TRUE
)

cat(sprintf("==> %d fichiers trouvés au total.\n\n", length(fichiers_csv)))

# Fonction pour nettoyer et convertir proprement en numérique (gère les virgules et points)
to_numeric_safe <- function(colonne) {
  if (is.null(colonne)) return(NULL)
  if (is.numeric(colonne)) return(colonne)
  # Remplace les virgules par des points si les nombres sont stockés en texte avec virgule
  colonne_propre <- gsub(",", ".", as.character(colonne))
  suppressWarnings(as.numeric(colonne_propre))
}

# --- 3. BOUCLE DE TRAITEMENT GLOBAL ---
for (chemin_csv in fichiers_csv) {
  
  nom_fichier <- basename(chemin_csv)
  
  # Ignore les fichiers qui ne sont pas des prédictions externes si besoin
  if (!grepl("PREDICTIONS_EXTERNES", nom_fichier)) next
  
  compose <- sub("^PREDICTIONS_EXTERNES_(.*)\\.csv$", "\\1", nom_fichier)
  dossier_cible <- dirname(chemin_csv)
  
  cat(sprintf("Traitement -> Composé : %s | Dossier : %s\n", compose, basename(dossier_cible)))
  
  df <- tryCatch({
    read.csv(chemin_csv, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(df) || nrow(df) == 0) next
  
  # Détection souple des noms de colonnes (au cas où ils diffèrent de RF)
  cols <- colnames(df)
  col_true <- cols[grepl("Vraie_Valeur|Observed|Measured|y_true", cols, ignore.case = TRUE)][1]
  col_pred <- cols[grepl("Valeur_Predite|Predicted|y_pred|pred", cols, ignore.case = TRUE)][1]
  
  if (is.na(col_true) || is.na(col_pred)) {
    cat("  [AVERTISSEMENT] Colonnes introuvables dans ce fichier. Ignoré.\n")
    next
  }
  
  y_true <- to_numeric_safe(df[[col_true]])
  y_pred <- to_numeric_safe(df[[col_pred]])
  
  # Nettoyage des éventuels NA générés
  valid_idx <- !is.na(y_true) & !is.na(y_pred)
  y_true <- y_true[valid_idx]
  y_pred <- y_pred[valid_idx]
  
  if (length(y_true) == 0) next
  
  # Calculs des métriques
  rmsep <- sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
  ss_res <- sum((y_true - y_pred)^2, na.rm = TRUE)
  ss_tot <- sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
  r2p <- ifelse(ss_tot == 0, 0, 1 - (ss_res / ss_tot))
  rpd <- ifelse(rmsep == 0, 0, sd(y_true, na.rm = TRUE) / rmsep)
  
  # Textes du graphique
  titre_graph <- sprintf("PLS Predictions vs Mesures : %s", compose)
  sous_titre_graph <- sprintf("R2p = %.3f    |    RMSEp = %.3f    |    RPD = %.3f", r2p, rmsep, rpd)
  
  # Création du data.frame propre pour ggplot
  df_plot <- data.frame(Vraie_Valeur = y_true, Valeur_Predite = y_pred)
  
  # Création du graphique ggplot2 (Style publication épuré)
  p <- ggplot(data = df_plot, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_point(shape = 1, size = 3, color = "black") +
    labs(
      title = titre_graph,
      subtitle = sous_titre_graph,
      x = "Valeurs predites",
      y = "Valeurs mesurees"
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(color = "#EBEBEB", linewidth = 0.8),
      axis.line.y = element_line(color = "#EBEBEB", linewidth = 0.8),
      plot.title = element_text(size = 12, color = "black", hjust = 0, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 9, color = "black", hjust = 0, margin = margin(b = 15)),
      axis.title.x = element_text(size = 10, color = "black", margin = margin(t = 10)),
      axis.title.y = element_text(size = 10, color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 8, color = "#555555")
    )
  
  # Chemins de sauvegarde (PDF et PNG)
  chemin_pdf <- file.path(dossier_cible, sprintf("SCATTER_PUBLI_PLS_%s.pdf", compose))
  chemin_png <- file.path(dossier_cible, sprintf("SCATTER_PUBLI_PLS_%s.png", compose))
  
  ggsave(chemin_pdf, plot = p, width = 7, height = 5, dpi = 300)
  ggsave(chemin_png, plot = p, width = 7, height = 5, dpi = 300)
}

cat("\n========================================================\n")
cat(" TERMINÉ ! Tous les graphiques PLS ont été générés.\n")
cat("========================================================\n")