# ==============================================================================
# SCRIPT R : Génération automatique de tous les graphiques Pred vs Mesure (RF)
# ==============================================================================

library(ggplot2)

# --- 1. CONFIGURATION LOCALE (Chemin corrigé avec U108-N806) ---
root_path <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER"
dossier_test_final <- file.path(root_path, "random_forest", "moyennes", "test_final")

# --- 2. RECHERCHE RÉCURSIVE DE TOUS LES FICHIERS DE PRÉDICTION ---
fichiers_csv <- list.files(
  path = dossier_test_final, 
  pattern = "^PREDICTIONS_EXTERNES_.*\\.csv$", 
  recursive = TRUE, 
  full.names = TRUE
)

cat(sprintf("==> %d fichiers de prédictions trouvés au total.\n\n", length(fichiers_csv)))

# --- 3. BOUCLE DE TRAITEMENT GLOBAL ---
for (chemin_csv in fichiers_csv) {
  
  nom_fichier <- basename(chemin_csv)
  compose <- sub("^PREDICTIONS_EXTERNES_(.*)\\.csv$", "\\1", nom_fichier)
  dossier_cible <- dirname(chemin_csv)
  
  cat(sprintf("Traitement -> Composé : %s | Dossier : %s\n", compose, basename(dossier_cible)))
  
  df <- tryCatch({
    read.csv(chemin_csv, sep = ";", stringsAsFactors = FALSE)
  }, error = function(e) {
    cat(sprintf("  [ERREUR] Impossible de lire %s\n", chemin_csv))
    return(NULL)
  })
  
  if (is.null(df) || nrow(df) == 0) {
    next
  }
  
  y_true <- df$Vraie_Valeur
  y_pred <- df$Valeur_Predite
  
  if (is.null(y_true) || is.null(y_pred) || length(y_true) == 0) {
    next
  }
  
  # Calculs des métriques
  rmsep <- sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
  ss_res <- sum((y_true - y_pred)^2, na.rm = TRUE)
  ss_tot <- sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
  r2p <- ifelse(ss_tot == 0, 0, 1 - (ss_res / ss_tot))
  rpd <- ifelse(rmsep == 0, 0, sd(y_true, na.rm = TRUE) / rmsep)
  
  # Textes du graphique
  titre_graph <- sprintf("RF Predictions vs Mesures : %s", compose)
  sous_titre_graph <- sprintf("R2p = %.3f    |    RMSEp = %.3f    |    RPD = %.3f", r2p, rmsep, rpd)
  
  # Création du graphique ggplot2 (Style publication épuré)
  p <- ggplot(data = df, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
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
  
  # Chemins de sauvegarde (PDF et PNG directement dans le dossier iter_X)
  chemin_pdf <- file.path(dossier_cible, sprintf("SCATTER_PUBLI_RF_%s.pdf", compose))
  chemin_png <- file.path(dossier_cible, sprintf("SCATTER_PUBLI_RF_%s.png", compose))
  
  ggsave(chemin_pdf, plot = p, width = 7, height = 5, dpi = 300)
  ggsave(chemin_png, plot = p, width = 7, height = 5, dpi = 300)
}

cat("\n========================================================\n")
cat(" TERMINÉ ! Tous les graphiques ont été générés dans leurs dossiers respectifs.\n")
cat("========================================================\n")