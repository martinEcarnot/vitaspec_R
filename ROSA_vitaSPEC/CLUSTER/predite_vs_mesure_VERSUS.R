# ==============================================================================
# SCRIPT R : Comparaison RF (Vert) vs PLS (Orange) - Version Stylisée
# ==============================================================================

library(ggplot2)
library(ggtext) # Nécessaire pour afficher les sous-titres en couleurs multiples

# --- 1. CONFIGURATION LOCALE ---
root_path <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER"
rf_root <- file.path(root_path, "random_forest", "moyennes", "test_final")
pls_root <- file.path(root_path, "pls", "test_final")
output_root <- file.path(root_path, "predit_vs_mesure")

# Fonction de conversion numérique sécurisée
to_numeric_safe <- function(col) {
  if (is.null(col)) return(NULL)
  if (is.numeric(col)) return(col)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(col))))
}

# --- 2. RECHERCHE DES FICHIERS RF ---
rf_files <- list.files(rf_root, pattern = "^PREDICTIONS_EXTERNES_.*\\.csv$", recursive = TRUE, full.names = TRUE)

cat(sprintf("==> %d fichiers de référence RF trouvés. Traitement en cours...\n\n", length(rf_files)))

succes_counter <- 0
missing_pls_counter <- 0

# --- 3. BOUCLE DE TRAITEMENT ET FUSION ---
for (rf_file in rf_files) {
  
  parts <- unlist(strsplit(rf_file, "/|\\\\"))
  n <- length(parts)
  
  compose_file <- parts[n]
  iter_folder  <- parts[n - 1]
  compose      <- sub("^PREDICTIONS_EXTERNES_(.*)\\.csv$", "\\1", compose_file)
  tissu        <- parts[n - 3]
  
  pls_file <- file.path(pls_root, tissu, compose, iter_folder, compose_file)
  
  if (!file.exists(pls_file)) {
    missing_pls_counter <- missing_pls_counter + 1
    next
  }
  
  df_rf <- tryCatch(read.csv(rf_file, sep = ";", stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  df_pls <- tryCatch(read.csv(pls_file, sep = ";", stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  
  if (is.null(df_rf) || is.null(df_pls) || nrow(df_rf) == 0 || nrow(df_pls) == 0) next
  
  # Détection des colonnes
  cols_rf <- colnames(df_rf)
  col_true_rf <- cols_rf[grepl("Vraie_Valeur|Observed|Measured|y_true", cols_rf, ignore.case = TRUE)][1]
  col_pred_rf <- cols_rf[grepl("Valeur_Predite|Predicted|y_pred|pred", cols_rf, ignore.case = TRUE)][1]
  
  cols_pls <- colnames(df_pls)
  col_true_pls <- cols_pls[grepl("Vraie_Valeur|Observed|Measured|y_true", cols_pls, ignore.case = TRUE)][1]
  col_pred_pls <- cols_pls[grepl("Valeur_Predite|Predicted|y_pred|pred", cols_pls, ignore.case = TRUE)][1]
  
  if (is.na(col_true_rf) || is.na(col_pred_rf) || is.na(col_true_pls) || is.na(col_pred_pls)) next
  
  y_true_rf <- to_numeric_safe(df_rf[[col_true_rf]])
  y_pred_rf <- to_numeric_safe(df_rf[[col_pred_rf]])
  y_true_pls <- to_numeric_safe(df_pls[[col_true_pls]])
  y_pred_pls <- to_numeric_safe(df_pls[[col_pred_pls]])
  
  if (is.null(y_true_rf) || is.null(y_pred_rf) || is.null(y_true_pls) || is.null(y_pred_pls)) next
  
  # --- CALCUL DES MÉTRIQUES PAR MODÈLE ---
  # Random Forest
  rmsep_rf <- sqrt(mean((y_true_rf - y_pred_rf)^2, na.rm = TRUE))
  r2p_rf <- 1 - (sum((y_true_rf - y_pred_rf)^2, na.rm = TRUE) / sum((y_true_rf - mean(y_true_rf, na.rm = TRUE))^2, na.rm = TRUE))
  rpd_rf <- sd(y_true_rf, na.rm = TRUE) / rmsep_rf
  
  # PLS
  rmsep_pls <- sqrt(mean((y_true_pls - y_pred_pls)^2, na.rm = TRUE))
  r2p_pls <- 1 - (sum((y_true_pls - y_pred_pls)^2, na.rm = TRUE) / sum((y_true_pls - mean(y_true_pls, na.rm = TRUE))^2, na.rm = TRUE))
  rpd_pls <- sd(y_true_pls, na.rm = TRUE) / rmsep_pls
  
  # Construction des sous-titres colorés en HTML/CSS via ggtext
  sous_titre_html <- sprintf(
    "<span style='color:#27AE60;'><b>Random Forest</b> : R²p = %.3f | RMSEp = %.3f | RPD = %.3f</span><br><span style='color:#E67E22;'><b>PLS</b> : R²p = %.3f | RMSEp = %.3f | RPD = %.3f</span>",
    r2p_rf, rmsep_rf, rpd_rf, r2p_pls, rmsep_pls, rpd_pls
  )
  
  # Assemblage des données pour ggplot
  plot_data_rf <- data.frame(Vraie_Valeur = y_true_rf, Valeur_Predite = y_pred_rf, Modele = "Random Forest")
  plot_data_pls <- data.frame(Vraie_Valeur = y_true_pls, Valeur_Predite = y_pred_pls, Modele = "PLS")
  combined_df <- rbind(plot_data_rf, plot_data_pls)
  
  output_dir <- file.path(output_root, tissu, compose, iter_folder)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- CRÉATION DU GRAPHIQUE ---
  p <- ggplot(combined_df, aes(x = Valeur_Predite, y = Vraie_Valeur, color = Modele, fill = Modele)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    # Points avec contour fin et remplissage transparent
    geom_point(shape = 1, size = 2.2, stroke = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    # Couleurs des contours (opacité 100%)
    scale_color_manual(values = c("Random Forest" = "#27AE60", "PLS" = "#E67E22")) +
    # Couleurs de remplissage (semi-transparentes à 40%)
    scale_fill_manual(values = c(
      "Random Forest" = scales::alpha("#27AE60", 0.4), 
      "PLS" = scales::alpha("#E67E22", 0.4)
    )) +
    labs(
      title = sprintf("Comparaison RF vs PLS : %s (%s)", compose, tissu),
      subtitle = sous_titre_html, # Intégration du texte multi-couleurs
      x = "Valeurs predites",
      y = "Valeurs mesurees",
      color = "Modèle",
      fill = "Modèle"
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(color = "#EBEBEB", linewidth = 0.8),
      axis.line.y = element_line(color = "#EBEBEB", linewidth = 0.8),
      plot.title = element_text(size = 12, color = "black", hjust = 0, margin = margin(b = 5)),
      # Utilisation de element_markdown pour interpréter les couleurs du sous-titre
      plot.subtitle = element_markdown(size = 9, hjust = 0, margin = margin(b = 15)),
      axis.title.x = element_text(size = 10, color = "black", margin = margin(t = 10)),
      axis.title.y = element_text(size = 10, color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 8, color = "#555555"),
      legend.position = "bottom"
    )
  
  chemin_pdf <- file.path(output_dir, sprintf("COMPARISON_RF_PLS_%s.pdf", compose))
  chemin_png <- file.path(output_dir, sprintf("COMPARISON_RF_PLS_%s.png", compose))
  
  ggsave(chemin_pdf, plot = p, width = 7, height = 5.5, dpi = 300)
  ggsave(chemin_png, plot = p, width = 7, height = 5.5, dpi = 300)
  
  succes_counter <- succes_counter + 1
}

cat(sprintf("\n========================================================\n"))
cat(sprintf(" TERMINÉ ! %d graphiques comparatifs mis à jour.\n", succes_counter))
cat(sprintf("========================================================\n"))