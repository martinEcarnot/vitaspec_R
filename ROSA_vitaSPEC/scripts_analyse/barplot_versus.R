library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. PARAMÈTRES ET CHEMINS
# ==============================================================================
d0 <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/Results"
d_save_racine <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu"

methodes <- c("pls", "random_forest", "xgboost")
idparams <- c("meso_frais", "meso_silica", "HR")

# Listes utilisées pour la LECTURE des dossiers (on garde les orthographes exactes des dossiers sources)
composants_meso <- c(
  "eau", "C14.0", "C16.0", "C18.0", "C18.1n9", "C18.1n7", "C18.2",
  "C18.3", "C20.0", "tlip.MS", "trans.alpha.carotene", "trans.beta.carotene",
  "total.trans.carotene.natif", "ratio.alpha.beta", "ratio.alpha.natif",
  "ratio.beta.natif", "X13.cis.beta.carotene", "X9.cis.beta.carotene",
  "total.beta.carotene", "total.carotene"
)

composants_hr <- c(
  "C14.0", "C16.0", "C18.0", "C18.1n9", "C18.2", "FFA",
  "trans.alpha.carotene", "trans.beta.carotene", "total.trans.carotenes.natif",
  "ratio.alpha.beta", "ratio.alpha.natif", "ratio.beta.natif",
  "X13.cis.beta.carotene", "X9.cis.beta.carotene", "total.beta.carotene",
  "total.carotene", "lycopene", "aT", "aT3", "gT3", "dT3", "total.T3", "total.toco"
)

# ==============================================================================
# 2. EXTRACTION OPTIMISÉE DES DONNÉES (RMSEP & R2p)
# ==============================================================================
results_df <- data.frame(
  Methode = character(), Idparam = character(), Compose = character(),
  Metrique = character(), Valeur = numeric(), stringsAsFactors = FALSE
)

message("Début de l'extraction des données JSON...")

for (methode in methodes) {
  for (idparam in idparams) {
    composes <- if (idparam %in% c("meso_silica", "meso_frais")) composants_meso else composants_hr
    
    for (compose in composes) {
      file_path <- file.path(d0, methode, "moyennes", "Results", idparam, compose, paste0("rapport_A_", compose, ".json"))
      
      if (file.exists(file_path)) {
        json_data <- tryCatch({ fromJSON(file_path) }, error = function(e) NULL)
        
        if (!is.null(json_data) && "Crash_Test_Externe" %in% names(json_data)) {
          val_rmsep <- json_data$Crash_Test_Externe$RMSEP_Externe
          if (!is.null(val_rmsep)) {
            results_df <- rbind(results_df, data.frame(
              Methode = methode, Idparam = idparam, Compose = compose, 
              Metrique = "RMSEP", Valeur = as.numeric(val_rmsep), stringsAsFactors = FALSE
            ))
          }
          
          val_r2p <- json_data$Crash_Test_Externe$R2p_Externe
          if (!is.null(val_r2p)) {
            results_df <- rbind(results_df, data.frame(
              Methode = methode, Idparam = idparam, Compose = compose, 
              Metrique = "R2p", Valeur = as.numeric(val_r2p), stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
}

# ==============================================================================
# 2.5 HARMONISATION DES NOMS DE COMPOSÉS (Correction de la faute de frappe)
# ==============================================================================
message("Harmonisation des noms de composés...")

# On force le nom au pluriel à devenir le nom au singulier pour tout regrouper
results_df <- results_df %>%
  mutate(Compose = ifelse(Compose == "total.trans.carotenes.natif", "total.trans.carotene.natif", Compose))

# La liste des composés à traiter est maintenant déduite automatiquement du tableau propre !
tous_les_composes <- unique(results_df$Compose)

# ==============================================================================
# 3. GÉNÉRATION DES DOSSIERS, TABLEAUX ET GRAPHIQUES UNIFIÉS
# ==============================================================================
metriques_a_traiter <- c("RMSEP", "R2p")

theme_gris_epure <- theme_gray(base_size = 15) + theme(
  
  # Suppression de l'axe X (inchangé)
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  
  # Titre de l'axe Y ("RMSEP" ou "R²p")
  axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 10)), 
  
  # Chiffres de l'axe Y
  axis.text.y = element_text(size = 13),
  
  # Textes des facettes en haut ("Frais", "Sec", "HR")
  strip.text = element_text(size = 15, face = "bold"),
  
  # Paramètres de la légende à droite ("PLS", "RF", "XGB")
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 13) 
)

couleurs_methodes <- scale_fill_discrete(labels = c("PLS", "RF", "XGB"))

for (metrique_actuelle in metriques_a_traiter) {
  
  message(sprintf("Génération des résultats pour la métrique : %s...", metrique_actuelle))
  
  d_save_metrique <- file.path(d_save_racine, metrique_actuelle)
  df_metrique <- results_df %>% filter(Metrique == metrique_actuelle)
  
  label_y <- if(metrique_actuelle == "RMSEP") "RMSEP" else "R²p"
  
  for (compose_actuel in tous_les_composes) {
    
    df_compose <- df_metrique %>% filter(Compose == compose_actuel)
    
    if (nrow(df_compose) > 0) {
      
      dossier_sauvegarde <- file.path(d_save_metrique, compose_actuel)
      if (!dir.exists(dossier_sauvegarde)) dir.create(dossier_sauvegarde, recursive = TRUE)
      
      # --- A. TABLEAU ---
      df_table <- df_compose %>%
        select(Methode, Compose, Idparam, Valeur) %>%
        pivot_wider(names_from = Idparam, values_from = Valeur)
      
      for (col in idparams) {
        if (!col %in% names(df_table)) df_table[[col]] <- NA
      }
      
      df_table <- df_table %>% select(Methode, Compose, all_of(idparams))
      write.csv2(df_table, file.path(dossier_sauvegarde, paste0("Tableau_", metrique_actuelle, "_", compose_actuel, ".csv")), row.names = FALSE)
      
      # Préparation des facteurs
      df_compose$Methode <- factor(df_compose$Methode, levels = c("pls", "random_forest", "xgboost"))
      df_compose$Idparam <- factor(df_compose$Idparam, 
                                   levels = c("HR", "meso_frais", "meso_silica"),
                                   labels = c("HUILE_ROUGE", "meso_FRAIS", "meso_SEC"))
      
      # --- B. GRAPHIQUES INDIVIDUELS ---
      for (idparam_actuel in unique(df_compose$Idparam)) {
        
        df_plot_indiv <- df_compose %>% filter(Idparam == idparam_actuel)
        
        p_indiv <- ggplot(df_plot_indiv, aes(x = Methode, y = Valeur, fill = Methode)) +
          geom_bar(stat = "identity", position = "dodge") + 
          labs(y = label_y) +
          couleurs_methodes +
          theme_gris_epure 
        
        ggsave(file.path(dossier_sauvegarde, paste0("Barplot_Indiv_", idparam_actuel, "_", compose_actuel, ".png")), plot = p_indiv, width = 7, height = 5, dpi = 300)
        ggsave(file.path(dossier_sauvegarde, paste0("Barplot_Indiv_", idparam_actuel, "_", compose_actuel, ".pdf")), plot = p_indiv, width = 7, height = 5)
      }
      
      # --- C. GRAPHIQUE COMPLET (Facet Grid) ---
      p_facet <- ggplot(df_compose, aes(x = Methode, y = Valeur, fill = Methode)) +
        geom_bar(stat = "identity", position = "dodge") + 
        facet_grid(. ~ Idparam) + 
        labs(y = label_y) +
        couleurs_methodes +
        theme_gris_epure 
      
      ggsave(file.path(dossier_sauvegarde, paste0("Barplot_Complet_Facet_", compose_actuel, ".png")), plot = p_facet, width = 7.5, height = 5, dpi = 300)
      ggsave(file.path(dossier_sauvegarde, paste0("Barplot_Complet_Facet_", compose_actuel, ".pdf")), plot = p_facet, width = 7.5, height = 5)
    }
  }
}

message("Exécution terminée avec succès !")