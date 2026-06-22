library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr) # Nécessaire pour formater le tableau

# 1. Définition des chemins et paramètres
d0 <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/Results"
d_save_base <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/RMSEP"

methodes <- c("pls", "random_forest", "xgboost")
idparams <- c("meso_silica", "meso_frais", "HR") # CORRIGÉ ICI

# Listes des composés
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

# 2. Initialisation du dataframe
results_df <- data.frame(
  Methode = character(),
  Idparam = character(),
  Compose = character(),
  RMSEP_Externe = numeric(),
  stringsAsFactors = FALSE
)

# 3. Boucle d'extraction
for (methode in methodes) {
  for (idparam in idparams) {
    
    # CORRIGÉ ICI (meso_silica au lieu de meso_sec)
    composes <- if (idparam %in% c("meso_silica", "meso_frais")) composants_meso else composants_hr
    
    for (compose in composes) {
      file_path <- file.path(d0, methode, "moyennes", "Results", idparam, compose, paste0("rapport_A_", compose, ".json"))
      
      if (file.exists(file_path)) {
        json_data <- tryCatch({ fromJSON(file_path) }, error = function(e) NULL)
        
        if (!is.null(json_data) && "Crash_Test_Externe" %in% names(json_data)) {
          rmsep_val <- json_data$Crash_Test_Externe$RMSEP_Externe
          if (!is.null(rmsep_val)) {
            results_df <- rbind(results_df, data.frame(
              Methode = methode,
              Idparam = idparam,
              Compose = compose,
              RMSEP_Externe = as.numeric(rmsep_val),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
}

# 4. NOUVELLE LOGIQUE : Création des dossiers, tableaux et graphiques par COMPOSÉ
tous_les_composes <- unique(c(composants_meso, composants_hr))

for (compose_actuel in tous_les_composes) {
  
  # Récupérer toutes les données de ce composé spécifique
  df_compose <- results_df %>% filter(Compose == compose_actuel)
  
  if (nrow(df_compose) > 0) {
    
    # Création du dossier unique du composé : "visu/RMSEP/{compose}/"
    dossier_sauvegarde <- file.path(d_save_base, compose_actuel)
    if (!dir.exists(dossier_sauvegarde)) {
      dir.create(dossier_sauvegarde, recursive = TRUE)
    }
    
    # --- A. CRÉATION DU TABLEAU ---
    # On transforme le format long en format large (les tissus deviennent des colonnes)
    df_table <- df_compose %>%
      select(Methode, Compose, Idparam, RMSEP_Externe) %>%
      pivot_wider(names_from = Idparam, values_from = RMSEP_Externe)
    
    # Sécurité : Si un composé n'existe pas dans un tissu (ex: lycopene absent de meso_frais), 
    # la colonne n'est pas créée. On force sa création avec des NA pour avoir toujours la même structure.
    for (col in idparams) {
      if (!col %in% names(df_table)) df_table[[col]] <- NA
    }
    
    # Réorganisation propre des colonnes
    df_table <- df_table %>% select(Methode, Compose, all_of(idparams))
    
    # Sauvegarde en CSV (j'utilise write.csv2 pour que Excel l'ouvre proprement en France avec les ";" et ",")
    write.csv2(
      df_table, 
      file.path(dossier_sauvegarde, paste0("Tableau_RMSEP_", compose_actuel, ".csv")), 
      row.names = FALSE
    )
    
    # --- B. CRÉATION DES BARPLOTS ---
    # Pour ce composé, on génère un graphique pour chaque tissu disponible
    tissus_presents <- unique(df_compose$Idparam)
    
    for (idparam_actuel in tissus_presents) {
      
      df_plot <- df_compose %>% filter(Idparam == idparam_actuel)
      df_plot$Methode <- factor(df_plot$Methode, levels = c("pls", "random_forest", "xgboost"))
      
      p <- ggplot(df_plot, aes(x = Methode, y = RMSEP_Externe, fill = Methode)) +
        geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.5) +
        scale_fill_manual(values = c("pls" = "#F8766D", "random_forest" = "#33a02c", "xgboost" = "#1f78b4")) +
        scale_x_discrete(labels = c("PLS", "RF", "XGB")) +
        labs(x = "Méthode", y = "RMSEP Externe") +
        theme_bw() + 
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5), 
          panel.border = element_rect(color = "black", linewidth = 1), 
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.2, "cm"), 
          axis.text.x = element_text(margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 5)),
          legend.position = "none"
        )
      
      nom_fichier_base <- paste0("Barplot_", idparam_actuel, "_", compose_actuel)
      
      # Sauvegarde dans le dossier unique du composé
      ggsave(file.path(dossier_sauvegarde, paste0(nom_fichier_base, ".png")), plot = p, width = 7, height = 5, dpi = 300)
      ggsave(file.path(dossier_sauvegarde, paste0(nom_fichier_base, ".pdf")), plot = p, width = 7, height = 5)
    }
  }
}
# 5. GRAPHIQUE GLOBAL : Synthèse de tous les composés et tissus


# On crée une copie propre des données pour l'affichage final
df_global <- results_df %>%
  mutate(
    # On renomme proprement pour la légende et les axes
    Methode = factor(Methode, levels = c("pls", "random_forest", "xgboost"), labels = c("PLS", "RF", "XGB")),
    Idparam = factor(Idparam, levels = c("meso_silica", "meso_frais", "HR"), labels = c("Méso Silica", "Méso Frais", "Huile Rouge"))
  )

# Création du grand graphique
p_global <- ggplot(df_global, aes(x = Idparam, y = RMSEP_Externe, fill = Methode)) +
  # Barres groupées par tissu, un peu plus fines
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", linewidth = 0.4, width = 0.7) +
  
  # Le coeur du système : un sous-graphique par COMPOSÉ
  # scales = "free_y" est absolument OBLIGATOIRE ici car l'erreur sur l'eau et sur un carotène trace n'ont rien à voir
  facet_wrap(~ Compose, scales = "free_y", ncol = 5) + 
  
  # Les couleurs académiques que tu as validées
  scale_fill_manual(values = c("PLS" = "#F8766D", "RF" = "#33a02c", "XGB" = "#1f78b4")) +
  
  labs(
    x = NULL, # Pas besoin de titre d'axe X, "Méso Silica", etc. parlent d'eux-mêmes
    y = "RMSEP Externe"
  ) +
  
  # Esthétique "Statistica" appliquée au grand format
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5), 
    panel.border = element_rect(color = "black", linewidth = 1), 
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"), 
    
    # On incline légèrement le texte de l'axe X pour éviter que les noms de tissus ne se chevauchent
    axis.text.x = element_text(angle = 35, hjust = 1, margin = margin(t = 6), size = 8), 
    axis.text.y = element_text(margin = margin(r = 5), size = 8),
    
    # Titres des facets
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold", size = 9, color = "black"),
    
    # Sur le graph global, on a besoin de la légende ! On la place en bas.
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 12, face = "bold"),
    
    # Espace entre les multiples sous-graphiques
    panel.spacing = unit(1, "lines") 
  )

# On sauvegarde ce grand graphique à la racine (d_save_base)
# Dimensions généreuses (16x10 pouces) pour accommoder la vingtaine de graphiques
ggsave(file.path(d_save_base, "Graphique_Synthese_Globale.png"), plot = p_global, width = 16, height = 10, dpi = 300)
ggsave(file.path(d_save_base, "Graphique_Synthese_Globale.pdf"), plot = p_global, width = 16, height = 10)
