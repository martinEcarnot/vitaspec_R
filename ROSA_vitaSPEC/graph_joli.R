library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. SAISIE DES RÉSULTATS (Valeurs de R2p_Externe)
# ==============================================================================
df_r2 <- data.frame(
  Algorithme = c(rep("PLSr", 9), rep("RF", 9), rep("XGB", 9)),
  Compose = rep(c(rep("Acide Oléique", 3), rep("trans-α-carotène", 3), rep("Tocophérols", 3)), 3),
  Tissu = rep(c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"), 9),
  
  R2 = c(
    # --- 1. VALEURS PLSr (Exemples à remplacer par vos vrais résultats) ---
    0.88, 0.72, 0.81,  # Acide Oléique (Huile, Frais, Sec)
    0.68, 0.55, 0.62,  # trans-α-carotène (Huile, Frais, Sec)
    0.35, NA, NA,      # Tocophérols (NA pour Frais et Sec)
    
    # --- 2. VALEURS Random Forest (RF) ---
    0.79, 0.65, 0.72,  
    0.78, 0.68, 0.75,  
    0.40, NA, NA,      
    
    # --- 3. VALEURS XGBoost (Vos "trous" à remplir manuellement plus tard) ---
    NA, NA, NA,        
    NA, NA, NA,        
    NA, NA, NA         
  )
)

# Fixer l'ordre d'affichage pour que ce soit logique et esthétique
df_r2$Tissu <- factor(df_r2$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))
df_r2$Compose <- factor(df_r2$Compose, levels = c("Acide Oléique", "trans-α-carotène", "Tocophérols"))

# ==============================================================================
# 2. FONCTION DE CRÉATION DE GRAPHIQUE
# ==============================================================================
creer_graphique_r2 <- function(donnees, nom_algo) {
  
  ggplot(donnees, aes(x = Tissu, y = R2, fill = Tissu)) +
    
    # Création des 3 barres par diagramme (sans barres d'erreur)
    geom_col(color = "black", alpha = 0.9, width = 0.7) +
    
    # Ajout des valeurs au-dessus des barres
    geom_text(aes(label = sprintf("%.2f", R2), y = R2 + 0.05), 
              size = 4.5, fontface = "bold", na.rm = TRUE) +
    
    # Séparation en 3 diagrammes (Un par composé)
    facet_wrap(~ Compose) +
    
    # Couleurs distinctes pour les 3 tissus
    scale_fill_manual(values = c("Huile Rouge" = "#D95F02",   
                                 "Mésocarpe Frais" = "#1B9E77", 
                                 "Mésocarpe Sec" = "#E6A100")) + 
    
    # L'axe Y va toujours de 0 à 1 
    scale_y_continuous(limits = c(0, 1.10), breaks = seq(0, 1, by = 0.2)) +
    
    labs(title = paste("Performances de prédiction (", nom_algo, ")"),
         x = NULL, 
         y = expression(bold(R^2~externe))) +
    
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 15)),
      strip.text = element_text(size = 14, face = "bold", color = "black"),
      strip.background = element_rect(fill = "gray90", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 13, color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      panel.grid.major.x = element_blank()
    )
}

# ==============================================================================
# 3. GÉNÉRATION DES 3 GRAPHIQUES
# ==============================================================================
dossier_images <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/"

# 1. Graphique PLS
graph_pls <- creer_graphique_r2(df_r2 %>% filter(Algorithme == "PLSr"), "PLSr")
print(graph_pls)
ggsave(paste0(dossier_images, "R2_PLSr_Comparaison.png"), plot = graph_pls, width = 10, height = 5.5, dpi = 300)

# 2. Graphique RF
graph_rf <- creer_graphique_r2(df_r2 %>% filter(Algorithme == "RF"), "Random Forest")
print(graph_rf)
ggsave(paste0(dossier_images, "R2_RF_Comparaison.png"), plot = graph_rf, width = 10, height = 5.5, dpi = 300)

# 3. Graphique XGB (Vide)
graph_xgb <- creer_graphique_r2(df_r2 %>% filter(Algorithme == "XGB"), "XGBoost")
print(graph_xgb)
ggsave(paste0(dossier_images, "R2_XGB_Comparaison.png"), plot = graph_xgb, width = 10, height = 5.5, dpi = 300)