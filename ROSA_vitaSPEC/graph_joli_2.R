library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. SAISIE DES RÉSULTATS (Valeurs de R2p)
# ==============================================================================
df_r2 <- data.frame(
  Algorithme = c(rep("PLSr", 21), rep("RF", 21), rep("XGB", 21)),
  Tissu = rep(c(rep("Huile Rouge", 7), rep("Mésocarpe Frais", 7), rep("Mésocarpe Sec", 7)), 3),
  
  # LA NOUVEAUTÉ : La colonne "Groupe"
  Groupe = rep(c(rep("Acides Gras", 3), rep("Caroténoïdes", 4)), 9),
  
  Compose = rep(c("Acide palmitique", "Acide oléique", "Acide linoléique", 
                  "trans-a-carotène", "trans-b-carotène", "b-carotènes Totaux", "Carotènes Totaux"), 9),
  
  R2 = c(
    # --- 1. VALEURS PLSr ---
    0.89, 0.96, 0.96, 0.78, 0.82, 0.90, 0.89, # Huile
    0.80, 0.79, 0.59, 0.64, 0.68, 0.69, 0.81, # Frais
    0.87, 0.90, 0.83, 0.80, 0.70, 0.73, 0.82, # Sec
    
    # --- 2. VALEURS Random Forest (RF) ---
    0.85, 0.92, 0.91, 0.75, 0.80, 0.88, 0.86, # Huile
    0.78, 0.75, 0.61, 0.67, 0.65, 0.71, 0.79, # Frais
    0.89, 0.91, 0.85, 0.82, 0.75, 0.77, 0.85, # Sec
    
    # --- 3. VALEURS XGBoost ---
    NA, NA, NA, NA, NA, NA, NA,               
    NA, NA, NA, NA, NA, NA, NA,               
    NA, NA, NA, NA, NA, NA, NA                
  )
)

# Ordre d'affichage (Facteurs)
df_r2$Tissu <- factor(df_r2$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))
df_r2$Groupe <- factor(df_r2$Groupe, levels = c("Acides Gras", "Caroténoïdes")) # Ordre des groupes
df_r2$Compose <- factor(df_r2$Compose, levels = c("Acide palmitique", "Acide oléique", "Acide linoléique", 
                                                  "trans-a-carotène", "trans-b-carotène", "b-carotènes Totaux", "Carotènes Totaux"))

# ==============================================================================
# 2. FONCTION DE CRÉATION DE PDF MULTIPAGE (AVEC GROUPES COLLÉS)
# ==============================================================================
exporter_pdf_multipage <- function(donnees_algo, nom_algo, chemin_fichier_pdf) {
  
  pdf(file = chemin_fichier_pdf, width = 11, height = 6.5)
  
  for (nom_tissu in levels(donnees_algo$Tissu)) {
    
    donnees_page <- donnees_algo %>% filter(Tissu == nom_tissu)
    
    graphique <- ggplot(donnees_page, aes(x = Compose, y = R2, fill = Tissu)) +
      
      # width = 1 permet de coller les barres à l'intérieur d'un même groupe
      geom_col(color = "black", alpha = 1, width = 1) +
      
      geom_text(aes(label = sprintf("%.2f", R2), y = R2 + 0.05), 
                size = 4.5, fontface = "bold", na.rm = TRUE) +
      
      # LA MAGIE DU REGROUPEMENT : On sépare par "Groupe" avec un espacement proportionnel
      facet_grid(~ Groupe, scales = "free_x", space = "free_x") +
      
      scale_fill_manual(values = c("Huile Rouge" = "#DE6E26",   
                                   "Mésocarpe Frais" = "#38A882", 
                                   "Mésocarpe Sec" = "#F0B423")) + 
      
      scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, by = 0.2)) +
      
      # Le Tissu passe dans le titre principal
      labs(title = paste("Performances de prédiction (", nom_algo, ") -", nom_tissu),
           x = NULL, 
           y = expression(bold(R^2~p))) +
      
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 15)),
        
        # Esthétique de la barre grise qui contient désormais le nom du GROUPE
        strip.text = element_text(size = 14, face = "bold", color = "black"),
        strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.8),
        
        # Espace (le "trou") entre les différents groupes chimiques
        panel.spacing = unit(1, "lines"), 
        
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        
        # Mots penchés pour la lisibilité
        axis.text.x = element_text(angle = 40, hjust = 1, size = 12, face = "bold", color = "black"),
        
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    print(graphique)
  }
  
  dev.off()
  cat("-> PDF généré :", basename(chemin_fichier_pdf), "\n")
}

# ==============================================================================
# 3. GÉNÉRATION DES 3 FICHIERS PDF
# ==============================================================================
dossier_images <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/"

exporter_pdf_multipage(df_r2 %>% filter(Algorithme == "PLSr"), "PLSr", 
                       paste0(dossier_images, "R2_PLSr_Profil_Chimique_Groupes.pdf"))

exporter_pdf_multipage(df_r2 %>% filter(Algorithme == "RF"), "Random Forest", 
                       paste0(dossier_images, "R2_RF_Profil_Chimique_Groupes.pdf"))

exporter_pdf_multipage(df_r2 %>% filter(Algorithme == "XGB"), "XGBoost", 
                       paste0(dossier_images, "R2_XGB_Profil_Chimique_Groupes.pdf"))