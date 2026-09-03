# ==============================================================================
# SCRIPT COMPLET : COMPARAISON DES ALGORITHMES (PLS vs RF vs XGB) PAR GROUPE
# ==============================================================================

library(ggplot2)
library(dplyr)
library(readxl)

# ==============================================================================
# 1. IMPORTATION ET PRÉPARATION DES DONNÉES
# ==============================================================================
chemin_pls <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER/PRODUCTION/METRIQUES_PERFORMANCES_PLS.xlsx"
chemin_rf <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER/PRODUCTION/METRIQUES_PERFORMANCES_RANDOM_FOREST.xlsx"
# chemin_xgb <- "C:/Users/U108-N806/.../METRIQUES_PERFORMANCES_XGBOOST.xlsx" # À décommenter plus tard !

df_pls <- read_excel(chemin_pls) %>% 
  select(Tissu, Compose, R2 = R2p_Attendu) %>% 
  mutate(Algorithme = "PLSr")

df_rf <- read_excel(chemin_rf) %>% 
  select(Tissu, Compose, R2 = R2p_Attendu) %>% 
  mutate(Algorithme = "RF")

# Ligne préparée pour XGBoost : 
# df_xgb <- read_excel(chemin_xgb) %>% select(Tissu, Compose, R2 = R2p_Attendu) %>% mutate(Algorithme = "XGBoost")
# df_brut <- bind_rows(df_pls, df_rf, df_xgb)

# Pour l'instant, on fusionne juste PLS et RF :
df_brut <- bind_rows(df_pls, df_rf)

# --- LE DICTIONNAIRE EXACT DES TISSUS ---
df_brut <- df_brut %>%
  mutate(Tissu = case_when(
    Tissu == "meso_frais" ~ "Mésocarpe Frais",
    Tissu == "meso_silica"   ~ "Mésocarpe Sec",
    Tissu == "HR"         ~ "Huile Rouge",
    TRUE ~ Tissu
  ))

# Listes exactes des composés
comp_g1 <- c("C14.0", "C16.0", "C18.0", "C18.1n9", "C18.2")
comp_g2 <- c("trans.alpha.carotene", "trans.beta.carotene", "X13.cis.beta.carotene", "X9.cis.beta.carotene")
comp_g3 <- c("aT", "aT3", "gT3")

# Filtrage et création des groupes
df_r2 <- df_brut %>%
  filter(Compose %in% c(comp_g1, comp_g2, comp_g3)) %>%
  mutate(Groupe = case_when(
    Compose %in% comp_g1 ~ "Acides Gras",
    Compose %in% comp_g2 ~ "Caroténoïdes",
    Compose %in% comp_g3 ~ "Tocophérols"
  ))

# Sécurisation de l'ordre d'affichage (Tissus, Groupes, Composés, Algorithmes)
df_r2$Tissu <- factor(df_r2$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))
df_r2$Groupe <- factor(df_r2$Groupe, levels = c("Acides Gras", "Caroténoïdes", "Tocophérols"))
df_r2$Algorithme <- factor(df_r2$Algorithme, levels = c("PLSr", "RF", "XGBoost"))

# Ordre des composés sur l'axe X (plus besoin des espaces !)
ordre_composes <- c(comp_g1, comp_g2, comp_g3)
df_r2$Compose <- factor(df_r2$Compose, levels = ordre_composes)


# ==============================================================================
# 2. FONCTION DE CRÉATION DE PDF PAR TISSU
# ==============================================================================
# Cette fonction va créer 1 PDF contenant 3 pages (une par groupe chimique)
exporter_pdf_comparatif <- function(donnees_totales, nom_tissu, chemin_fichier_pdf) {
  
  # On isole toutes les données du tissu demandé
  donnees_tissu <- donnees_totales %>% filter(Tissu == nom_tissu)
  
  # Si le tissu n'a aucune donnée, on annule pour éviter un crash
  if(nrow(donnees_tissu) == 0) return(NULL)
  
  pdf(file = chemin_fichier_pdf, width = 10, height = 6.5)
  
  # Boucle sur les groupes chimiques (Acides Gras -> Carotènes -> Tocophérols)
  for (nom_groupe in levels(donnees_tissu$Groupe)) {
    
    donnees_page <- donnees_tissu %>% filter(Groupe == nom_groupe)
    
    # Si le groupe est vide dans ce tissu, on passe au suivant
    if(nrow(donnees_page) == 0) next 
    
    # Création d'une variable factice pour avoir le joli bandeau gris en haut
    donnees_page$Titre_Bandeau <- paste(nom_tissu, "-", nom_groupe)
    
    graphique <- ggplot(donnees_page, aes(x = Compose, y = R2, fill = Algorithme)) +
      
      # L'astuce "position_dodge" met les barres côte à côte
      geom_col(position = position_dodge(width = 0.8), color = "black", alpha = 0.95, width = 0.7) +
      
      # On doit appliquer le même dodge sur le texte pour qu'il suive sa barre
      geom_text(aes(label = sprintf("%.2f", R2), y = R2 + 0.05), 
                position = position_dodge(width = 0.8),
                size = 3.8, fontface = "bold", na.rm = TRUE) +
      
      # Le bandeau gris avec le nom "Tissu - Groupe"
      facet_wrap(~ Titre_Bandeau) +
      
      # Couleurs distinctes pour les algorithmes
      scale_fill_manual(values = c("PLSr" = "#E15759",      # Bleu classique
                                   "RF" = "#59A14F",        # Vert forêt
                                   "XGBoost" = "#4E79A7"))+ # Rouge/Orange (quand il sera ajouté)
      
      scale_y_continuous(limits = c(0, 1.20), breaks = seq(0, 1, by = 0.2)) +
      
      labs(title = "Comparaison des Algorithmes de Prédiction",
           x = NULL, 
           y = expression(bold(R^2~p))) +
      
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 15)),
        strip.text = element_text(size = 14, face = "bold", color = "black"),
        strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.8),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 35, hjust = 1, size = 12, face = "bold", color = "black"),
        
        # LA LÉGENDE REVIENT (en bas, horizontale)
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    print(graphique)
  }
  
  dev.off()
  cat("-> PDF généré :", basename(chemin_fichier_pdf), "\n")
}

# ==============================================================================
# 3. GÉNÉRATION DES FICHIERS PDF
# ==============================================================================
dossier_images <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/"
if(!dir.exists(dossier_images)) dir.create(dossier_images, recursive = TRUE)

# Génération d'un PDF pour chaque Tissu
exporter_pdf_comparatif(df_r2, "Huile Rouge", paste0(dossier_images, "Comparaison_Modeles_Huile_Rouge.pdf"))
exporter_pdf_comparatif(df_r2, "Mésocarpe Frais", paste0(dossier_images, "Comparaison_Modeles_Meso_Frais.pdf"))
exporter_pdf_comparatif(df_r2, "Mésocarpe Sec", paste0(dossier_images, "Comparaison_Modeles_Meso_Sec.pdf"))

cat("Terminé ! Les 3 PDF sont dans votre dossier 'visu'.\n")