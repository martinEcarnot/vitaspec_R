# ==============================================================================
# SCRIPT COMPLET : VUE GLOBALE (TOUS GROUPES) AVEC COMPARAISON DES ALGORITHMES
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

# Fusion actuelle (PLS + RF) :
df_brut <- bind_rows(df_pls, df_rf)

# --- LE DICTIONNAIRE EXACT DES TISSUS ---
df_brut <- df_brut %>%
  mutate(Tissu = case_when(
    Tissu == "meso_frais"  ~ "Mésocarpe Frais",
    Tissu == "meso_silica" ~ "Mésocarpe Sec",
    Tissu == "HR"          ~ "Huile Rouge",
    TRUE ~ Tissu
  ))

# Listes exactes des composés
comp_g1 <- c("C14.0", "C16.0", "C18.0", "C18.1n9", "C18.2")
comp_g2 <- c("trans.alpha.carotene", "trans.beta.carotene", "X13.cis.beta.carotene", "X9.cis.beta.carotene")
comp_g3 <- c("aT", "aT3", "gT3")

# Filtrage et création des groupes (bien qu'on ne colore plus par groupe, on le garde pour l'ordre)
df_r2 <- df_brut %>%
  filter(Compose %in% c(comp_g1, comp_g2, comp_g3)) %>%
  mutate(Groupe = case_when(
    Compose %in% comp_g1 ~ "Acides Gras",
    Compose %in% comp_g2 ~ "Caroténoïdes",
    Compose %in% comp_g3 ~ "Tocophérols"
  ))

# Sécurisation de l'ordre d'affichage
df_r2$Tissu <- factor(df_r2$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))
df_r2$Algorithme <- factor(df_r2$Algorithme, levels = c("PLSr", "RF", "XGBoost"))

# ==============================================================================
# 2. L'ASTUCE DES ESPACES SUR L'AXE X
# ==============================================================================
# On réintroduit les trous blancs entre les familles chimiques
niveaux_axe_x <- c(
  "C14.0", "C16.0", "C18.0", "C18.1n9", "C18.2",  
  "ESPACE_1",                                     
  "trans.alpha.carotene", "trans.beta.carotene", "X13.cis.beta.carotene", "X9.cis.beta.carotene", 
  "ESPACE_2",                                     
  "aT", "aT3", "gT3"                              
)

df_r2$Compose <- factor(df_r2$Compose, levels = niveaux_axe_x)

# On prépare la liste pour masquer les mots "ESPACE" sur le graphique
vrais_composes <- niveaux_axe_x[!grepl("ESPACE", niveaux_axe_x)]

# Calcul précis du centre des groupes (incluant les espaces) pour placer les titres
df_titres_groupes <- data.frame(
  X_center = c(3, 8.5, 13), 
  Y_pos = 1.20, # On monte un peu le texte pour laisser la place aux R2
  Label = c("ACIDES GRAS", "CAROTÉNOÏDES", "TOCOPHÉROLS")
)


# ==============================================================================
# 3. FONCTION DE CRÉATION DE PDF PAR TISSU
# ==============================================================================
exporter_pdf_comparatif_global <- function(donnees_totales, nom_tissu, chemin_fichier_pdf) {
  
  donnees_tissu <- donnees_totales %>% filter(Tissu == nom_tissu)
  if(nrow(donnees_tissu) == 0) return(NULL)
  
  # On allonge un peu la largeur du PDF pour que les 3 algos rentrent bien
  pdf(file = chemin_fichier_pdf, width = 12, height = 6.5)
  
  graphique <- ggplot(donnees_tissu, aes(x = Compose, y = R2, fill = Algorithme)) +
    
    # Création des barres : position_dodge met les algos côte à côte
    geom_col(position = position_dodge(width = 0.85), color = "black", alpha = 0.95, width = 0.85) +
    
    # Ajout des valeurs de R2 au-dessus de chaque barre (taille réduite à 3 pour ne pas se chevaucher)
    geom_text(aes(label = sprintf("%.2f", R2), y = R2 + 0.04), 
              position = position_dodge(width = 0.85),
              size = 3.2, fontface = "bold", na.rm = TRUE) +
    
    # Titres flottants des groupes (Acides Gras, etc.)
    geom_text(data = df_titres_groupes, aes(x = X_center, y = Y_pos, label = Label), 
              inherit.aes = FALSE, fontface = "bold", size = 5.5, color = "gray20") +
    
    # Le bandeau gris contenant le nom du tissu (ex: Mésocarpe Sec)
    facet_wrap(~ Tissu) +
    
    # Force l'axe X à afficher les trous, tout en cachant le mot "ESPACE"
    scale_x_discrete(drop = FALSE, breaks = vrais_composes) +
    
    # Couleurs des Algorithmes
    scale_fill_manual(values = c("PLSr" = "#E15759",      # Rouge
                                 "RF" = "#59A14F",        # Vert
                                 "XGBoost" = "#4E79A7"))+ # Bleu
    
    # Axe Y qui monte à 1.30 pour garantir que rien ne se superpose
    scale_y_continuous(limits = c(0, 1.30), breaks = seq(0, 1, by = 0.2)) +
    
    labs(title = "Comparaison globale des modèles de prédiction",
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
      # Noms des molécules inclinés
      axis.text.x = element_text(angle = 35, hjust = 1, size = 11, face = "bold", color = "black"),
      
      # Légende en bas
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 13, face = "bold"),
      legend.key.size = unit(0.8, "cm"),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(graphique)
  dev.off()
  
  cat("-> PDF généré :", basename(chemin_fichier_pdf), "\n")
}

# ==============================================================================
# 4. GÉNÉRATION DES FICHIERS PDF
# ==============================================================================
dossier_images <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/"
if(!dir.exists(dossier_images)) dir.create(dossier_images, recursive = TRUE)

exporter_pdf_comparatif_global(df_r2, "Huile Rouge", paste0(dossier_images, "Comparaison_Globale_Huile_Rouge.pdf"))
exporter_pdf_comparatif_global(df_r2, "Mésocarpe Frais", paste0(dossier_images, "Comparaison_Globale_Meso_Frais.pdf"))
exporter_pdf_comparatif_global(df_r2, "Mésocarpe Sec", paste0(dossier_images, "Comparaison_Globale_Meso_Sec.pdf"))

cat("Terminé ! Les 3 PDF (vue panoramique) sont dans votre dossier 'visu'.\n")