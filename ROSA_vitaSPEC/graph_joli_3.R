# ==============================================================================
# SCRIPT COMPLET : GÉNÉRATION DES GRAPHIQUES DE PERFORMANCES DEPUIS EXCEL
# ==============================================================================

library(ggplot2)
library(dplyr)
library(readxl)

# ==============================================================================
# 1. IMPORTATION ET PRÉPARATION DES DONNÉES
# ==============================================================================
chemin_pls <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER/PRODUCTION/METRIQUES_PERFORMANCES_PLS.xlsx"
chemin_rf <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/CLUSTER/PRODUCTION/METRIQUES_PERFORMANCES_RANDOM_FOREST.xlsx"

df_pls <- read_excel(chemin_pls) %>% 
  select(Tissu, Compose, R2 = R2p_Attendu) %>% 
  mutate(Algorithme = "PLSr")

df_rf <- read_excel(chemin_rf) %>% 
  select(Tissu, Compose, R2 = R2p_Attendu) %>% 
  mutate(Algorithme = "RF")

df_brut <- bind_rows(df_pls, df_rf)

# --- LE DICTIONNAIRE EXACT DES TISSUS ---
df_brut <- df_brut %>%
  mutate(Tissu = case_when(
    Tissu == "meso_frais" ~ "Mésocarpe Frais",
    Tissu == "meso_silica"   ~ "Mésocarpe Sec",
    Tissu == "HR"         ~ "Huile Rouge",
    TRUE ~ Tissu # Sécurité
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

# Ordre d'affichage
df_r2$Tissu <- factor(df_r2$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))
df_r2$Groupe <- factor(df_r2$Groupe, levels = c("Acides Gras", "Caroténoïdes", "Tocophérols"))

cat("=========================================================\n")
cat("Vérification - Nombre de lignes trouvées :", nrow(df_r2), "\n")
cat("=========================================================\n")

# ==============================================================================
# 2. PARAMÉTRAGE DE L'AXE X ET DES ESPACES
# ==============================================================================
niveaux_axe_x <- c(
  "C14.0", "C16.0", "C18.0", "C18.1n9", "C18.2",  
  "ESPACE_1",                                     
  "trans.alpha.carotene", "trans.beta.carotene", "X13.cis.beta.carotene", "X9.cis.beta.carotene", 
  "ESPACE_2",                                     
  "aT", "aT3", "gT3"                              
)

df_r2$Compose <- factor(df_r2$Compose, levels = niveaux_axe_x)
vrais_composes <- niveaux_axe_x[!grepl("ESPACE", niveaux_axe_x)]

df_titres_groupes <- data.frame(
  X_center = c(3, 8.5, 13), 
  Y_pos = 1.15, 
  Label = c("ACIDES GRAS", "CAROTÉNOÏDES", "TOCOPHÉROLS")
)

# ==============================================================================
# 3. FONCTION DE CRÉATION DE PDF
# ==============================================================================
exporter_pdf_multipage <- function(donnees_algo, nom_algo, chemin_fichier_pdf) {
  
  pdf(file = chemin_fichier_pdf, width = 11, height = 6.5)
  
  # On sécurise la boucle pour ne tourner que sur les tissus existants
  tissus_presents <- levels(donnees_algo$Tissu)[levels(donnees_algo$Tissu) %in% unique(donnees_algo$Tissu)]
  
  for (nom_tissu in tissus_presents) {
    
    donnees_page <- donnees_algo %>% filter(Tissu == nom_tissu)
    
    graphique <- ggplot(donnees_page, aes(x = Compose, y = R2, fill = Groupe)) +
      geom_col(color = "black", alpha = 1, width = 1) +
      geom_text(aes(label = sprintf("%.2f", R2), y = R2 + 0.05), 
                size = 4, fontface = "bold", na.rm = TRUE) +
      geom_text(data = df_titres_groupes, aes(x = X_center, y = Y_pos, label = Label), 
                inherit.aes = FALSE, fontface = "bold", size = 5, color = "gray20") +
      facet_wrap(~ Tissu) +
      scale_x_discrete(drop = FALSE, breaks = vrais_composes) +
      scale_fill_manual(values = c("Acides Gras" = "#F0B423",   
                                   "Caroténoïdes" = "#DE6E26",  
                                   "Tocophérols" = "#38A882")) + 
      scale_y_continuous(limits = c(0, 1.20), breaks = seq(0, 1, by = 0.2)) +
      labs(title = paste("Performances de prédiction (", nom_algo, ")"),
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
# 4. GÉNÉRATION DES FICHIERS PDF
# ==============================================================================
dossier_images <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/save/visu/"

# Si le dossier Images n'existe pas, on le crée pour éviter une erreur
if(!dir.exists(dossier_images)) dir.create(dossier_images, recursive = TRUE)

exporter_pdf_multipage(df_r2 %>% filter(Algorithme == "PLSr"), "PLSr", 
                       paste0(dossier_images, "R2_PLSr_Profil_Chimique_Automatique.pdf"))

exporter_pdf_multipage(df_r2 %>% filter(Algorithme == "RF"), "Random Forest", 
                       paste0(dossier_images, "R2_RF_Profil_Chimique_Automatique.pdf"))