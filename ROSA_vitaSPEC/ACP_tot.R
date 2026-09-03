# ==============================================================================
# SCRIPT COMPLET : ANALYSE EN COMPOSANTES PRINCIPALES (ACP) DES SPECTRES
# ==============================================================================

# --- 0. CHARGEMENT DES LIBRAIRIES ---
# (Installez les packages avec install.packages(c("FactoMineR", "factoextra")) si besoin)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# --- 1. CONFIGURATION DU CHEMIN ---
dossier_matrices <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC/Data/Matrices_Compilees_CSV"

# --- 2. IMPORTATION ET FUSION DES DONNÉES ---
cat("Recherche des fichiers .csv...\n")
fichiers_csv <- list.files(dossier_matrices, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(fichiers_csv) == 0) {
  stop("Aucun fichier .csv trouvé dans ce dossier !")
}

liste_df <- list()

for (fichier in fichiers_csv) {
  df_temp <- read.csv(fichier, stringsAsFactors = FALSE)
  
  # Déduction du tissu d'après le nom du fichier (Insensible à la casse)
  if (grepl("FRAIS", fichier, ignore.case = TRUE)) {
    tissu <- "Mésocarpe Frais"
  } else if (grepl("SEC", fichier, ignore.case = TRUE)) {
    tissu <- "Mésocarpe Sec"
  } else if (grepl("HR", fichier, ignore.case = TRUE)) {
    tissu <- "Huile Rouge"
  } else {
    tissu <- "Inconnu"
  }
  
  df_temp$Tissu <- tissu
  liste_df[[fichier]] <- df_temp
}

# Fusion et mise en ordre des facteurs
df_global <- bind_rows(liste_df)
df_global$Tissu <- factor(df_global$Tissu, levels = c("Huile Rouge", "Mésocarpe Frais", "Mésocarpe Sec"))

cat("->", nrow(df_global), "spectres chargés au total.\n")

# --- 3. CALCUL DE L'ACP ---
cat("Calcul de l'ACP en cours...\n")
col_spectres <- grep("^x\\.", names(df_global), value = TRUE)
matrice_X <- df_global[, col_spectres]

# ACP non normée (recommandé pour les spectres SPIR)
res_pca <- PCA(matrice_X, scale.unit = FALSE, graph = FALSE)

var_pc1 <- round(res_pca$eig[1, 2], 1)
var_pc2 <- round(res_pca$eig[2, 2], 1)

# --- 4. GRAPHIQUE 1 : SCORE PLOT (Le nuage de points des échantillons) ---
cat("Génération des graphiques...\n")

df_pca <- data.frame(
  Echantillon = df_global$ech,
  Tissu = df_global$Tissu,
  PC1 = res_pca$ind$coord[, 1],
  PC2 = res_pca$ind$coord[, 2]
)

graphique_acp <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Tissu, fill = Tissu)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5, alpha = 0.8, shape = 21, color = "black") +
  stat_ellipse(geom = "polygon", alpha = 0.15, type = "norm", level = 0.95, linetype = "dashed") +
  scale_fill_manual(values = c("Huile Rouge" = "#D95F02", "Mésocarpe Frais" = "#1B9E77", "Mésocarpe Sec" = "#E6A100")) +
  scale_color_manual(values = c("Huile Rouge" = "#D95F02", "Mésocarpe Frais" = "#1B9E77", "Mésocarpe Sec" = "#E6A100")) +
  labs(
    title = "Analyse en Composantes Principales (ACP) des spectres",
    subtitle = "Projection sur les deux premières composantes",
    x = paste0("Composante Principale 1 (", var_pc1, " %)"),
    y = paste0("Composante Principale 2 (", var_pc2, " %)")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30", margin = margin(b=15)),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

# --- 5. GRAPHIQUE 2 : SCREE PLOT (Variance expliquée) ---
graphique_variance <- fviz_eig(res_pca, 
                               addlabels = TRUE, 
                               ylim = c(0, 100),
                               barfill = "#4169E1",
                               barcolor = "black",
                               main = "Pourcentage de variance expliquée par composante",
                               ylab = "Pourcentage de variance (%)",
                               xlab = "Composantes Principales (PC)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --- 6. GRAPHIQUE 3 : LOADINGS (Poids des longueurs d'ondes) ---
df_loadings <- data.frame(
  Wavelength = as.numeric(gsub("x\\.", "", rownames(res_pca$var$coord))),
  PC1 = res_pca$var$coord[, 1],
  PC2 = res_pca$var$coord[, 2]
)

df_loadings_long <- pivot_longer(df_loadings, cols = c(PC1, PC2), names_to = "Axe", values_to = "Coordonnee")

graphique_loadings <- ggplot(df_loadings_long, aes(x = Wavelength, y = Coordonnee, color = Axe)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1) +
  facet_grid(Axe ~ ., scales = "free_y") +
  scale_color_manual(values = c("PC1" = "#D62728", "PC2" = "#1F77B4")) +
  labs(
    title = "Poids des longueurs d'onde sur les Axes 1 et 2 (Loadings)",
    x = "Longueur d'onde (nm)",
    y = "Coordonnée de la variable"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 12))


# --- 7. AFFICHAGE ET SAUVEGARDES ---
print(graphique_acp)
# print(graphique_variance) # (Décommentez pour afficher directement dans R)
# print(graphique_loadings) # (Décommentez pour afficher directement dans R)

# Sauvegarde des images
ggsave(file.path(dossier_matrices, "ACP_1_ScorePlot_Tissus.png"), plot = graphique_acp, width = 10, height = 7, dpi = 300)
ggsave(file.path(dossier_matrices, "ACP_2_Variance.png"), plot = graphique_variance, width = 8, height = 5, dpi = 300)
ggsave(file.path(dossier_matrices, "ACP_3_Loadings_Variables.png"), plot = graphique_loadings, width = 10, height = 6, dpi = 300)

# Sauvegarde des données mathématiques
write.csv(res_pca$ind$cos2, file.path(dossier_matrices, "ACP_Donnees_Qualite_Cos2_Individus.csv"))
write.csv(res_pca$var$contrib, file.path(dossier_matrices, "ACP_Donnees_Contributions_Variables.csv"))

cat("\n=========================================================\n")
cat("TERMINE ! Les graphiques et données ont été sauvegardés dans :\n")
cat(dossier_matrices, "\n")
cat("=========================================================\n")