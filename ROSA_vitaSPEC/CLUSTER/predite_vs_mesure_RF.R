# ==============================================================================
# SCRIPT R : predite_vs_mesure_RF.R
# EMPLACEMENT : CLUSTER/
# TRACÉ DU GRAPHIQUE PRED vs MESURE (Style Publication pour Random Forest)
# ==============================================================================

# 1. Chargement de la librairie
# (Fais un install.packages("ggplot2") si tu ne l'as pas encore sur le cluster)
library(ggplot2)

# --- 2. CONFIGURATION DE LA RECHERCHE ---
# Modifie ces valeurs selon l'itération et le composé que tu veux analyser
modele_nom <- "RF"                  # Le nom affiché sur le titre du graphique
dossier_modele <- "random_forest"   # Nom du sous-dossier racine
idparam <- "meso_silica"            # Le sous-dossier de paramétrage
compose <- "C18.0"                  # Nom exact du composé (ex: trans.alpha.carotene)
iteration <- 10                     # Numéro de l'itération ciblée (1 à 10)

# --- 3. GESTION DES CHEMINS ---
# Le script est dans CLUSTER, on construit donc le chemin relatif vers le dossier de l'itération
dossier_cible <- sprintf("%s/moyennes/test_final/%s/%s/iter_%d", 
                         dossier_modele, idparam, compose, iteration)

chemin_csv <- sprintf("%s/PREDICTIONS_EXTERNES_%s.csv", dossier_cible, compose)

if (!file.exists(chemin_csv)) {
  stop(paste("Erreur : Impossible de trouver le fichier. Vérifie les noms (compose, idparam, iter) :", chemin_csv))
} else {
  cat("Fichier de prédiction trouvé ! Génération du graphique en cours...\n")
}

# --- 4. LECTURE ET CALCUL DES MÉTRIQUES ---
df <- read.csv(chemin_csv, sep = ";")

y_true <- df$Vraie_Valeur
y_pred <- df$Valeur_Predite

# Re-calcul exact des métriques de validation externe
rmsep <- sqrt(mean((y_true - y_pred)^2))

ss_res <- sum((y_true - y_pred)^2)
ss_tot <- sum((y_true - mean(y_true))^2)
r2p <- 1 - (ss_res / ss_tot)

rpd <- sd(y_true) / rmsep

# Construction du texte respectant l'alignement de l'image de référence
titre_graph <- sprintf("%s Predictions vs Mesures : %s", modele_nom, compose)
sous_titre_graph <- sprintf("R2p = %.3f    |    RMSEp = %.3f    |    RPD = %.3f", r2p, rmsep, rpd)

# --- 5. CRÉATION DU GRAPHIQUE ---
p <- ggplot(data = df, aes(x = Valeur_Predite, y = Vraie_Valeur)) +
  
  # Droite de régression (Bleue, continue)
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  
  # Droite parfaite 1:1 (Rouge, pointillés)
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  
  # Points du nuage de dispersion (Cercles vides avec contour noir)
  geom_point(shape = 21, color = "black", fill = "white", size = 2.5, stroke = 0.8) +
  
  # Textes
  labs(
    title = titre_graph,
    subtitle = sous_titre_graph,
    x = "Valeurs predites",
    y = "Valeurs mesurees"
  ) +
  
  # Thème d'affichage ultra-épuré (calqué sur la photo)
  theme_minimal() +
  theme(
    # Fonds blancs parfaits
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grille majeure très claire, pas de grille mineure
    panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    
    # Lignes des axes uniquement à gauche et en bas
    axis.line.x = element_line(color = "#EBEBEB", linewidth = 0.8),
    axis.line.y = element_line(color = "#EBEBEB", linewidth = 0.8),
    
    # Textes : taille et positionnement des titres
    plot.title = element_text(size = 14, color = "black", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "black", hjust = 0, margin = margin(b = 15)),
    
    # Textes : taille et positionnement des labels des axes
    axis.title.x = element_text(size = 11, color = "black", margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, color = "black", margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "#555555")
  )

# --- 6. SAUVEGARDE DES GRAPHIQUES ---
# On sauvegarde les graphiques directement dans le dossier "iter_X"
chemin_pdf <- sprintf("%s/SCATTER_PUBLI_RF_%s.pdf", dossier_cible, compose)
chemin_png <- sprintf("%s/SCATTER_PUBLI_RF_%s.png", dossier_cible, compose)

ggsave(chemin_pdf, plot = p, width = 8, height = 6, dpi = 300)
ggsave(chemin_png, plot = p, width = 8, height = 6, dpi = 300)

cat("\nOpération terminée ! Les graphiques (PDF et PNG) ont été créés dans :\n->", dossier_cible, "\n")