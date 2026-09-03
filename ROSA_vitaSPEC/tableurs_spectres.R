# ==============================================================================
# SCRIPT UNIVERSEL DE COMPILATION DES SPECTRES .asd EN MATRICES .csv
# A executer en local sur le PC
# ==============================================================================

# Chargement des librairies necessaires
library(tidyverse)
library(nirsextra) 

# --- 1. CONFIGURATION ---
# Remplacez ce chemin par la localisation de votre dossier principal contenant les sous-dossiers.
# Attention : Utilisez bien des slashs normaux (/) et non des antislashs (\).
dossier_parent <- "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/spectres"

# On cree un dossier de sortie propre a l'interieur du dossier parent pour ne rien melanger
dossier_sortie <- file.path("C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/CLUSTER/commun/Matrices_Compilees_CSV")
if (!dir.exists(dossier_sortie)) dir.create(dossier_sortie)


# --- 2. MOTEUR DE LECTURE (Fonction basee sur vos anciens codes) ---
compiler_dossier <- function(chemin_dossier) {
  nom_dossier <- basename(chemin_dossier)
  fichiers_asd <- list.files(chemin_dossier, pattern = "\\.asd$", full.names = TRUE, ignore.case = TRUE)
  
  # Si le dossier ne contient pas de fichier .asd, on l'ignore silencieusement
  if (length(fichiers_asd) == 0) return(NULL)
  
  cat("-> Traitement de", length(fichiers_asd), "spectres dans le sous-dossier :", nom_dossier, "...\n")
  
  liste_lignes <- list()
  
  for (i in seq_along(fichiers_asd)) {
    fichier <- fichiers_asd[i]
    # Le nom de l'echantillon sera le nom du fichier sans l'extension .asd
    nom_spectre <- tools::file_path_sans_ext(basename(fichier)) 
    
    # Lecture du spectre avec la librairie nirsextra
    sp <- tryCatch({ asd_read(fichier) }, error = function(e) NULL)
    
    if (!is.null(sp)) {
      # Calcul de la reflectance brute[cite: 3]
      valeurs <- as.numeric(sp$spectrum / sp$reference)
      
      # Extraction des longueurs d'ondes et securite si nulles[cite: 3]
      wl <- sp$wavelength
      if (is.null(wl)) wl <- seq(350, 2500, length.out = length(valeurs))
      
      # Formatage en ligne de tableau
      ligne <- data.frame(matrix(valeurs, nrow = 1))
      colnames(ligne) <- paste0("x.", round(wl))
      
      # Ajout de la colonne "ech" en premier
      ligne <- cbind(ech = nom_spectre, ligne)
      
      liste_lignes[[i]] <- ligne
    }
  }
  
  # Assemblage de toutes les lignes dans un grand tableau final
  df_final <- bind_rows(liste_lignes)
  
  # Sauvegarde au format .csv pour le cluster
  chemin_csv <- file.path(dossier_sortie, paste0(nom_dossier, ".csv"))
  write.csv(df_final, chemin_csv, row.names = FALSE)
  cat("   [OK] Matrice sauvegardee :", basename(chemin_csv), "\n")
}

# --- 3. ORCHESTRATION ---
cat("=========================================================\n")
cat("LANCEMENT DE LA COMPILATION NIRS\n")
cat("=========================================================\n")

# On liste tous les sous-dossiers presents dans le dossier parent
liste_sous_dossiers <- list.dirs(dossier_parent, recursive = FALSE, full.names = TRUE)

# On evite que le script n'essaie de lire son propre dossier de sortie
liste_sous_dossiers <- liste_sous_dossiers[liste_sous_dossiers != dossier_sortie]

# On boucle sur chaque sous-dossier
for (sous_dos in liste_sous_dossiers) {
  compiler_dossier(sous_dos)
}

cat("\n=========================================================\n")
cat("TERMINE ! Vos matrices sont pretes dans :\n", dossier_sortie, "\n")
cat("Vous pouvez les envoyer sur le cluster.\n")
cat("=========================================================\n")

