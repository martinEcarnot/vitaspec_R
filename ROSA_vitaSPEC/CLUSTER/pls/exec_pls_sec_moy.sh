#!/bin/bash

# Configuration des fichiers d'entrée globaux
DATA="dat_mean_Meso_sec_2425_DIADE.csv"
ID_PARAM="meso_silica" # Modifiable selon la modalité étudiée (frais, silica, HR)

# Liste exhaustive des composés à injecter dans la boucle
COMPOSES=(
    "eau"
    "C14.0"
    "C16.0"
    "C18.0"
    "C18.1n9"
    "C18.1n7"
    "C18.2"
    "C18.3"
    "C20.0"
    "tlip.MS"
    "trans.alpha.carotene"
    "trans.beta.carotene"
    "total.trans.carotene.natif"
    "ratio.alpha.beta"
    "ratio.alpha.natif"
    "ratio.beta.natif"
    "X13.cis.beta.carotene"
    "X9.cis.beta.carotene"
    "total.beta.carotene"
    "total.carotene"
)

# Envoi des tâches au gestionnaire Slurm
for c in "${COMPOSES[@]}"; do
    echo "lancement de $c"
    sbatch run_pls_moy.slurm "$c" "$DATA" "$ID_PARAM"
done

echo "Jobs tous lances"