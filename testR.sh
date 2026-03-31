#!/bin/bash
#SBATCH --job-name=TEST
#SBATCH --array=1-33                  # un job par dataset
#SBATCH --cpus-per-task=1             # 1 CPU par job
#SBATCH --mem=4G                      # mémoire par job
#SBATCH -p cpu-dedicated              # partition obligatoire
#SBATCH -A dedicated-cpu@cirad        # account obligatoire
#SBATCH -t 01:00:00                   # durée max (adapter si besoin, max 01:00:00)
#SBATCH --output=logs/CV%a.out
#SBATCH --error=logs/CV%a.err

# Charger R (adapter selon les modules disponibles sur ton cluster)
module load r/4.5.2   # ← vérifier avec : module avail R

# Lancer le script
Rscript ROSA_vitaSPEC/CLUSTER/modele_pls/bloc_CV_cluster.R