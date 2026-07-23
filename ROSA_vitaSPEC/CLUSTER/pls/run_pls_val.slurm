#!/bin/bash
#SBATCH --job-name=PLS_VAL              
#SBATCH --cpus-per-task=2              
#SBATCH --mem=4G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 48:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/PLS_val_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/PLS_val_%j.err

COMPOSE=$1
DATA=$2
ID_PARAM=$3

echo "Execution Validation Finale PLS de $COMPOSE"

# Nettoyage + Chargement de l'environnement R natif du cluster
module purge
module load bioinfo-ifb
module load r/4.5.2

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# On se deplace dans le dossier PLS
DOSSIER_RACINE="/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/pls"
cd "$DOSSIER_RACINE"

# Lancement du script R
Rscript pls_validation.R "$COMPOSE" "$DATA" "$ID_PARAM"

echo "Fin Validation PLS de $COMPOSE"