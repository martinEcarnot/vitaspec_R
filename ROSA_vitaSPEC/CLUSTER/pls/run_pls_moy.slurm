#!/bin/bash
#SBATCH --job-name=PLS_moy            
#SBATCH --cpus-per-task=1              
#SBATCH --mem=4G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 48:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/PLS_opti_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/PLS_opti_%j.err

COMPOSE=$1
DATA=$2
ID_PARAM=$3


# nettoyage + env
module purge
module load bioinfo-ifb
module load r/4.5.2

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Definition de la racine du projet
DOSSIER_RACINE="/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/pls"

# Creation et positionnement dans l'isoloir de calcul
mkdir -p "$DOSSIER_RACINE/workspaces_isoles/work_${ID_PARAM}_${COMPOSE}"
cd "$DOSSIER_RACINE/workspaces_isoles/work_${ID_PARAM}_${COMPOSE}"

# Execution du script R
Rscript ../../pls_moyennes.R "$COMPOSE" "$DATA" "$ID_PARAM"

echo "fin de $COMPOSE"