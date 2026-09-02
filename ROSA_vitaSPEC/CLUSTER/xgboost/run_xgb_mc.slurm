#!/bin/bash
#SBATCH --job-name=XGB_MC              
#SBATCH --cpus-per-task=32             
#SBATCH --mem=8G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-long  
#SBATCH -t 100:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/XGB_MC_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/XGB_MC_%j.err

COMPOSE=$1
DATA=$2
ID_PARAM=$3

echo "execution Monte-Carlo XGB de $COMPOSE"

# nettoyage + Miniconda + Env virtuel
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/etc/profile.d/conda.sh
conda activate env_nirs_v2

# dossier racine (adapte pour XGBoost)
DOSSIER_RACINE="/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/xgboost/moyennes"

# sous dossier (isolation du workspace)
mkdir -p "$DOSSIER_RACINE/workspaces_isoles/work_mc_${ID_PARAM}_${COMPOSE}"
cd "$DOSSIER_RACINE/workspaces_isoles/work_mc_${ID_PARAM}_${COMPOSE}"

# execution (pointe vers le script 1 XGBoost)
python ../../../xgb_moyennes_mc.py "$COMPOSE" "$DATA" "$ID_PARAM"

echo "fin Monte-Carlo XGB de $COMPOSE"