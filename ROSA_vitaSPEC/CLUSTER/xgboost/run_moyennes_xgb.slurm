#!/bin/bash
#SBATCH --job-name=XGB_opti               
#SBATCH --cpus-per-task=5              
#SBATCH --mem=8G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 48:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/XGB_opti_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/XGB_opti_%j.err


COMPOSE=$1
DATA=$2
ID_PARAM=$3


echo "xgb de $COMPOSE"

# nettoyage + Miniconda + Env virtuel
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/etc/profile.d/conda.sh
conda activate env_nirs_v2

# dossier racine
DOSSIER_RACINE="/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/xgboost/moyennes"

# sous dossier
mkdir -p "$DOSSIER_RACINE/workspaces_isoles/work_${ID_PARAM}_${COMPOSE}"
cd "$DOSSIER_RACINE/workspaces_isoles/work_${ID_PARAM}_${COMPOSE}"

# exec
python ../../../XGB_moyennes.py "$COMPOSE" "$DATA" "$ID_PARAM"

echo "fin de $COMPOSE"
