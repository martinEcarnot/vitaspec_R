#!/bin/bash
#SBATCH --job-name=RF_opti               
#SBATCH --cpus-per-task=5              
#SBATCH --mem=8G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 48:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/RF_opti_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/RF_opti_%j.err

COMPOSE=$1
FICHIER_DATA=$2
ID_PARAM=$3


echo "execution de $COMPOSE"

# nettoyage + miniconda + env virtuel
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/bin/activate
conda activate env_nirs_v2

# path
cd /storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/random_forest/

# exec
python RF.py "$COMPOSE" "$FICHIER_DATA" "$ID_PARAM"

echo "fin de $COMPOSE"