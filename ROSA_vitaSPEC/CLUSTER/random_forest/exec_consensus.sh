#!/bin/bash
#SBATCH --job-name=RF_consensus              
#SBATCH --cpus-per-task=1              
#SBATCH --mem=4G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 00:10:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/RF_consensus_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/RF_consensus_%j.err

echo "Lancement de l'analyse des consensus Monte-Carlo"

# Nettoyage + Miniconda + Env virtuel
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/etc/profile.d/conda.sh
conda activate env_nirs_v2

# Execution directe du script
python /storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/random_forest/analyse_consensus_mc.py

echo "Analyse terminee avec succes"