#!/bin/bash
#SBATCH --job-name=Pred_Globale          
#SBATCH --cpus-per-task=8               
#SBATCH --mem=16G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 04:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/Pred_Globale_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/Pred_Globale_%j.err     

echo "Nettoyage + Activation Miniconda..."
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/etc/profile.d/conda.sh
conda activate env_nirs_v2

echo "Lancement des prédictions (pred.py)..."
python pred.py

echo "Script terminé. Fichiers sauvegardés !"