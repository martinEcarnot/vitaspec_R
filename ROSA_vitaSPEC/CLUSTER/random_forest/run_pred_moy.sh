#!/bin/bash
#SBATCH --job-name=Pred_Globale          
#SBATCH --cpus-per-task=8              
#SBATCH --mem=16G                        
#SBATCH -p cpu-dedicated                 
#SBATCH -A dedicated-cpu@cirad-normal    
#SBATCH -t 04:00:00                      
#SBATCH --output=/storage/simple/users/ecarnotm/logs/Pred_Globale_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/Pred_Globale_%j.err     


# nettoyage + Miniconda + Env virtuel
module purge
source /storage/replicated/cirad_users/ecarnotm/miniconda3/etc/profile.d/conda.sh
conda activate env_nirs_v2

# meso frais
echo "pred meso_frais"

python pred.py dat_Meso_frais_2425_repet.csv meso_frais


# meso sec

echo "pred meso_sec"
python pred.py dat_Meso_sec_2425_DIADE_repet.csv meso_silica


# HR 

echo "pred HR"
python pred.py dat_HR_25_DIADE_clean_repet.csv HR



echo "pred toute lancee"