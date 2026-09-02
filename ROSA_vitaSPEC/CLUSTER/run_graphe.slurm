#!/bin/bash
#SBATCH --job-name=R_Graph
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH -p cpu-dedicated
#SBATCH -A dedicated-cpu@cirad-long
#SBATCH -t 00:10:00
#SBATCH --output=/storage/simple/users/ecarnotm/logs/R_graph_%j.out
#SBATCH --error=/storage/simple/users/ecarnotm/logs/R_graph_%j.err

echo "Nettoyage et chargement de R..."
module purge
module load bioinfo-ifb
module load r/4.5.2

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

echo "Lancement du script R..."
Rscript predite_vs_mesure_RF.R

echo "Graphique généré avec succès !"