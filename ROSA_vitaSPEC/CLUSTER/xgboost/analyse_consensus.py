# -*- coding: utf-8 -*-

import pandas as pd
from pathlib import Path
import sys

# %% CONFIGURATION CHEMINS
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
dossier_results = d0 / "xgboost" / "moyennes" / "Results"

print("\n" + "="*60)
print(" ANALYSE DES CONSENSUS XGBOOST (10 ITERATIONS)")
print("="*60)

if not dossier_results.exists():
    print(f"Erreur : Le dossier {dossier_results} n'existe pas.")
    sys.exit(1)

# Parcourir les dossiers de tissus (meso_frais, meso_silica, HR)
for path_tissu in dossier_results.iterdir():
    if not path_tissu.is_dir():
        continue
        
    tissu = path_tissu.name
    print(f"\n--- Exploration du tissu : {tissu} ---")
    
    # Parcourir les composes chimiques dans ce tissu
    for path_compose in path_tissu.iterdir():
        if not path_compose.is_dir():
            continue
            
        compose = path_compose.name
        fichier_bilan = path_compose / f"BILAN_MONTE_CARLO_{compose}.csv"
        
        # Si le fichier Monte-Carlo existe, on l'analyse
        if fichier_bilan.exists():
            try:
                df = pd.read_csv(fichier_bilan, sep=";")
                
                # Agregation pour trouver le consensus
                synthese = df.groupby("Pretraitement_Gagnant").agg({
                    "Iteration": "count", # Compte le nombre de victoires
                    "RMSECV": "mean",
                    "R2p_Externe": "mean",
                    "RMSEP_Externe": "mean",
                    "RPD_Externe": "mean"
                }).reset_index()
                
                # Renommer la colonne pour plus de clarte
                synthese = synthese.rename(columns={"Iteration": "Nombre_Victoires"})
                
                # Tri : d'abord par nombre de victoires (descendant), puis par RMSECV moyen (ascendant) en cas d'egalite
                synthese = synthese.sort_values(by=["Nombre_Victoires", "RMSECV"], ascending=[False, True])
                
                # Sauvegarde en Excel et CSV dans le dossier du compose
                chemin_excel = path_compose / f"SYNTHESE_PRETRAITEMENTS_{compose}.xlsx"
                chemin_csv = path_compose / f"SYNTHESE_PRETRAITEMENTS_{compose}.csv"
                
                synthese.to_excel(chemin_excel, index=False)
                synthese.to_csv(chemin_csv, sep=";", index=False)
                
                # Affichage du grand gagnant dans la console
                gagnant = synthese.iloc[0]["Pretraitement_Gagnant"]
                victoires = synthese.iloc[0]["Nombre_Victoires"]
                print(f"  [{compose}] -> Gagnant a {victoires}/10 victoires : {gagnant[:40]}...")
                
            except Exception as e:
                print(f"  [{compose}] Erreur lors de l'analyse : {e}")
        else:
            # Si le compose n'a pas encore fini de tourner sur le cluster, on l'ignore
            pass
            
print("\n" + "="*60)
print(" ANALYSE TERMINEE ! Les fichiers de synthese sont prets.")
print("="*60)