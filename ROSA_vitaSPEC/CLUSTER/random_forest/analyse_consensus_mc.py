# -*- coding: utf-8 -*-

import pandas as pd
from pathlib import Path
import sys

# --- CONFIGURATION ---
METHODE = "random_forest" 

d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER")
dir_results = d0 / METHODE / "moyennes" / "Results"
dir_test_final = d0 / METHODE / "moyennes" / "test_final"

# Verification de base
if not dir_results.exists():
    print(f"Erreur : Le dossier {dir_results} n'existe pas.")
    sys.exit(1)

print(f"\n{'='*60}")
print(f" DEBUT DE L'ANALYSE DES BILANS MONTE-CARLO ({METHODE})")
print(f"{'='*60}\n")

compteurs_succes = 0

# 1. Parcours des dossiers Tissus
for path_tissu in dir_results.iterdir():
    if not path_tissu.is_dir():
        continue
    
    idparam = path_tissu.name
    
    # 2. Parcours des dossiers Composes
    for path_compose in path_tissu.iterdir():
        if not path_compose.is_dir():
            continue
        
        compose = path_compose.name
        fichier_bilan = path_compose / f"BILAN_MONTE_CARLO_{compose}.csv"
        
        if not fichier_bilan.exists():
            continue 
        
        # --- 3. LECTURE ET TRAITEMENT ---
        try:
            df = pd.read_csv(fichier_bilan, sep=";")
            
            cols_requises = ["Pretraitement_Gagnant", "R2p_Externe", "RMSEP_Externe", "RPD_Externe"]
            if not all(col in df.columns for col in cols_requises):
                print(f"[AVERTISSEMENT] Colonnes manquantes dans {idparam}/{compose}")
                continue
            
            # Agregation : On groupe par pretraitement, on compte, et on fait les moyennes
            df_agg = df.groupby("Pretraitement_Gagnant").agg(
                Nombre_Iterations=("Pretraitement_Gagnant", "count"),
                Moyenne_R2p=("R2p_Externe", "mean"),
                Moyenne_RMSEP=("RMSEP_Externe", "mean"),
                Moyenne_RPD=("RPD_Externe", "mean")
            ).reset_index()
            
            # 4. LE CLASSEMENT (VOTE + BRIS D'EGALITE)
            # Tri principal : Nombre_Iterations (Decroissant)
            # Tri secondaire : Moyenne_RMSEP (Croissant, on veut le plus petit)
            df_agg = df_agg.sort_values(
                by=["Nombre_Iterations", "Moyenne_RMSEP"], 
                ascending=[False, True]
            )
            
            # Esthetique : Arrondir a 4 decimales
            df_agg["Moyenne_R2p"] = df_agg["Moyenne_R2p"].round(4)
            df_agg["Moyenne_RMSEP"] = df_agg["Moyenne_RMSEP"].round(4)
            df_agg["Moyenne_RPD"] = df_agg["Moyenne_RPD"].round(4)
            
            # Identification du grand gagnant 
            pretraitement_definitif = df_agg.iloc[0]["Pretraitement_Gagnant"]
            nb_victoires = df_agg.iloc[0]["Nombre_Iterations"]
            
            # --- 5. SAUVEGARDE ---
            dossier_sortie = dir_test_final / idparam / compose
            dossier_sortie.mkdir(parents=True, exist_ok=True)
            
            chemin_excel = dossier_sortie / f"SYNTHESE_PRETRAITEMENTS_{compose}.xlsx"
            
            # Export vers Excel
            df_agg.to_excel(chemin_excel, index=False)
            
            pre_court = (pretraitement_definitif[:45] + '...') if len(pretraitement_definitif) > 45 else pretraitement_definitif
            print(f"[OK] {idparam} -> {compose:<15} | Gagnant ({nb_victoires}/10) : {pre_court}")
            
            compteurs_succes += 1
            
        except Exception as e:
            print(f"[ERREUR] Echec du traitement pour {idparam}/{compose} : {e}")

print(f"\n{'='*60}")
print(f" ANALYSE TERMINEE : {compteurs_succes} fichiers Excel generes.")
print(f" Emplacement : {dir_test_final}")
print(f"{'='*60}")