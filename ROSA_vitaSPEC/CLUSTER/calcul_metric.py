# -*- coding: utf-8 -*-

import sys
import pandas as pd
import numpy as np
from pathlib import Path
import warnings

warnings.filterwarnings("ignore")

# --- CHEMINS ---
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
dossier_production = d0 / "PRODUCTION"
dossier_commun = d0 / "commun"

# --- 1. LECTURE DES ARGUMENTS DU TERMINAL ---
mappage_algos = {
    "pls": "pls",
    "rf": "random_forest",
    "random_forest": "random_forest",
    "xgb": "xgboost",
    "xgboost": "xgboost"
}

args_utilisateur = sys.argv[1:]
algorithmes_a_traiter = []

if len(args_utilisateur) == 0:
    algorithmes_a_traiter = ["pls", "random_forest", "xgboost"]
else:
    for arg in args_utilisateur:
        arg_propre = arg.lower()
        if arg_propre in mappage_algos:
            algorithmes_a_traiter.append(mappage_algos[arg_propre])
        else:
            # L'accent sur "ignore" a ete retire
            print(f"Attention : l'algorithme '{arg}' n'est pas reconnu et sera ignore.")
            
algorithmes_a_traiter = list(set(algorithmes_a_traiter))

if not algorithmes_a_traiter:
    print("Aucun algorithme valide fourni. Fin du script.")
    sys.exit(0)

print("="*60)
print(f"CALCUL DES METRIQUES AVANCEES (CV, RER, RPD)")
print(f"ALGORITHMES ACTIFS : {', '.join(algorithmes_a_traiter)}")
print("="*60)

# --- 2. BOUCLE DE CALCUL ---
for algo in algorithmes_a_traiter:
    chemin_cat = dossier_production / f"CATALOGUE_PRODUCTION_{algo}.csv"
    
    if not chemin_cat.exists():
        # L'accent sur "genere" a ete retire
        print(f"[{algo.upper()}] Catalogue introuvable. A-t-il ete genere ?")
        continue
        
    df_cat = pd.read_csv(chemin_cat, sep=";")
    resultats_algo = []
    
    print(f"\n--- Traitement du catalogue : {algo.upper()} ---")
    
    for _, row in df_cat.iterrows():
        tissu = row["Tissu"]
        compose = row["Compose"]
        fichier_data = row["Fichier_Data"]
        rmsep = float(row["RMSEP_Attendu"])
        r2p = float(row["R2p_Attendu"])
        
        chemin_data = dossier_commun / fichier_data
        
        if not chemin_data.exists():
            print(f" -> [Alerte] Matrice introuvable pour {tissu} | {compose} ({fichier_data})")
            continue
            
        df_data = pd.read_csv(chemin_data)
        
        if compose not in df_data.columns:
            print(f" -> [Alerte] Colonne '{compose}' introuvable dans {fichier_data}")
            continue
            
        y_brut = pd.to_numeric(df_data[compose], errors="coerce")
        y_propre = y_brut.dropna().values
        n_ech = len(y_propre)
        
        if n_ech == 0:
            continue
            
        moyenne = np.mean(y_propre)
        ecart_type = np.std(y_propre, ddof=1)
        val_min = np.min(y_propre)
        val_max = np.max(y_propre)
        etendue = val_max - val_min
        
        rmsep_safe = max(rmsep, 1e-9)
        moyenne_safe = moyenne if abs(moyenne) > 1e-9 else np.nan
        
        cv = (rmsep_safe / moyenne_safe) * 100 if pd.notnull(moyenne_safe) else np.nan
        rer = etendue / rmsep_safe
        rpd = ecart_type / rmsep_safe
        
        resultats_algo.append({
            "Tissu": tissu,
            "Compose": compose,
            "N_Echantillons": n_ech,
            "Moyenne": round(moyenne, 4),
            "SD": round(ecart_type, 4),
            "Min": round(val_min, 4),
            "Max": round(val_max, 4),
            "RMSEP_Attendu": rmsep,
            "R2p_Attendu": r2p,
            "CV_(%)": round(cv, 2) if pd.notnull(cv) else "N/A",
            "RER": round(rer, 2),
            "RPD": round(rpd, 2)
        })
        
    if resultats_algo:
        df_final = pd.DataFrame(resultats_algo)
        df_final = df_final.sort_values(by=["Tissu", "Compose"])
        
        nom_sortie = dossier_production / f"METRIQUES_PERFORMANCES_{algo.upper()}.xlsx"
        try:
            df_final.to_excel(nom_sortie, index=False)
            print(f" -> Succes : Fichier {nom_sortie.name} genere dans PRODUCTION/ !")
        except Exception:
            nom_csv = dossier_production / f"METRIQUES_PERFORMANCES_{algo.upper()}.csv"
            df_final.to_csv(nom_csv, sep=";", index=False)
            print(f" -> Succes : Fichier {nom_csv.name} genere (Format CSV).")

print("\nProcessus totalement termine.")