# -*- coding: utf-8 -*-

import os
import sys
import ast
import pandas as pd
import numpy as np
import random
import joblib
import warnings
from pathlib import Path

from sklearn.ensemble import RandomForestRegressor
from xgboost import XGBRegressor
from sklearn.base import BaseEstimator, RegressorMixin
import nirs4all

warnings.filterwarnings("ignore")

# --- CHEMINS ---
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
from diy_functions.pre_translation import pre_translation

dossier_production = d0 / "PRODUCTION"
dossier_modeles_prod = dossier_production / "modeles_pack"
dossier_production.mkdir(parents=True, exist_ok=True)

# Dictionnaire pour retrouver le bon fichier data
dict_fichiers_data = {
    "meso_frais": "dat_mean_Meso_frais_2425.csv",
    "meso_silica": "dat_mean_Meso_sec_2425_DIADE.csv",
    "HR": "dat_HR_25_DIADE_clean.csv",
    "exo_frais": "dat_mean_Exo_frais_2425.csv"
}

# --- LECTURE DES ARGUMENTS DU TERMINAL ---
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
            print(f"Attention : l'algorithme '{arg}' n'est pas reconnu et sera ignore.")
            
    algorithmes_a_traiter = list(set(algorithmes_a_traiter))

if not algorithmes_a_traiter:
    print("Aucun algorithme valide fourni. Fin du script.")
    sys.exit(0)

print("="*60)
print(f"ALGORITHMES SELECTIONNES : {', '.join(algorithmes_a_traiter)}")
print("="*60)


print("\n--- PHASE 1 : CREATION DES CATALOGUES ---")
for algo in algorithmes_a_traiter:
    dossier_test_final = d0 / algo / "test_final" if algo == "pls" else d0 / algo / "moyennes" / "test_final"
    if not dossier_test_final.exists(): 
        print(f"[{algo}] Dossier source introuvable. A-t-il ete calcule ?")
        continue
        
    lignes_catalogue = []
    
    for dossier_tissu in dossier_test_final.iterdir():
        if not dossier_tissu.is_dir(): continue
        tissu = dossier_tissu.name
        
        fichier_data_nom = next((v for k, v in dict_fichiers_data.items() if k in tissu), "A_DEFINIR.csv")
        
        for dossier_compose in dossier_tissu.iterdir():
            if not dossier_compose.is_dir(): continue
            compose = dossier_compose.name
            fichier_bilan = dossier_compose / f"BILAN_FINAL_VALIDATION_{compose}.csv"
            
            if not fichier_bilan.exists(): continue
                
            df_bilan = pd.read_csv(fichier_bilan, sep=";")
            
            # THEORIE DU CHAMPION : Par frequence puis par RMSEP moyen
            col_param = "Variables_Latentes" if algo == "pls" else "Hyperparametres"
            
            if col_param not in df_bilan.columns:
                print(f"[{algo}] Erreur : colonne '{col_param}' absente dans le bilan de {compose}.")
                continue
            
            # 1. On regroupe par hyperparametres (ou NVL)
            resume_params = df_bilan.groupby(col_param).agg(
                Frequence=('Iteration', 'count'),
                RMSEP_Moyen=('RMSEP_Externe', 'mean'),
                R2p_Moyen=('R2p_Externe', 'mean')
            ).reset_index()
            
            # 2. On trie : Frequence decroissante, puis RMSEP_Moyen croissant (le plus petit gagne)
            resume_params = resume_params.sort_values(by=['Frequence', 'RMSEP_Moyen'], ascending=[False, True])
            
            # 3. Le premier de la liste est notre Champion absolu
            ligne_championne = resume_params.iloc[0]
            
            # Le pretraitement etant le meme pour les 10 iterations dans ce fichier, on recupere le premier
            pretraitement = str(df_bilan["Pretraitement"].iloc[0])
            rmsep_opti = round(ligne_championne["RMSEP_Moyen"], 4)
            r2p_opti = round(ligne_championne["R2p_Moyen"], 4)
            
            nvl_opt = ligne_championne["Variables_Latentes"] if algo == "pls" else ""
            hyper_opt = ligne_championne["Hyperparametres"] if algo in ["random_forest", "xgboost"] else ""
            
            dossier_pack_dest = dossier_modeles_prod / algo / tissu / compose
            dossier_pack_dest.mkdir(parents=True, exist_ok=True)
            
            lignes_catalogue.append({
                "Algorithme": algo,
                "Tissu": tissu,
                "Compose": compose,
                "Fichier_Data": fichier_data_nom,
                "Pretraitement": pretraitement,
                "Variables_Latentes": nvl_opt,
                "Hyperparametres": hyper_opt,
                "RMSEP_Attendu": rmsep_opti,
                "R2p_Attendu": r2p_opti,
                "Dossier_Pack_Modeles": str(dossier_pack_dest.relative_to(d0))
            })
            
    if lignes_catalogue:
        pd.DataFrame(lignes_catalogue).to_csv(dossier_production / f"CATALOGUE_PRODUCTION_{algo}.csv", index=False, sep=";")
        print(f"[{algo}] Catalogue genere !")

print("\n--- PHASE 2 : ENTRAINEMENT DEFINITIF 100% (RF & XGB) ---")
algos_a_entrainer = [a for a in algorithmes_a_traiter if a in ["random_forest", "xgboost"]]

if not algos_a_entrainer:
    print("Aucun algorithme Python a entrainer (seule la PLS a ete demandee ou rien n'a ete precise).")
else:
    for algo in algos_a_entrainer:
        chemin_cat = dossier_production / f"CATALOGUE_PRODUCTION_{algo}.csv"
        if not chemin_cat.exists(): continue
            
        df_cat = pd.read_csv(chemin_cat, sep=";")
        
        for _, row in df_cat.iterrows():
            tissu, compose, fichier_data = row["Tissu"], row["Compose"], row["Fichier_Data"]
            print(f"\nEntrainement {algo.upper()} : {tissu} | {compose}")
            
            df_data = pd.read_csv(d0 / "commun" / fichier_data)
            df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
            df_propre = df_data.dropna(subset=[compose])
            
            col_spectres = [c for c in df_propre.columns if str(c).startswith("x.")]
            y = df_propre[compose].values.astype(float) + np.random.normal(0, 1e-6, size=len(df_propre))
            X = df_propre[col_spectres].values
            X[X <= 0] = 1e-5
            
            etapes_pre = pre_translation(str(row["Pretraitement"]))
            if len(etapes_pre) > 0:
                panier = {}
                class Interceptor(BaseEstimator, RegressorMixin):
                    def fit(self, X_t, y_t, **kwargs): panier['X'] = X_t; return self
                    def predict(self, X_t): return np.zeros(len(X_t))
                try: nirs4all.run(dataset=(X, y), pipeline=etapes_pre + [{"model": Interceptor()}])
                except: pass
                X_trans = panier.get('X', X)
            else:
                X_trans = X
                
            hyper_dict = ast.literal_eval(row["Hyperparametres"])
            chemin_dest = d0 / row["Dossier_Pack_Modeles"]
            
            for i in range(1, 11):
                seed = random.randint(1, 999999)
                if algo == "random_forest":
                    model = RandomForestRegressor(**hyper_dict, random_state=seed, n_jobs=-1)
                else:
                    model = XGBRegressor(**hyper_dict, random_state=seed, n_jobs=-1, tree_method="hist")
                    
                model.fit(X_trans, y)
                joblib.dump(model, chemin_dest / f"modele_prod_iter_{i}.joblib")
                
            print("-> 10 modeles enregistres.")

print("\nScript Python termine.")