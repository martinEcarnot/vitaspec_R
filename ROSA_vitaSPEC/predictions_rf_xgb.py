# -*- coding: utf-8 -*-

import os
import sys
import ast
import pandas as pd
import numpy as np
import joblib
import warnings
from pathlib import Path
from sklearn.base import BaseEstimator, RegressorMixin
import nirs4all

warnings.filterwarnings("ignore")

# --- CHEMIN LOCAL ---
d0 = Path(
    r"C:\Users\U108-N806\Documents\STAGE_M2_ROSA_NIRS\VitaSPEC\vitaspec_R\ROSA_vitaSPEC\CLUSTER"
)
sys.path.append(str((d0 / "commun").resolve()))
from diy_functions.pre_translation import pre_translation

# --- 1. LECTURE DES ARGUMENTS DU TERMINAL ---
if len(sys.argv) < 3:
    print("Erreur : arguments manquants.")
    print(
        'Exemple : python prediction_dossier_rf_xgb.py "C:\\Dossier\\FRAIS" frais rf xgb'
    )
    sys.exit(1)

dossier_cible = Path(sys.argv[1])
type_tissu_user = sys.argv[2].lower()
args_algos = sys.argv[3:]

if not dossier_cible.is_dir():
    print(f"Erreur : Le dossier {dossier_cible} est introuvable.")
    sys.exit(1)

# Traduction du mot-cle utilisateur vers le vrai nom du tissu dans le catalogue
map_tissu = {"frais": "meso_frais", "sec": "meso_silica", "hr": "HR"}
if type_tissu_user not in map_tissu:
    print(
        f"Erreur : Le tissu '{type_tissu_user}' n'est pas reconnu. Utilisez 'frais', 'sec' ou 'hr'."
    )
    sys.exit(1)
tissu_exact = map_tissu[type_tissu_user]

mappage_algos = {"rf": "random_forest", "xgb": "xgboost"}
algorithmes_a_traiter = []

if not args_algos:
    algorithmes_a_traiter = ["random_forest", "xgboost"]
else:
    for arg in args_algos:
        arg_propre = arg.lower()
        if arg_propre in mappage_algos:
            algorithmes_a_traiter.append(mappage_algos[arg_propre])
        else:
            print(
                f"Attention: algorithme '{arg}' non reconnu (utilisez 'rf' et/ou 'xgb')."
            )

algorithmes_a_traiter = list(set(algorithmes_a_traiter))
if not algorithmes_a_traiter:
    sys.exit(0)

# On recupere tous les fichiers .csv du dossier
fichiers_csv = list(dossier_cible.glob("*.csv"))
if not fichiers_csv:
    print(f"Aucun fichier .csv trouve dans le dossier {dossier_cible}.")
    sys.exit(0)

print("=" * 60)
print(f"PREDICTIONS EN LOT PYTHON | TISSU : {tissu_exact}")
print(f"DOSSIER CIBLE : {dossier_cible.name} ({len(fichiers_csv)} fichiers trouves)")
print(f"ALGORITHMES   : {', '.join(algorithmes_a_traiter)}")
print("=" * 60)

# --- 2. PRE-CHARGEMENT DES CATALOGUES ---
dossier_production = d0 / "PRODUCTION"
catalogues = {}
for algo in algorithmes_a_traiter:
    chemin_cat = dossier_production / f"CATALOGUE_PRODUCTION_{algo}.csv"
    if chemin_cat.exists():
        df_cat = pd.read_csv(chemin_cat, sep=";")
        # On filtre immediatement pour ne garder que le tissu souhaite
        df_cat_tissu = df_cat[df_cat["Tissu"] == tissu_exact]
        if not df_cat_tissu.empty:
            catalogues[algo] = df_cat_tissu

# --- 3. BOUCLE SUR CHAQUE MATRICE CSV ---
for fichier_csv in fichiers_csv:
    nom_matrice = fichier_csv.stem
    dossier_out = d0 / "predictions" / nom_matrice
    dossier_out.mkdir(parents=True, exist_ok=True)

    print(f"\n>>> Traitement de la matrice : {fichier_csv.name} <<<")

    df_inconnu = pd.read_csv(fichier_csv)
    if "ech" not in df_inconnu.columns:
        df_inconnu["ech"] = [f"Inconnu_{i}" for i in range(1, len(df_inconnu) + 1)]

    col_spectres = [c for c in df_inconnu.columns if str(c).startswith("x.")]
    X_brut = df_inconnu[col_spectres].values
    X_brut[X_brut <= 0] = 1e-5
    y_factice = np.zeros(len(X_brut))

    # Boucle sur les algorithmes pour ce fichier
    for algo, df_cat_tissu in catalogues.items():
        nom_algo_court = "RF" if algo == "random_forest" else "XGB"
        df_resultats = pd.DataFrame({"ech": df_inconnu["ech"]})
        modeles_trouves = False

        print(f"--- Application {nom_algo_court} ---")

        for _, row in df_cat_tissu.iterrows():
            compose = row["Compose"]
            chemin_pack = d0 / row["Dossier_Pack_Modeles"]

            print(f" -> Prédiction {compose}...")

            etapes_pre = pre_translation(str(row["Pretraitement"]))
            if len(etapes_pre) > 0:
                panier = {}

                class Interceptor(BaseEstimator, RegressorMixin):
                    def fit(self, X_t, y_t, **kwargs):
                        panier["X"] = X_t
                        return self

                    def predict(self, X_t):
                        return np.zeros(len(X_t))

                try:
                    nirs4all.run(
                        dataset=(X_brut, y_factice),
                        pipeline=etapes_pre + [{"model": Interceptor()}],
                    )
                    X_trans = panier.get("X", X_brut)
                except:
                    X_trans = X_brut
            else:
                X_trans = X_brut

            predictions_10 = []
            for i in range(1, 11):
                fichier_modele = chemin_pack / f"modele_prod_iter_{i}.joblib"
                if fichier_modele.exists():
                    predictions_10.append(joblib.load(fichier_modele).predict(X_trans))

            if predictions_10:
                df_resultats[compose] = np.mean(predictions_10, axis=0).round(4)
                modeles_trouves = True

        if modeles_trouves:
            fichier_excel = dossier_out / f"{nom_algo_court}_{tissu_exact}.xlsx"
            try:
                df_resultats.to_excel(fichier_excel, index=False)
                print(f"  [OK] Sauvegarde : {fichier_excel.name}")
            except Exception:
                fichier_csv_out = dossier_out / f"{nom_algo_court}_{tissu_exact}.csv"
                df_resultats.to_csv(fichier_csv_out, sep=";", index=False)
                print(f"  [OK] Sauvegarde CSV : {fichier_csv_out.name}")

print("\nProcessus par lot Python termine.")
