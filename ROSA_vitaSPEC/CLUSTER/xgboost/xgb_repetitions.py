# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import joblib
import json
import numpy as np
import random

import nirs4all

# Seed globale pour la reproductibilité absolue
SEED = 42
np.random.seed(SEED)
random.seed(SEED)

## mod
from xgboost import XGBRegressor
from sklearn.model_selection import RandomizedSearchCV, GroupKFold

# graphs
import matplotlib.pyplot as plt
import seaborn as sns

## pathing
d0 = Path(
    "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
)
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "xgboost").resolve()))

## Fonctions
from diy_functions.pre_translation import pre_translation
from diy_functions.metrics import calculer_metriques

# %% CHARGEMENT DONNEES 

if len(sys.argv) < 4:
    print("error : il manque des arguments")
    sys.exit(1)

compose = sys.argv[1]
fichier_data = sys.argv[2]
idparam = sys.argv[3]

DATA = d0 / "commun" / fichier_data

## Lecture du fichier R pretraitements
list_pre_tot = d0 / "commun" / "diy_functions" / "list_pre_test_tot.R"
with open(list_pre_tot, "r", encoding="utf-8") as f:
    contenu_r = f.read()

# extrait chaque ligne rbind(...)
liste_pretraitements_r = re.findall(r"rbind\((.*)\)", contenu_r)

## Data
df_data = pd.read_csv(DATA)
# Nettoyage des colonnes inutiles pour nirs4all
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

print(f"{len(df_data)} spectres au total.")
print(f"{len(col_spectres)} longueurs d'ondes")
print(f"{len(liste_pretraitements_r)} prétraitements")

# %% CONFIG MOD XGBOOST

## RandomizedSearchCV (Grille optimisée XGBoost)
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [3, 5, 7, 10],
    "learning_rate": [0.01, 0.05, 0.1, 0.2],
    "subsample": [0.7, 0.8, 0.9, 1.0],
    "colsample_bytree": [0.3, 0.5, 0.8, 1.0],
}

# %% SÉLECTION DU COMPOSÉ ET SYNCHRONISATION DU TEST EXTERNE

print(f"exécution XGBoost Répétitions pour {compose}")

## X et Y
# 1. FORÇAGE NUMÉRIQUE : Isole les anomalies textuelles du fichier frais
df_data[compose] = pd.to_numeric(df_data[compose], errors='coerce')

# 2. NETTOYAGE
df_propre = df_data.dropna(subset=[compose])
print(f"spectres valides : {len(df_propre)} / {len(df_data)}")

# Lecture du sanctuaire créé par le modèle XGB Moyennes (stocké dans le sous-dossier idparam)
chemin_test_externe = d0 / "commun" / idparam / f"valid_externe_{compose}.csv"
try:
    df_sanctuaire = pd.read_csv(chemin_test_externe)
    ech_interdits = df_sanctuaire['ech'].unique()
except FileNotFoundError:
    print(f"error : Le fichier {chemin_test_externe} n'existe pas. Lancez d'abord le modèle de moyennes.")
    sys.exit(1)

# Séparation stricte par blocs d'individus ('ech') pour éviter le data leakage
df_test_externe = df_propre[df_propre['ech'].isin(ech_interdits)]
df_train_val = df_propre[~df_propre['ech'].isin(ech_interdits)]

print(f"Spectres pour l'entraînement/CV : {len(df_train_val)}")
print(f"Spectres isolés pour le crash-test : {len(df_test_externe)}")

y = df_train_val[compose].values
X = df_train_val[col_spectres].values
groupes = df_train_val['ech'].values

# Pré-calcul des plis GroupKFold basés sur 'ech' (5 plis pour lier les répétitions)
gkf = GroupKFold(n_splits=5)
cv_splits = list(gkf.split(X, y, groups=groupes))

# variables pour le meilleur
meilleur_rmsecv_global = float("inf")
meilleur_modele_joblib = None
rapport_du_champion = None

# la liste qui va contenir les résultats de l'exploration
tableau_compose = []

## Boucle sur les prétraitements
for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
    
    # randomSearch XGBoost couplé avec le découpage groupé
    random_search = RandomizedSearchCV(
        estimator=XGBRegressor(
            random_state=SEED, n_jobs=-1, objective="reg:squarederror"
        ),
        param_distributions=param_grid,
        n_iter=30,
        cv=cv_splits, 
        scoring="neg_mean_squared_error",
        random_state=SEED
    )

    # pipeline
    etapes_pretraitement = pre_translation(chaine_r_brute)
    pipeline = etapes_pretraitement + [
        {"model": random_search},
    ]

    try:
        # exec
        resultat = nirs4all.run(dataset=(X, y), pipeline=pipeline)

        # extraction des pred (Uniquement Train)
        results = resultat.predictions.to_dicts()
        y_train_true, pred_train = [], []

        for bloc in results:
            partition = bloc.get("partition", "")
            if partition == "train":
                y_train_true = np.array(bloc.get("y_true", [])).ravel()
                pred_train = np.array(bloc.get("y_pred", [])).ravel()

        # metrics internes (uniquement sur le train)
        rc, _, rmsec, _, _ = calculer_metriques(
            y_train_true, pred_train, y_train_true, pred_train
        )

        modele_actuel = getattr(resultat, "final", resultat)

        # --- MACHINE D'EXTRACTION ROBUSTE DU RMSECV ET DES PARAMÈTRES ---
        meilleurs_params = "{}"
        rmsecv = 0.0
        
        def trouver_search_cv(obj):
            if hasattr(obj, "best_score_") and hasattr(obj, "best_params_"):
                return obj
            if hasattr(obj, "steps"): 
                return trouver_search_cv(obj.steps[-1][1])
            if hasattr(obj, "__getitem__") and not isinstance(obj, (str, dict, pd.DataFrame, np.ndarray)):
                try:
                    return trouver_search_cv(obj[-1])
                except:
                    pass
            if isinstance(obj, dict) and "model" in obj:
                return trouver_search_cv(obj["model"])
            if hasattr(obj, "model"): 
                return trouver_search_cv(obj.model)
            return None

        recherche_sk = trouver_search_cv(modele_actuel)

        if recherche_sk is not None:
            meilleurs_params = str(recherche_sk.best_params_)
            score_neg_mse = recherche_sk.best_score_
            rmsecv = float(np.sqrt(abs(score_neg_mse)))
        else:
            meilleurs_params = "Erreur extraction"
            rmsecv = 0.0 

        # ajoute cette combinaison dans le tableau géant
        ligne_resultat = {
            "Compose": compose,
            "ID_Pretraitement": id_pre + 1,
            "Code_R_Pretraitement": chaine_r_brute,
            "Meilleurs_Hyperparam_XGB": str(meilleurs_params),
            "Rc": round(rc, 4),
            "RMSEC": round(rmsec, 4),
            "RMSECV": round(rmsecv, 4),
        }
        tableau_compose.append(ligne_resultat)

        # si meilleur : on garde
        if rmsecv > 0 and rmsecv < meilleur_rmsecv_global:
            meilleur_rmsecv_global = rmsecv
            meilleur_modele_joblib = modele_actuel

            # json du meilleur
            rapport_du_champion = {
                "Compose": compose,
                "Pretraitement_Gagnant": chaine_r_brute,
                "Meilleurs_Parametres": meilleurs_params,
                "Metriques_Internes": {
                    "Rc": round(rc, 4),
                    "RMSEC": round(rmsec, 4),
                    "RMSECV": round(rmsecv, 4),
                },
            }

    except Exception as e:
        print(f"error {id_pre + 1} : {e}")
        continue

## Save
dossier_compose = d0 / "xgboost" / "repetitions" / "Results" / idparam / compose
dossier_compose.mkdir(parents=True, exist_ok=True)

# tableau du compose
df_compose = pd.DataFrame(tableau_compose)

chemin_csv_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_REP_{compose}.csv"
df_compose.to_csv(chemin_csv_compose, sep=";", index=False)

try:
    chemin_excel_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_REP_{compose}.xlsx"
    df_compose.to_excel(chemin_excel_compose, index=False)
except ModuleNotFoundError:
    pass

# json meilleur et TEST EXTERNE FINAL
if meilleur_modele_joblib is not None:
    print(f"?? Meilleur modèle XGBoost Répétitions validé (RMSECV: {meilleur_rmsecv_global:.4f})")

    # ÉPREUVE DU FEU : Crash-test sur les répétitions des 15 individus exclus
    try:
        X_ext = df_test_externe[col_spectres].values
        y_ext_true = df_test_externe[compose].values
        
        pred_ext = meilleur_modele_joblib.predict(X_ext)
        if isinstance(pred_ext, dict) and "y_pred" in pred_ext:
             pred_ext = np.array(pred_ext["y_pred"]).ravel()
        else:
             pred_ext = np.array(pred_ext).ravel()

        _, _, _, rmsep_ext, rpd_ext = calculer_metriques(
            y_ext_true, pred_ext, y_ext_true, pred_ext 
        )
        
        rapport_du_champion["Crash_Test_Externe"] = {
            "Avertissement": "Validation finale calculée sur l'intégralité des spectres répétitions des 15 individus exclus.",
            "RMSEP_Externe": round(rmsep_ext, 4),
            "RPD_Externe": round(rpd_ext, 4)
        }
        print(f"?? SCORE INVIOLABLE (XGBoost Modèle B) -> RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")
        
    except Exception as e_test:
        print(f"?? error test externe : {e_test}")

    # Sauvegarde du modèle physique (.joblib)
    chemin_modele = dossier_compose / f"modele_XGB_B_{compose}.joblib"
    joblib.dump(meilleur_modele_joblib, chemin_modele)

    # Sauvegarde du rapport JSON
    with open(dossier_compose / f"rapport_B_{compose}.json", "w", encoding="utf-8") as f:
        json.dump(rapport_du_champion, f, indent=4)
else:
    print(f"no mod pour {compose}.")

## GRAPHS
if meilleur_modele_joblib is not None:
    sns.set_theme(style="whitegrid")  

    ## graph robustesse (obverfitting)
    plt.figure(figsize=(10, 6))

    sns.scatterplot(
        data=df_compose,
        x="RMSECV",
        y="RMSEC",
        color="lightgray",
        alpha=0.8,
        edgecolor="gray",
        label="Prétraitements testés",
    )

    champion_row = df_compose.loc[df_compose["RMSECV"].idxmin()]
    plt.scatter(
        champion_row["RMSECV"],
        champion_row["RMSEC"],
        color="crimson",
        s=150,
        edgecolor="black",
        linewidth=1.5,
        label="?? Champion Absolu",
        zorder=5,
    )

    min_val = min(df_compose["RMSECV"].min(), df_compose["RMSEC"].min())
    max_val = max(df_compose["RMSECV"].max(), df_compose["RMSEC"].max())

    plt.plot(
        [min_val * 0.9, max_val * 1.1],
        [min_val * 0.9, max_val * 1.1],
        "k--",
        alpha=0.5,
        label="y = x",
    )

    plt.title(
        f"Overfitting pretraitements (XGBoost Répétitions) - {compose}",
        fontsize=14,
        fontweight="bold",
    )
    plt.xlabel("RMSEcv", fontsize=12)
    plt.ylabel("RMSEC