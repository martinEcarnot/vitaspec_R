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

# Forçage du type pour éviter l'erreur de classification nirs4all, et ajout d'un très léger bruit de fond
y = df_train_val[compose].values.astype(float) + np.random.normal(0, 1e-5, size=len(df_train_val))
X = df_train_val[col_spectres].values
groupes = df_train_val['ech'].values

# Pré-calcul des plis GroupKFold basés sur 'ech' (5 plis pour lier les répétitions)
gkf = GroupKFold(n_splits=5)
cv_splits = list(gkf.split(X, y, groups=groupes))

# variables pour le meilleur
meilleur_rmsecv_global = float("inf")
meilleur_modele_joblib = None
meilleur_pipeline_pre = []
rapport_du_champion = None

# la liste qui va contenir les résultats de l'exploration
tableau_compose = []

## Boucle sur les prétraitements
for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
    try:
        etapes_pretraitement = pre_translation(chaine_r_brute)
        
        # ---------------------------------------------------------
        # ÉTAPE 1 : PRÉTRAITEMENT PUR AVEC NIRS4ALL
        # ---------------------------------------------------------
        if len(etapes_pretraitement) > 0:
            from sklearn.dummy import DummyRegressor
            class InterceptorRegressor(DummyRegressor):
                def fit(self, X_t, y_t, **kwargs):
                    self.X_intercepted = X_t
                    return super().fit(X_t, y_t, **kwargs)
            
            interceptor = InterceptorRegressor()
            pipeline_intercept = etapes_pretraitement + [{"model": interceptor}]
            nirs4all.run(dataset=(X, y), pipeline=pipeline_intercept)
            
            X_transforme = interceptor.X_intercepted
        else:
            X_transforme = X
            
        if X_transforme.shape[1] == 0:
            raise ValueError(f"Le prétraitement a supprimé toutes les variables.")

        # ---------------------------------------------------------
        # ÉTAPE 2 : ENTRAÎNEMENT PUR ET TRANSPARENT AVEC SCIKIT-LEARN
        # ---------------------------------------------------------
        random_search = RandomizedSearchCV(
            estimator=XGBRegressor(random_state=SEED, n_jobs=-1, objective="reg:squarederror"),
            param_distributions=param_grid,
            n_iter=30,
            cv=cv_splits, 
            scoring="neg_mean_squared_error",
            random_state=SEED,
        )
        
        random_search.fit(X_transforme, y)
        
        # ---------------------------------------------------------
        # ÉTAPE 3 : EXTRACTION GARANTIE
        # ---------------------------------------------------------
        meilleurs_params = str(random_search.best_params_)
        score_neg_mse = random_search.best_score_
        rmsecv = float(np.sqrt(abs(score_neg_mse)))

        pred_train = random_search.predict(X_transforme)
        rc, _, rmsec, _, _ = calculer_metriques(y, pred_train, y, pred_train)

        ligne_resultat = {
            "Compose": compose,
            "ID_Pretraitement": id_pre + 1,
            "Code_R_Pretraitement": chaine_r_brute,
            "Meilleurs_Hyperparam_XGB": meilleurs_params,
            "Rc": round(rc, 4),
            "RMSEC": round(rmsec, 4),
            "RMSECV": round(rmsecv, 4),
        }
        tableau_compose.append(ligne_resultat)

        # --- SÉCURITÉ FAIL-FAST ---
        if id_pre == 0 and rmsecv == 0.0:
            print("\n?? FAIL-FAST: RMSECV à 0 dès le premier modèle. L'entraînement explicite Scikit-Learn a échoué.")
            sys.exit(1)

        # si meilleur : on garde
        if rmsecv > 0 and rmsecv < meilleur_rmsecv_global:
            meilleur_rmsecv_global = rmsecv
            meilleur_modele_joblib = random_search 
            meilleur_pipeline_pre = etapes_pretraitement

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
        print(f"error {id_pre + 1} ({chaine_r_brute}) : {e}")
        continue

## Save
dossier_compose = d0 / "xgboost" / "Repetitions" / "Results" / idparam / compose
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
        
        # Application du meilleur prétraitement à X_ext
        if len(meilleur_pipeline_pre) > 0:
            from sklearn.dummy import DummyRegressor
            class InterceptorPredict(DummyRegressor):
                def fit(self, X_t, y_t, **kwargs):
                    self.X_intercepted = X_t
                    return super().fit(X_t, y_t, **kwargs)
                    
            interceptor_ext = InterceptorPredict()
            pipeline_ext = meilleur_pipeline_pre + [{"model": interceptor_ext}]
            nirs4all.run(dataset=(X_ext, y_ext_true), pipeline=pipeline_ext)
            X_ext_transforme = interceptor_ext.X_intercepted
        else:
            X_ext_transforme = X_ext

        # Prédiction avec le modèle pur XGBoost
        pred_ext = meilleur_modele_joblib.predict(X_ext_transforme)
        
        _, _, _, rmsep_ext, rpd_ext = calculer_metriques(
            y_ext_true, pred_ext, y_ext_true, pred_ext 
        )
        
        rapport_du_champion["Crash_Test_Externe"] = {
            "Avertissement": "Validation finale calculée sur l'intégralité des spectres répétitions des individus exclus.",
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
    plt.ylabel("RMSEC", fontsize=12)
    plt.legend()

    chemin_graph_robustesse_png = dossier_compose / f"Graph_robustesse_XGB_{compose}.png"
    chemin_graph_robustesse_pdf = dossier_compose / f"Graph_robustesse_XGB_{compose}.pdf"
    plt.savefig(chemin_graph_robustesse_png, dpi=300, bbox_inches="tight")
    plt.savefig(chemin_graph_robustesse_pdf, dpi=300, bbox_inches="tight")
    plt.close()

    ## graph feature importance (stem plot)
    try:
        fitted_mod = meilleur_modele_joblib.best_estimator_
        importances = fitted_mod.feature_importances_
        
        plt.figure(figsize=(10, 5))

        toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
        pre_gagnant = rapport_du_champion["Pretraitement_Gagnant"]

        match_reduction = re.search(
            r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", pre_gagnant
        )

        if match_reduction:
            drop_start = int(match_reduction.group(1))
            drop_end = int(match_reduction.group(2))
            step = int(match_reduction.group(3))

            end_idx = len(toutes_longueurs) - drop_end
            x_values = toutes_longueurs[drop_start:end_idx:step]
        else:
            x_values = toutes_longueurs

        if len(x_values) != len(importances):
            print(f"?? Alignement forcé : {len(x_values)} longueurs d'ondes vs {len(importances)} importances.")
            x_values = list(range(len(importances)))
            xlabel_text = "Index des variables (longueurs d'ondes désalignées)"
        else:
            xlabel_text = "Longueur d'onde (nm)"

        couleur_graph = "forestgreen" 
        
        plt.vlines(x=x_values, ymin=0, ymax=importances, color=couleur_graph, linewidth=1, alpha=0.7)
        plt.plot(
            x_values,
            importances,
            marker="o",
            markersize=2.5,
            color=couleur_graph,
            linestyle="None",
        )
        plt.title(
            f"Importance des variables (XGBoost Répétitions) - {compose}",
            fontsize=16,
            fontweight="bold",
            pad=15,
        )
        plt.ylabel("Importance", fontsize=12)
        plt.xlabel(xlabel_text, fontsize=12)

        plt.grid(True, linestyle="--", alpha=0.5)
        plt.gca().spines["top"].set_visible(False)
        plt.gca().spines["right"].set_visible(False)

        chemin_graph_importance_png = (
            dossier_compose / f"Graph_feature_importance_XGB_{compose}.png"
        )
        chemin_graph_importance_pdf = (
            dossier_compose / f"Graph_feature_importance_XGB_{compose}.pdf"
        )
        plt.savefig(chemin_graph_importance_png, dpi=300, bbox_inches="tight")
        plt.savefig(chemin_graph_importance_pdf, dpi=300, bbox_inches="tight")
        plt.close()

    except Exception as e_graph:
        print(f"? error graph importance pour {compose} : {e_graph}")