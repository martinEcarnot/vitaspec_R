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

# Seed globale
SEED = 42
np.random.seed(SEED)
random.seed(SEED)

## mod
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RandomizedSearchCV, KFold, train_test_split

# graphs
import matplotlib.pyplot as plt
import seaborn as sns

## pathing
d0 = Path(
    "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
)
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

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
col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

print(f"{len(df_data)} échantillons.")
print(f"{len(col_spectres)} longueurs d'ondes")
print(f"{len(liste_pretraitements_r)} prétraitements")

# %% CONFIG MOD

## RandomizedSearchCV
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [None, 10, 20],
    "min_samples_split": [2, 5, 10],
    "min_samples_leaf": [1, 2, 4],
    "max_features": ["sqrt", "log2", 0.3],
}

kf = KFold(n_splits=5, shuffle=True, random_state=SEED)

# %% SÉLECTION DU COMPOSÉ

print(f"exécution RF moyennes pour {compose}")

## X et Y

df_data[compose] = pd.to_numeric(df_data[compose], errors='coerce')
df_propre = df_data.dropna(subset=[compose])
print(f"echantillons valides : {len(df_propre)} / {len(df_data)}")

# isole 15 ech pour la valid externe (Le Sanctuaire)
df_train_val, df_test_externe = train_test_split(df_propre, test_size=15, random_state=SEED)

# save jeu valid
chemin_test_externe = d0 / "commun" / f"valid_externe_{compose}.csv"
df_test_externe.to_csv(chemin_test_externe, index=False)
print(f"echantillons pour train + test : {len(df_train_val)}")

y = df_train_val[compose].values
X = df_train_val[col_spectres].values

# variables pour le meilleur
meilleur_rmsecv_global = float("inf")
meilleur_modele_joblib = None
rapport_du_champion = None

# la liste qui va contenir les résultats JUSTE pour ce composé
tableau_compose = []

## Boucle sur les prétraitements
for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
    
    # randomSearch avec 5-Fold
    random_search = RandomizedSearchCV(
        estimator=RandomForestRegressor(random_state=SEED, n_jobs=-1),
        param_distributions=param_grid,
        n_iter=30,
        cv=kf,
        scoring="neg_mean_squared_error",
        random_state=SEED
    )

    # pipeline épuré
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

        # extraction des meilleurs hyperparamètres et du RMSECV
        df_summary = resultat.predictions.to_pandas()
        meilleurs_params = df_summary.iloc[0].get("best_params", "Non trouvé")

        def securiser_nombre(valeur):
            if valeur is None:
                return 0.0
            try:
                return float(valeur)
            except:
                return 0.0

        if "rmsecv" in df_summary.columns:
            rmsecv = securiser_nombre(df_summary.iloc[0]["rmsecv"])
        elif "val_score" in df_summary.columns:
            rmsecv = securiser_nombre(df_summary.iloc[0]["val_score"])
        else:
            rmsecv = securiser_nombre(getattr(resultat, "best_rmse", 0.0))

        modele_actuel = getattr(resultat, "final", resultat)

        # metrics internes (uniquement sur le train)
        rc, _, rmsec, _, _ = calculer_metriques(
            y_train_true, pred_train, y_train_true, pred_train
        )

        # ajoute cette combinaison dans le tableau géant
        ligne_resultat = {
            "Compose": compose,
            "ID_Pretraitement": id_pre + 1,
            "Code_R_Pretraitement": chaine_r_brute,
            "Meilleurs_Hyperparam_RF": str(meilleurs_params),
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
dossier_compose = d0 / "random_forest" / "moyennes" / "Results" / idparam / compose
dossier_compose.mkdir(parents=True, exist_ok=True)

# tableau du compose
df_compose = pd.DataFrame(tableau_compose)

chemin_csv_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_{compose}.csv"
df_compose.to_csv(chemin_csv_compose, sep=";", index=False)

try:
    chemin_excel_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_{compose}.xlsx"
    df_compose.to_excel(chemin_excel_compose, index=False)
except ModuleNotFoundError:
    pass

# json meilleur + TEST EXTERNE
if meilleur_modele_joblib is not None:
    print(f"Meilleur modèle (RMSECV: {meilleur_rmsecv_global:.4f})")

    # Test sur les 15 échantillons externes
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
            "RMSEP_Externe": round(rmsep_ext, 4),
            "RPD_Externe": round(rpd_ext, 4)
        }
        print(f"test : RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")
        
    except Exception as e_test:
        print(f"error test : {e_test}")

    # Save du modèle physique (.joblib)
    chemin_modele = dossier_compose / f"modele_RF_A_{compose}.joblib"
    joblib.dump(meilleur_modele_joblib, chemin_modele)

    # Save du rapport JSON
    with open(dossier_compose / f"rapport_A_{compose}.json", "w", encoding="utf-8") as f:
        json.dump(rapport_du_champion, f, indent=4)
else:
    print(f"no mod pour {compose}.")

## GRAPHS
if meilleur_modele_joblib is not None:
    sns.set_theme(style="whitegrid")  

    ## graph robustesse (obverfitting)
    # on trace RMSECV vs RMSEC car le RMSEP interne n'existe plus
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
        label="🏆 Champion Absolu",
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
        f"overfitting_pretrait_{compose}",
        fontsize=14,
        fontweight="bold",
    )
    plt.xlabel("RMSEcv", fontsize=12)
    plt.ylabel("RMSEC", fontsize=12)
    plt.legend()

    chemin_graph_robustesse_png = dossier_compose / f"Graph_robustesse_{compose}.png"
    chemin_graph_robustesse_pdf = dossier_compose / f"Graph_robustesse_{compose}.pdf"
    plt.savefig(chemin_graph_robustesse_png, dpi=300, bbox_inches="tight")
    plt.savefig(chemin_graph_robustesse_pdf, dpi=300, bbox_inches="tight")
    plt.close()

    ## graph feature importance (stem plot)
    try:
        if hasattr(meilleur_modele_joblib, "__getitem__"):
            dernier_element = meilleur_modele_joblib[-1]
            if isinstance(dernier_element, dict) and "model" in dernier_element:
                fitted_rf = dernier_element["model"].best_estimator_
            else:
                fitted_rf = dernier_element.best_estimator_
        else:
            fitted_rf = meilleur_modele_joblib.best_estimator_

        importances = fitted_rf.feature_importances_

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

        xlabel_text = "longueur d'onde (nm)"

        plt.vlines(x=x_values, ymin=0, ymax=importances, color="blue", linewidth=1)
        plt.plot(
            x_values,
            importances,
            marker="o",
            markersize=2,
            color="blue",
            linestyle="None",
        )
        plt.title(
            f"RF - {compose} (Variables: {len(importances)})",
            fontsize=16,
            fontweight="bold",
            pad=15,
        )
        plt.ylabel("Importance", fontsize=12)
        plt.xlabel(xlabel_text, fontsize=12)

        plt.grid(False)
        plt.gca().spines["top"].set_visible(True)
        plt.gca().spines["right"].set_visible(True)

        chemin_graph_importance_png = (
            dossier_compose / f"Graph_feature_importance_{compose}.png"
        )
        chemin_graph_importance_pdf = (
            dossier_compose / f"Graph_feature_importance_{compose}.pdf"
        )
        plt.savefig(chemin_graph_importance_png, dpi=300, bbox_inches="tight")
        plt.savefig(chemin_graph_importance_pdf, dpi=300, bbox_inches="tight")
        plt.close()

    except Exception as e_graph:
        print(f"error pour {compose} : {e_graph}")