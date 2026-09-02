# -*- coding: utf-8 -*-

# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import numpy as np
import random
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin
from xgboost import XGBRegressor
from sklearn.model_selection import RandomizedSearchCV, KFold, train_test_split
from sklearn.metrics import r2_score

# %% CONFIGURATION CHEMINS ET FONCTIONS
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "xgboost").resolve()))

from diy_functions.pre_translation import pre_translation
from diy_functions.metrics import calculer_metriques

# %% CHARGEMENT DONNEES ET ARGUMENTS
if len(sys.argv) < 4:
    print("erreur : il manque des arguments")
    sys.exit(1)

compose = sys.argv[1]
fichier_data = sys.argv[2]
idparam = sys.argv[3]

DATA = d0 / "commun" / fichier_data

# Lecture des pretraitements
list_pre_tot = d0 / "commun" / "diy_functions" / "list_pre_test_tot.R"
with open(list_pre_tot, "r", encoding="utf-8") as f:
    contenu_r = f.read()

liste_pretraitements_r = re.findall(r"rbind\((.*)\)", contenu_r)

# Preparation des donnees
df_data = pd.read_csv(DATA)
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]
df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
df_propre = df_data.dropna(subset=[compose])

print(f"[{compose}] Lancement XGB Monte-Carlo ({len(df_propre)} echantillons valides)")

# %% PARAMETRES DU XGBOOST
NB_ITERATIONS = 5

# Ajout des parametres de penalisation L1 (reg_alpha) et L2 (reg_lambda) demandés
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [3, 5, 7, 10],
    "learning_rate": [0.01, 0.05, 0.1, 0.2],
    "subsample": [0.7, 0.8, 0.9, 1.0],
    "colsample_bytree": [0.3, 0.5, 0.8, 1.0],
    "reg_alpha": [0, 0.01, 0.1, 1, 10],  
    "reg_lambda": [0, 0.01, 0.1, 1, 10], 
}

bilan_monte_carlo = []
dossier_compose = d0 / "xgboost" / "moyennes" / "Results" / idparam / compose
dossier_compose.mkdir(parents=True, exist_ok=True)

# %% BOUCLE MONTE-CARLO
for iteration in range(1, NB_ITERATIONS + 1):
    
    SEED_ITER = random.randint(1, 999999)
    np.random.seed(SEED_ITER)
    random.seed(SEED_ITER)
    kf = KFold(n_splits=5, shuffle=True, random_state=SEED_ITER)
    
    print(f"\n--- ITERATION {iteration}/{NB_ITERATIONS} (Seed: {SEED_ITER}) ---")
    
    dossier_iter = dossier_compose / f"iter_{iteration}"
    dossier_iter.mkdir(parents=True, exist_ok=True)
    
    df_train_val, df_test_externe = train_test_split(df_propre, test_size=30, random_state=SEED_ITER)
    
    y = df_train_val[compose].values.astype(float) + np.random.normal(0, 1e-5, size=len(df_train_val))
    X = df_train_val[col_spectres].values
    X[X <= 0] = 1e-5
    
    meilleur_rmsecv_iter = float("inf")
    meilleur_pre_iter = None
    meilleur_modele_iter = None
    meilleur_pipeline_iter = []
    
    tableau_pre_iter = []
    
    # Test des 30 pretraitements
    for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
        try:
            etapes_pretraitement = pre_translation(chaine_r_brute)
            
            if len(etapes_pretraitement) > 0:
                panier_donnees = {}
                class InterceptorRegressor(BaseEstimator, RegressorMixin):
                    def fit(self, X_t, y_t, **kwargs):
                        panier_donnees['X_transforme'] = X_t
                        return self
                    def predict(self, X_t): return np.zeros(len(X_t))
                
                pipeline_intercept = etapes_pretraitement + [{"model": InterceptorRegressor()}]
                
                try: nirs4all.run(dataset=(X, y), pipeline=pipeline_intercept)
                except Exception: pass
                
                X_transforme = panier_donnees.get('X_transforme', X)
            else:
                X_transforme = X
                
            if X_transforme is None or X_transforme.shape[1] == 0:
                continue

            random_search = RandomizedSearchCV(
                estimator=XGBRegressor(random_state=SEED_ITER, n_jobs=-1, objective="reg:squarederror", tree_method="hist"),
                param_distributions=param_grid,
                n_iter=30,
                cv=kf,
                scoring="neg_mean_squared_error",
                random_state=SEED_ITER,
            )
            
            random_search.fit(X_transforme, y)
            
            # Recuperation du score et RMSEC pour le graphique de robustesse
            score_neg_mse = random_search.best_score_
            rmsecv = float(np.sqrt(abs(score_neg_mse)))
            pred_train = random_search.predict(X_transforme)
            _, _, rmsec, _, _ = calculer_metriques(y, pred_train, y, pred_train)
            
            tableau_pre_iter.append({
                "Pretraitement": chaine_r_brute,
                "RMSEC": rmsec,
                "RMSECV": rmsecv
            })
            
            if 0 < rmsecv < meilleur_rmsecv_iter:
                meilleur_rmsecv_iter = rmsecv
                meilleur_pre_iter = chaine_r_brute
                meilleur_modele_iter = random_search
                meilleur_pipeline_iter = etapes_pretraitement
                
        except Exception as e:
            continue
            
    # CRASH TEST ET GRAPHIQUES POUR CETTE ITERATION
    if meilleur_modele_iter is not None:
        X_ext = df_test_externe[col_spectres].values
        y_ext_true = df_test_externe[compose].values
        
        if len(meilleur_pipeline_iter) > 0:
            panier_ext = {}
            class InterceptorPredict(BaseEstimator, RegressorMixin):
                def fit(self, X_t, y_t, **kwargs):
                    panier_ext['X_transforme'] = X_t
                    return self
                def predict(self, X_t): return np.zeros(len(X_t))
                
            pipeline_ext = meilleur_pipeline_iter + [{"model": InterceptorPredict()}]
            
            try: nirs4all.run(dataset=(X_ext, y_ext_true), pipeline=pipeline_ext)
            except: pass 
                
            X_ext_transforme = panier_ext.get('X_transforme', X_ext)
        else:
            X_ext_transforme = X_ext

        pred_ext = meilleur_modele_iter.predict(X_ext_transforme)
        
        _, _, _, rmsep_ext, rpd_ext = calculer_metriques(y_ext_true, pred_ext, y_ext_true, pred_ext)
        r2p_ext = r2_score(y_ext_true, pred_ext)
        
        print(f"Gagnant Iteration {iteration} : {meilleur_pre_iter[:40]}... -> RMSECV: {meilleur_rmsecv_iter:.4f} | R2p: {r2p_ext:.4f}")
        
        bilan_monte_carlo.append({
            "Iteration": iteration,
            "Pretraitement_Gagnant": meilleur_pre_iter,
            "R2p_Externe": round(r2p_ext, 4),
            "RMSEP_Externe": round(rmsep_ext, 4),
            "RPD_Externe": round(rpd_ext, 4),
            "RMSECV": round(meilleur_rmsecv_iter, 4)
        })
        
        df_predictions_ext = pd.DataFrame({
            "Vraie_Valeur": y_ext_true,
            "Valeur_Predite": pred_ext
        })

        # --- GENERATION DES GRAPHIQUES ---
        chemin_rapport_pdf = dossier_iter / f"RAPPORT_GRAPHIQUES_MC_{compose}.pdf"
        df_robustesse = pd.DataFrame(tableau_pre_iter)
        
        with PdfPages(chemin_rapport_pdf) as pdf:
            
            # Graphique 1 : Robustesse
            try:
                with plt.style.context('ggplot'):
                    fig_rob, ax_rob = plt.subplots(figsize=(8, 6))
                    ax_rob.scatter(df_robustesse["RMSECV"], df_robustesse["RMSEC"], facecolors='none', edgecolors='black', s=40, alpha=0.7, label="Pretraitements testes")
                    
                    champion_row = df_robustesse.loc[df_robustesse["RMSECV"].idxmin()]
                    ax_rob.scatter(champion_row["RMSECV"], champion_row["RMSEC"], color='none', s=70, edgecolor="black", linewidth=1.5, label="Gagnant iter")
                    
                    min_val = min(df_robustesse["RMSECV"].min(), df_robustesse["RMSEC"].min())
                    max_val = max(df_robustesse["RMSECV"].max(), df_robustesse["RMSEC"].max())
                    lims = [min_val * 0.9, max_val * 1.1]
                    ax_rob.plot(lims, lims, color="red", linestyle="--", linewidth=1.5, label="y = x")
                    
                    ax_rob.set_title(f"Overfitting pretraitements (Iter {iteration})", fontsize=13)
                    ax_rob.set_xlabel("RMSEcv")
                    ax_rob.set_ylabel("RMSEC")
                    ax_rob.legend()
                    ax_rob.set_xlim(lims)
                    ax_rob.set_ylim(lims)
                    pdf.savefig(fig_rob, bbox_inches="tight")
                    plt.close(fig_rob)
            except Exception as e_g1:
                print(f"Erreur graph 1 : {e_g1}")

            # Graphique 2 : Scatter Plot
            try:
                with plt.style.context('ggplot'):
                    fig_pub, ax_pub = plt.subplots(figsize=(8, 6))
                    ax_pub.scatter(pred_ext, y_ext_true, facecolors='none', edgecolors='black', s=35, alpha=0.7)
                    
                    z = np.polyfit(pred_ext, y_ext_true, 1)
                    p = np.poly1d(z)
                    x_line = np.linspace(pred_ext.min(), pred_ext.max(), 100)
                    ax_pub.plot(x_line, p(x_line), color='blue', linestyle='-', linewidth=1.5)
                    
                    min_val = min(pred_ext.min(), y_ext_true.min())
                    max_val = max(pred_ext.max(), y_ext_true.max())
                    limites = [min_val - (max_val-min_val)*0.05, max_val + (max_val-min_val)*0.05]
                    ax_pub.plot(limites, limites, color="red", linestyle="--", linewidth=1.5)
                    
                    texte = f"R2p = {r2p_ext:.3f}\nRMSEp = {rmsep_ext:.3f}\nRPD = {rpd_ext:.3f}"
                    ax_pub.text(0.02, 0.96, texte, transform=ax_pub.transAxes, fontsize=12, verticalalignment='top')
                    ax_pub.set_title(f"Predictions vs Mesures (Iter {iteration})", fontsize=13)
                    ax_pub.set_xlabel("Valeurs predites")
                    ax_pub.set_ylabel("Valeurs mesurees")
                    pdf.savefig(fig_pub, bbox_inches="tight")
                    plt.close(fig_pub)
            except Exception as e_g2:
                print(f"Erreur graph 2 : {e_g2}")

            # Graphique 3 : Feature Importance XGBoost
            try:
                with plt.style.context('default'):
                    fig_stem, ax_stem = plt.subplots(figsize=(8, 5))
                    toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
                    x_values = toutes_longueurs.copy()

                    match_red = re.search(r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", meilleur_pre_iter)
                    if match_red:
                        drop_start, drop_end, step = int(match_red.group(1)), int(match_red.group(2)), int(match_red.group(3))
                        x_values = x_values[drop_start : len(x_values)-drop_end : step]
                        
                    match_sder = re.search(r"list\('sder',\s*c\(\s*\d+\s*,\s*\d+\s*,\s*(\d+)\s*\)", meilleur_pre_iter)
                    if match_sder:
                        points_lost = (int(match_sder.group(1)) - 1) // 2
                        if points_lost > 0: x_values = x_values[points_lost : -points_lost]

                    importances = meilleur_modele_iter.best_estimator_.feature_importances_
                    if len(x_values) != len(importances):
                        x_values = np.linspace(min(x_values), max(x_values), len(importances))

                    ax_stem.vlines(x=x_values, ymin=0, ymax=importances, color="#4169E1", linewidth=1.5, alpha=0.8)
                    ax_stem.plot(x_values, importances, marker='o', markersize=2, color="#4169E1", linestyle='None')
                    ax_stem.axhline(y=0, color='gray', linewidth=0.8)
                    
                    ax_stem.set_title(f"XGB Importance Variables (Iter {iteration})", fontsize=13, fontweight='bold')
                    ax_stem.set_ylabel("Importance")
                    ax_stem.set_xlabel("Longueurs d'ondes (nm)")
                    pdf.savefig(fig_stem, bbox_inches="tight")
                    plt.close(fig_stem)
            except Exception as e_g3:
                print(f"Erreur graph 3 : {e_g3}")

# %% SAUVEGARDE FINALE DU BILAN MONTE-CARLO
df_bilan = pd.DataFrame(bilan_monte_carlo)
chemin_csv_bilan = dossier_compose / f"BILAN_MONTE_CARLO_{compose}.csv"
df_bilan.to_csv(chemin_csv_bilan, sep=";", index=False)

print(f"\nFin du script. Fichier enregistre : {chemin_csv_bilan}")