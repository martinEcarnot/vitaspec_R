# -*- coding: utf-8 -*-

# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import joblib
import numpy as np
import random
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin
from xgboost import XGBRegressor
from sklearn.model_selection import GridSearchCV, KFold, train_test_split
from sklearn.base import clone
from sklearn.metrics import r2_score

# %% CHEMINS ET FONCTIONS
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "xgboost").resolve()))

from diy_functions.pre_translation import pre_translation
from diy_functions.metrics import calculer_metriques

# %% CHARGEMENT ET LECTURE DU GAGNANT
if len(sys.argv) < 4:
    print("error : il manque des arguments")
    sys.exit(1)

compose = sys.argv[1]
fichier_data = sys.argv[2]
idparam = sys.argv[3]

DATA = d0 / "commun" / fichier_data

# Lecture dans le dossier Results ou le consensus a ete genere
dossier_consensus = d0 / "xgboost" / "moyennes" / "Results" / idparam / compose
# Sauvegarde dans le nouveau dossier test_final
dossier_base_test = d0 / "xgboost" / "moyennes" / "test_final" / idparam / compose
dossier_base_test.mkdir(parents=True, exist_ok=True)

chemin_excel_synthese = dossier_consensus / f"SYNTHESE_PRETRAITEMENTS_{compose}.xlsx"

try:
    df_synthese = pd.read_excel(chemin_excel_synthese)
    chaine_r_brute = df_synthese.iloc[0]["Pretraitement_Gagnant"]
    print(f"Pretraitement fige selectionne : {chaine_r_brute}")
except Exception as e:
    print(f"Erreur : Impossible de lire {chemin_excel_synthese.name}.")
    sys.exit(1)

# Preparation Data
df_data = pd.read_csv(DATA)
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]
df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
df_propre = df_data.dropna(subset=[compose])

print(f"\nDebut Validation Finale XGBoost (10 iterations) pour: {compose}")

# %% CONFIGURATION GRIDSEARCH
# Grille allegee (648 combinaisons) pour eviter une explosion combinatoire (des semaines de calculs)
param_grid = {
    "n_estimators": [100, 300, 500],
    "max_depth": [3, 5, 7],
    "learning_rate": [0.05, 0.1],
    "subsample": [0.8, 1.0],
    "colsample_bytree": [0.5, 1.0],
    "reg_alpha": [0, 0.1, 1],
    "reg_lambda": [0, 0.1, 1],
}

NB_ITERATIONS = 10
bilan_validation = []

# %% BOUCLE DE VALIDATION
for iteration in range(1, NB_ITERATIONS + 1):
    
    SEED_ITER = random.randint(1, 999999)
    np.random.seed(SEED_ITER)
    random.seed(SEED_ITER)
    kf = KFold(n_splits=5, shuffle=True, random_state=SEED_ITER)
    
    print(f"\n{'='*60}")
    print(f" ITERATION {iteration}/{NB_ITERATIONS} - SEED: {SEED_ITER}")
    print(f"{'='*60}")

    df_train_val, df_test_externe = train_test_split(df_propre, test_size=30, random_state=SEED_ITER)

    dossier_iter = dossier_base_test / f"iter_{iteration}"
    dossier_iter.mkdir(parents=True, exist_ok=True)
    
    df_test_externe.to_csv(dossier_iter / f"valid_externe_{compose}.csv", index=False)

    y = df_train_val[compose].values.astype(float) + np.random.normal(0, 1e-5, size=len(df_train_val))
    X = df_train_val[col_spectres].values
    X[X <= 0] = 1e-5

    # 1. APPLICATION DU PRETRAITEMENT FIGE
    etapes_pretraitement = pre_translation(chaine_r_brute)
    
    if len(etapes_pretraitement) > 0:
        panier_donnees = {}
        class InterceptorRegressor(BaseEstimator, RegressorMixin):
            def fit(self, X_t, y_t, **kwargs):
                panier_donnees['X_transforme'] = X_t
                return self
            def predict(self, X_t): return np.zeros(len(X_t))
        
        pipeline_intercept = etapes_pretraitement + [{"model": InterceptorRegressor()}]
        
        try:
            nirs4all.run(dataset=(X, y), pipeline=pipeline_intercept)
        except Exception:
            pass # Ignore erreur base de donnees sqlite
            
        X_transforme = panier_donnees.get('X_transforme', X)
    else:
        X_transforme = X

    # 2. GRID SEARCH
    print(f"Recherche GridSearchCV en cours (648 combinaisons * 5 folds)...")
    grid_search = GridSearchCV(
        estimator=XGBRegressor(random_state=SEED_ITER, n_jobs=-1, objective="reg:squarederror", tree_method="hist"),
        param_grid=param_grid,
        cv=kf,
        scoring="neg_mean_squared_error",
        return_train_score=True
    )
    
    grid_search.fit(X_transforme, y)
    
    meilleurs_params = str(grid_search.best_params_)
    score_neg_mse = grid_search.best_score_
    rmsecv = float(np.sqrt(abs(score_neg_mse)))
    
    variance_y = np.var(y)
    r2cv = 1 - (abs(score_neg_mse) / variance_y) if variance_y != 0 else 0

    print(f"Meilleur modele CV trouve -> RMSECV: {rmsecv:.4f}")

    # 3. CRASH TEST SUR JEU EXTERNE
    X_ext = df_test_externe[col_spectres].values
    y_ext_true = df_test_externe[compose].values
    
    if len(etapes_pretraitement) > 0:
        panier_ext = {}
        class InterceptorPredict(BaseEstimator, RegressorMixin):
            def fit(self, X_t, y_t, **kwargs):
                panier_ext['X_transforme'] = X_t
                return self
            def predict(self, X_t): return np.zeros(len(X_t))
                
        pipeline_ext = etapes_pretraitement + [{"model": InterceptorPredict()}]
        try: nirs4all.run(dataset=(X_ext, y_ext_true), pipeline=pipeline_ext)
        except: pass 
        X_ext_transforme = panier_ext.get('X_transforme', X_ext)
    else:
        X_ext_transforme = X_ext

    pred_ext = grid_search.predict(X_ext_transforme)
    _, _, _, rmsep_ext, rpd_ext = calculer_metriques(y_ext_true, pred_ext, y_ext_true, pred_ext)
    r2p_ext = r2_score(y_ext_true, pred_ext)

    print(f"Resultats Externe -> R2p: {r2p_ext:.4f} | RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")

    bilan_validation.append({
        "Iteration": iteration,
        "Seed": SEED_ITER,
        "Pretraitement": chaine_r_brute,
        "Hyperparametres": meilleurs_params,
        "R2cv": round(r2cv, 4),
        "RMSECV": round(rmsecv, 4),
        "R2p_Externe": round(r2p_ext, 4),
        "RMSEP_Externe": round(rmsep_ext, 4),
        "RPD_Externe": round(rpd_ext, 4)
    })

    # 4. EXPORT DES DETAILS
    df_predictions_ext = pd.DataFrame({
        "Echantillon": df_test_externe['ech'].values if 'ech' in df_test_externe.columns else range(1, len(y_ext_true) + 1),
        "Vraie_Valeur": y_ext_true,
        "Valeur_Predite": pred_ext,
        "SEP": np.abs(y_ext_true - pred_ext)
    })
    df_predictions_ext.to_csv(dossier_iter / f"PREDICTIONS_EXTERNES_{compose}.csv", sep=";", index=False)
    
    df_params_testes = pd.DataFrame(grid_search.cv_results_)
    df_params_testes.to_csv(dossier_iter / f"GRIDSEARCH_DETAILS_{compose}.csv", sep=";", index=False)
    joblib.dump(grid_search, dossier_iter / f"modele_XGB_FINAL_{compose}.joblib")

    model_clone = clone(grid_search.best_estimator_)
    for fold_idx, (train_idx, val_idx) in enumerate(kf.split(X_transforme, y)):
        X_train_f, X_val_f = X_transforme[train_idx], X_transforme[val_idx]
        y_train_f, y_val_f = y[train_idx], y[val_idx]
        model_clone.fit(X_train_f, y_train_f)
        preds_val_f = model_clone.predict(X_val_f)
        df_fold = pd.DataFrame({
            "Valeur_Mesuree": y_val_f,
            "Valeur_Predite": preds_val_f,
            "SEP": np.abs(y_val_f - preds_val_f)
        })
        df_fold.to_csv(dossier_iter / f"DETAILS_CV_FOLD_{fold_idx+1}_{compose}.csv", sep=";", index=False)

    # 5. GRAPHIQUES DE L'ITERATION
    chemin_rapport_pdf = dossier_iter / f"RAPPORT_GRAPHIQUES_{compose}.pdf"
    
    with PdfPages(chemin_rapport_pdf) as pdf:
        
        # Graphe 1 : Robustesse GridSearch
        try:
            with plt.style.context('ggplot'):
                fig_rob, ax_rob = plt.subplots(figsize=(8, 6))
                rmse_cv_grid = np.sqrt(np.abs(grid_search.cv_results_['mean_test_score']))
                rmse_c_grid = np.sqrt(np.abs(grid_search.cv_results_['mean_train_score']))
                
                ax_rob.scatter(rmse_cv_grid, rmse_c_grid, facecolors='none', edgecolors='black', s=40, alpha=0.5, label="Combinaisons Hyperparam")
                
                champion_idx = grid_search.best_index_
                ax_rob.scatter(rmse_cv_grid[champion_idx], rmse_c_grid[champion_idx], color='none', s=80, edgecolor="red", linewidth=2.0, label="Meilleur modele")
                
                lims = [min(rmse_cv_grid.min(), rmse_c_grid.min()) * 0.9, max(rmse_cv_grid.max(), rmse_c_grid.max()) * 1.1]
                ax_rob.plot(lims, lims, color="red", linestyle="--", linewidth=1.5, label="y = x")
                
                ax_rob.set_title(f"Overfitting Hyperparametres XGB (Iter {iteration})", fontsize=13)
                ax_rob.set_xlabel("RMSEcv")
                ax_rob.set_ylabel("RMSEC")
                ax_rob.legend()
                ax_rob.set_xlim(lims)
                ax_rob.set_ylim(lims)
                pdf.savefig(fig_rob, bbox_inches="tight")
                plt.close(fig_rob)
        except Exception as e_g1:
            print(f"Erreur graph 1 : {e_g1}")

        # Graphe 2 : Pred vs Mesure
        try:
            with plt.style.context('ggplot'):
                fig_pub, ax_pub = plt.subplots(figsize=(8, 6))
                ax_pub.scatter(pred_ext, y_ext_true, facecolors='none', edgecolors='black', s=35, alpha=0.7)
                z = np.polyfit(pred_ext, y_ext_true, 1)
                p = np.poly1d(z)
                x_line = np.linspace(pred_ext.min(), pred_ext.max(), 100)
                ax_pub.plot(x_line, p(x_line), color='blue', linestyle='-', linewidth=1.5)
                
                limites = [min(pred_ext.min(), y_ext_true.min())*0.95, max(pred_ext.max(), y_ext_true.max())*1.05]
                ax_pub.plot(limites, limites, color="red", linestyle="--", linewidth=1.5)
                
                texte = f"R2p = {r2p_ext:.3f}\nRMSEp = {rmsep_ext:.3f}\nRPD = {rpd_ext:.3f}\nn_ech = {len(y_ext_true)}"
                ax_pub.text(0.02, 0.96, texte, transform=ax_pub.transAxes, fontsize=12, verticalalignment='top')
                ax_pub.set_title(f"Predictions vs Mesures (Iter {iteration})", fontsize=13)
                ax_pub.set_xlabel("Valeurs predites")
                ax_pub.set_ylabel("Valeurs mesurees")
                pdf.savefig(fig_pub, bbox_inches="tight")
                plt.close(fig_pub)
        except Exception as e_g2:
            print(f"Erreur graph 2 : {e_g2}")

        # Graphe 3 : Feature Importance XGBoost
        try:
            with plt.style.context('default'):
                fig_stem, ax_stem = plt.subplots(figsize=(8, 5))
                toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
                x_values = toutes_longueurs.copy()

                match_red = re.search(r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", chaine_r_brute)
                if match_red:
                    drop_start, drop_end, step = int(match_red.group(1)), int(match_red.group(2)), int(match_red.group(3))
                    x_values = x_values[drop_start : len(x_values)-drop_end : step]
                    
                match_sder = re.search(r"list\('sder',\s*c\(\s*\d+\s*,\s*\d+\s*,\s*(\d+)\s*\)", chaine_r_brute)
                if match_sder:
                    points_lost = (int(match_sder.group(1)) - 1) // 2
                    if points_lost > 0: x_values = x_values[points_lost : -points_lost]

                importances = grid_search.best_estimator_.feature_importances_
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

# %% SAUVEGARDE DU BILAN GLOBAL ET BOXPLOTS FINAUX
print("\n" + "="*60)
print("FIN DES ITERATIONS - SAUVEGARDE DU BILAN DE VALIDATION XGBoost")
print("="*60)

df_bilan = pd.DataFrame(bilan_validation)
chemin_bilan = dossier_base_test / f"BILAN_FINAL_VALIDATION_{compose}.csv"
df_bilan.to_csv(chemin_bilan, sep=";", index=False)

print(f"Bilan enregistre : {chemin_bilan.name}")

# --- GENERATION DU BOXPLOT FINAL DU COMPOSE (STYLE GGPLOT) ---
try:
    chemin_boxplot = dossier_base_test / f"RAPPORT_FINAL_BOXPLOTS_{compose}.pdf"
    
    with PdfPages(chemin_boxplot) as pdf:
        # Couleur sarcelle
        couleur_boite = '#21908C' 
        
        # --- PAGE 1 : R2p ---
        fig_r2, ax_r2 = plt.subplots(figsize=(8, 6))
        ax_r2.set_facecolor('white')
        ax_r2.grid(True, color='#EBEBEB', linestyle='-', linewidth=0.8, zorder=0)
        for spine in ax_r2.spines.values(): spine.set_color('#CCCCCC')
            
        bp1 = ax_r2.boxplot([df_bilan["R2p_Externe"]], patch_artist=True, zorder=2, widths=0.3, showfliers=False, showcaps=False)
        for box in bp1['boxes']: box.set(facecolor=couleur_boite, edgecolor='black', linewidth=1)
        for median in bp1['medians']: median.set(color='black', linewidth=1.5)
        for whisker in bp1['whiskers']: whisker.set(color='black', linewidth=1)
            
        x_jitter_r2 = np.random.normal(1, 0.03, size=len(df_bilan["R2p_Externe"]))
        ax_r2.scatter(x_jitter_r2, df_bilan["R2p_Externe"], color='black', s=8, alpha=0.8, zorder=3)
        
        ax_r2.set_title(f"Boxplot des R2p (XGB) pour {compose}\nsur les {NB_ITERATIONS} iterations", fontsize=12, fontweight='bold', pad=15)
        ax_r2.set_ylabel("R2p", fontsize=12)
        ax_r2.set_xticks([1])
        ax_r2.set_xticklabels([compose], fontsize=11)
        
        plt.tight_layout()
        pdf.savefig(fig_r2, bbox_inches="tight")
        plt.close(fig_r2)

        # --- PAGE 2 : RMSEP ---
        fig_rmse, ax_rmse = plt.subplots(figsize=(8, 6))
        ax_rmse.set_facecolor('white')
        ax_rmse.grid(True, color='#EBEBEB', linestyle='-', linewidth=0.8, zorder=0)
        for spine in ax_rmse.spines.values(): spine.set_color('#CCCCCC')
            
        bp2 = ax_rmse.boxplot([df_bilan["RMSEP_Externe"]], patch_artist=True, zorder=2, widths=0.3, showfliers=False, showcaps=False)
        for box in bp2['boxes']: box.set(facecolor=couleur_boite, edgecolor='black', linewidth=1)
        for median in bp2['medians']: median.set(color='black', linewidth=1.5)
        for whisker in bp2['whiskers']: whisker.set(color='black', linewidth=1)
            
        x_jitter_rmse = np.random.normal(1, 0.03, size=len(df_bilan["RMSEP_Externe"]))
        ax_rmse.scatter(x_jitter_rmse, df_bilan["RMSEP_Externe"], color='black', s=8, alpha=0.8, zorder=3)
        
        ax_rmse.set_title(f"Boxplot des RMSEP (XGB) pour {compose}\nsur les {NB_ITERATIONS} iterations", fontsize=12, fontweight='bold', pad=15)
        ax_rmse.set_ylabel("RMSEP", fontsize=12)
        ax_rmse.set_xticks([1])
        ax_rmse.set_xticklabels([compose], fontsize=11)
        
        plt.tight_layout()
        pdf.savefig(fig_rmse, bbox_inches="tight")
        plt.close(fig_rmse)
        
    print(f"Rapport graphique final (Boxplots) enregistre : {chemin_boxplot.name}")
except Exception as e_box:
    print(f"Erreur lors de la generation du boxplot final : {e_box}")