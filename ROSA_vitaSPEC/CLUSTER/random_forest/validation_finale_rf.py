# -*- coding: utf-8 -*-

# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import joblib
import json
import numpy as np
import random
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV, KFold, train_test_split
from sklearn.base import clone
from sklearn.metrics import r2_score

## pathing
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

## fonctions
from diy_functions.pre_translation import pre_translation
from diy_functions.metrics import calculer_metriques

# %% CHARGEMENT DONNEES ET LECTURE DU GAGNANT

if len(sys.argv) < 4:
    print("error : il manque des arguments")
    sys.exit(1)

compose = sys.argv[1]
fichier_data = sys.argv[2]
idparam = sys.argv[3]

DATA = d0 / "commun" / fichier_data
dossier_base_test = d0 / "random_forest" / "moyennes" / "test_final" / idparam / compose

# 1. Lecture du pretraitement gagnant dans l'Excel
chemin_excel_synthese = dossier_base_test / f"SYNTHESE_PRETRAITEMENTS_{compose}.xlsx"

try:
    df_synthese = pd.read_excel(chemin_excel_synthese)
    # Le gagnant est sur la premiere ligne car le script precedent a trie le tableau
    chaine_r_brute = df_synthese.iloc[0]["Pretraitement_Gagnant"]
    print(f"Pretraitement fige selectionne : {chaine_r_brute}")
except Exception as e:
    print(f"Erreur : Impossible de lire le fichier {chemin_excel_synthese.name}.")
    print(f"Detail de l'erreur : {e}")
    sys.exit(1)

## Data
df_data = pd.read_csv(DATA)
# nettoyage
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

# %% CONFIG MOD

## GridSearchCV (Exploration exhaustive de la grille)
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [None, 10, 20],
    "min_samples_split": [2, 5, 10],
    "min_samples_leaf": [1, 2, 4],
    "max_features": ["sqrt", "log2", 0.3],
}

print(f"\nDebut de la Validation Finale (5 iterations) pour: {compose}")

# Nettoyage et isolation du compose
df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
df_propre = df_data.dropna(subset=[compose])

# Trackers globaux pour le resume final
historique_seeds = []
bilan_validation = []

NB_ITERATIONS = 10

# %% BOUCLE DE VALIDATION FINALE
for iteration in range(1, NB_ITERATIONS + 1):
    
    SEED_ITER = random.randint(1, 999999)
    np.random.seed(SEED_ITER)
    random.seed(SEED_ITER)
    
    historique_seeds.append({"Iteration": iteration, "Seed": SEED_ITER, "Compose": compose})
    
    print(f"\n{'='*60}")
    print(f" ITERATION {iteration}/{NB_ITERATIONS} - SEED: {SEED_ITER}")
    print(f"{'='*60}")

    kf = KFold(n_splits=5, shuffle=True, random_state=SEED_ITER)

    # Separation 100% independante et aleatoire
    df_train_val, df_test_externe = train_test_split(df_propre, test_size=30, random_state=SEED_ITER)

    # Sauvegarde du jeu de test dans le sous-dossier specifique a cette iteration
    dossier_iter = dossier_base_test / f"iter_{iteration}"
    dossier_iter.mkdir(parents=True, exist_ok=True)
    
    chemin_test_externe = dossier_iter / f"valid_externe_{compose}.csv"
    df_test_externe.to_csv(chemin_test_externe, index=False)

    # matrices x et y
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
            def predict(self, X_t):
                return np.zeros(len(X_t))
        
        pipeline_intercept = etapes_pretraitement + [{"model": InterceptorRegressor()}]
        
        try:
            nirs4all.run(dataset=(X, y), pipeline=pipeline_intercept)
        except Exception as e_run:
            # On ignore les crashs de la base de donnees nirs4all
            # car la matrice transformee est deja dans notre panier
            pass
            
        X_transforme = panier_donnees.get('X_transforme', X)
    else:
        X_transforme = X

    # 2. OPTIMISATION EXHAUSTIVE DES HYPERPARAMETRES (GridSearch)
    print("Recherche GridSearchCV en cours (cela peut prendre quelques minutes)...")
    grid_search = GridSearchCV(
        estimator=RandomForestRegressor(random_state=SEED_ITER, n_jobs=-1),
        param_grid=param_grid,
        cv=kf,
        scoring="neg_mean_squared_error",
        return_train_score=True # Crucial pour le graphique de robustesse
    )
    
    grid_search.fit(X_transforme, y)
    
    # 3. EXTRACTION DES RESULTATS
    meilleur_modele_joblib = grid_search 
    meilleurs_params = str(grid_search.best_params_)
    score_neg_mse = grid_search.best_score_
    rmsecv = float(np.sqrt(abs(score_neg_mse)))

    pred_train = grid_search.predict(X_transforme)
    rc, _, rmsec, _, _ = calculer_metriques(y, pred_train, y, pred_train)
    
    variance_y = np.var(y)
    r2cv = 1 - (abs(score_neg_mse) / variance_y) if variance_y != 0 else 0

    print(f"Meilleur modele CV trouve -> RMSECV: {rmsecv:.4f}")

    # 4. CRASH TEST SUR LE JEU EXTERNE
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

    pred_ext = meilleur_modele_joblib.predict(X_ext_transforme)
    _, _, _, rmsep_ext, rpd_ext = calculer_metriques(y_ext_true, pred_ext, y_ext_true, pred_ext)
    r2p_ext = r2_score(y_ext_true, pred_ext)

    print(f"Resultats Externe -> R2p: {r2p_ext:.4f} | RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")

    # Collecte pour le bilan final
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

    # 5. SAUVEGARDE DES DETAILS POUR L'ITERATION
    df_predictions_ext = pd.DataFrame({
        "Echantillon": df_test_externe['ech'].values if 'ech' in df_test_externe.columns else range(1, len(y_ext_true) + 1),
        "Vraie_Valeur": y_ext_true,
        "Valeur_Predite": pred_ext,
        "SEP": np.abs(y_ext_true - pred_ext)
    })
    
    # Calcul des incertitudes par arbre
    if hasattr(meilleur_modele_joblib.best_estimator_, "estimators_"):
        predictions_arbres = []
        for arbre in meilleur_modele_joblib.best_estimator_.estimators_:
            predictions_arbres.append(arbre.predict(X_ext_transforme))
        predictions_arbres = np.array(predictions_arbres)
        ecart_type_pred = np.std(predictions_arbres, axis=0)
        df_predictions_ext["Ecart_Type_Incertitude"] = ecart_type_pred
        df_predictions_ext["IC_Bas_95%"] = pred_ext - (1.96 * ecart_type_pred)
        df_predictions_ext["IC_Haut_95%"] = pred_ext + (1.96 * ecart_type_pred)

    chemin_csv_pred_ext = dossier_iter / f"PREDICTIONS_EXTERNES_{compose}.csv"
    df_predictions_ext.to_csv(chemin_csv_pred_ext, sep=";", index=False)

    df_params_testes = pd.DataFrame(grid_search.cv_results_)
    df_params_testes.to_csv(dossier_iter / f"GRIDSEARCH_DETAILS_{compose}.csv", sep=";", index=False)

    # Reproduction des predictions de la CV par fold
    model_clone = clone(meilleur_modele_joblib.best_estimator_)
    for fold_idx, (train_idx, val_idx) in enumerate(kf.split(X_transforme, y)):
        X_train_f, X_val_f = X_transforme[train_idx], X_transforme[val_idx]
        y_train_f, y_val_f = y[train_idx], y[val_idx]
        model_clone.fit(X_train_f, y_train_f)
        preds_val_f = model_clone.predict(X_val_f)
        
        df_fold = pd.DataFrame({
            "Numero_Echantillon": val_idx + 1,
            "Identifiant_Ech": df_train_val.iloc[val_idx]['ech'].values if 'ech' in df_train_val.columns else val_idx,
            "Valeur_Mesuree": y_val_f,
            "Valeur_Predite": preds_val_f,
            "Ecart_Mesure": y_val_f - preds_val_f,
            "SEP": np.abs(y_val_f - preds_val_f)
        })
        df_fold.to_csv(dossier_iter / f"DETAILS_CV_FOLD_{fold_idx+1}_{compose}.csv", sep=";", index=False)

    chemin_modele = dossier_iter / f"modele_RF_FINAL_{compose}.joblib"
    joblib.dump(meilleur_modele_joblib, chemin_modele)

    # 6. GRAPHIQUES POUR L'ITERATION
    chemin_rapport_pdf = dossier_iter / f"RAPPORT_GRAPHIQUES_{compose}.pdf"
    figures_generees = []

    with PdfPages(chemin_rapport_pdf) as pdf:
        
        # Graph 1 : Robustesse des Hyperparametres (Nouveau concept)
        try:
            with plt.style.context('ggplot'):
                fig_robustesse, ax_rob = plt.subplots(figsize=(8, 6))
                
                # Extraction des scores train et test depuis le GridSearch
                rmse_cv_grid = np.sqrt(np.abs(grid_search.cv_results_['mean_test_score']))
                rmse_c_grid = np.sqrt(np.abs(grid_search.cv_results_['mean_train_score']))
                
                ax_rob.scatter(rmse_cv_grid, rmse_c_grid, facecolors='none', edgecolors='black', s=40, alpha=0.5, label="Combinaisons Hyperparam", zorder=3)
                
                champion_idx = grid_search.best_index_
                ax_rob.scatter(rmse_cv_grid[champion_idx], rmse_c_grid[champion_idx], color='none', s=80, edgecolor="red", linewidth=2.0, label="Meilleur modele", zorder=5)
                
                min_val = min(rmse_cv_grid.min(), rmse_c_grid.min())
                max_val = max(rmse_cv_grid.max(), rmse_c_grid.max())
                lims = [min_val * 0.9, max_val * 1.1]
                ax_rob.plot(lims, lims, color="red", linestyle="--", linewidth=1.5, label="y = x", zorder=2)
                
                ax_rob.set_title(f"Overfitting Hyperparametres (Iter {iteration})", fontsize=15, loc='left', pad=15, color='black')
                ax_rob.set_xlabel("RMSEcv", fontsize=13, color='black')
                ax_rob.set_ylabel("RMSEC", fontsize=13, color='black')
                ax_rob.tick_params(colors='black')
                legend = ax_rob.legend(frameon=True, facecolor='white', edgecolor='black', fontsize=11)
                for text in legend.get_texts(): text.set_color("black")
                ax_rob.set_xlim(lims)
                ax_rob.set_ylim(lims)
                
                pdf.savefig(fig_robustesse, bbox_inches="tight")  
                figures_generees.append(fig_robustesse)
        except Exception as e_g1:
            print(f"Erreur graph 1 : {e_g1}")

        # Graph 2 : Pred vs Mesure
        try:
            with plt.style.context('ggplot'):
                fig_publi, ax_pub = plt.subplots(figsize=(8, 6))
                x_pred = df_predictions_ext["Valeur_Predite"].values
                y_vrai = df_predictions_ext["Vraie_Valeur"].values
                ax_pub.scatter(x_pred, y_vrai, facecolors='none', edgecolors='black', s=35, alpha=0.7, zorder=3)
                z = np.polyfit(x_pred, y_vrai, 1)
                p = np.poly1d(z)
                x_line = np.linspace(x_pred.min(), x_pred.max(), 100)
                ax_pub.plot(x_line, p(x_line), color='blue', linestyle='-', linewidth=1.5, zorder=2)
                
                min_val = min(x_pred.min(), y_vrai.min())
                max_val = max(x_pred.max(), y_vrai.max())
                marge = (max_val - min_val) * 0.05
                limites = [min_val - marge, max_val + marge]
                ax_pub.plot(limites, limites, color="red", linestyle="--", linewidth=1.5, zorder=1)
                
                texte_metriques = (f"R2p = {r2p_ext:.3f}\nRMSEp = {rmsep_ext:.3f}\nRPD = {rpd_ext:.3f}\nn_ech = {len(y_vrai)}")
                ax_pub.text(0.02, 0.96, texte_metriques, transform=ax_pub.transAxes, fontsize=13, verticalalignment='top', horizontalalignment='left', color='black')
                ax_pub.set_title(f"Predictions vs Mesures (Iter {iteration})", fontsize=15, loc='left', pad=15, color='black')
                ax_pub.set_xlabel("Valeurs predites", fontsize=13, color='black')
                ax_pub.set_ylabel("Valeurs mesurees", fontsize=13, color='black')
                ax_pub.tick_params(colors='black')
                
                pdf.savefig(fig_publi, bbox_inches="tight") 
                figures_generees.append(fig_publi)
        except Exception as e_papier:
            print(f"Erreur graph 2 : {e_papier}")

        # Graph 3 : Feature importance
        try:
            with plt.style.context('default'):
                fig_stem, ax_stem = plt.subplots(figsize=(8, 5))
                plt.rcParams['font.family'] = 'serif'
                
                toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
                x_values = toutes_longueurs.copy()

                match_reduction = re.search(r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", chaine_r_brute)
                if match_reduction:
                    drop_start, drop_end, step = int(match_reduction.group(1)), int(match_reduction.group(2)), int(match_reduction.group(3))
                    end_idx = len(x_values) - drop_end
                    x_values = x_values[drop_start:end_idx:step]
                    
                match_sder = re.search(r"list\('sder',\s*c\(\s*\d+\s*,\s*\d+\s*,\s*(\d+)\s*\)", chaine_r_brute)
                if match_sder:
                    window_size = int(match_sder.group(1))
                    points_lost = (window_size - 1) // 2
                    if points_lost > 0: x_values = x_values[points_lost : -points_lost]

                fitted_mod = meilleur_modele_joblib.best_estimator_
                importances = fitted_mod.feature_importances_

                if len(x_values) != len(importances):
                    x_values = np.linspace(min(x_values), max(x_values), len(importances))

                ax_stem.vlines(x=x_values, ymin=0, ymax=importances, color="#4169E1", linewidth=1.5, alpha=0.8)
                ax_stem.plot(x_values, importances, marker='o', markersize=2, color="#4169E1", linestyle='None')
                ax_stem.axhline(y=0, color='gray', linewidth=0.8, linestyle='-')
                
                ax_stem.set_title(f"RF Importance (Iter {iteration})", fontsize=16, fontweight='bold', pad=15, fontfamily='serif')
                ax_stem.set_ylabel("Importance", fontsize=14, fontfamily='serif')
                ax_stem.set_xlabel("Longueurs d'ondes (nm)", fontsize=12, fontfamily='serif')
                
                for spine in ax_stem.spines.values():
                    spine.set_visible(True)
                    spine.set_color('black')
                    spine.set_linewidth(1)
                
                ax_stem.tick_params(direction='in', length=5, width=1, colors='black', grid_alpha=0)
                marge_x = (max(x_values) - min(x_values)) * 0.05
                ax_stem.set_xlim(min(x_values) - marge_x, max(x_values) + marge_x)
                ax_stem.set_ylim(0, max(importances) * 1.1)

                pdf.savefig(fig_stem, bbox_inches="tight") 
                figures_generees.append(fig_stem)
        except Exception as e_graph:
            print(f"Erreur graph 3 : {e_graph}")

    plt.close('all')

# %% SAUVEGARDE DU BILAN GLOBAL
print("\n" + "="*60)
print("FIN DES ITERATIONS - SAUVEGARDE DU BILAN DE VALIDATION")
print("="*60)

df_bilan = pd.DataFrame(bilan_validation)
chemin_bilan = dossier_base_test / f"BILAN_FINAL_VALIDATION_{compose}.csv"
df_bilan.to_csv(chemin_bilan, sep=";", index=False)

print(f"Bilan enregistre : {chemin_bilan.name}")

# --- AJOUT : GENERATION DU BOXPLOT FINAL DU COMPOSE (STYLE GGPLOT) ---
try:
    chemin_boxplot = dossier_base_test / f"RAPPORT_FINAL_BOXPLOTS_{compose}.pdf"
    
    with PdfPages(chemin_boxplot) as pdf:
        # Couleur sarcelle (teal) similaire a votre image
        couleur_boite = '#21908C' 
        
        # ---------------------------------------------------------
        # PAGE 1 : R2p
        # ---------------------------------------------------------
        fig_r2, ax_r2 = plt.subplots(figsize=(8, 6))
        
        # Style du fond et de la grille (calque sur l'image)
        ax_r2.set_facecolor('white')
        ax_r2.grid(True, color='#EBEBEB', linestyle='-', linewidth=0.8, zorder=0)
        for spine in ax_r2.spines.values():
            spine.set_color('#CCCCCC') # Bordure grise claire
            
        # Boxplot sans fliers (car on les dessine apres) et sans caps horizontales
        bp1 = ax_r2.boxplot([df_bilan["R2p_Externe"]], patch_artist=True, zorder=2,
                            widths=0.3, showfliers=False, showcaps=False)
        
        # Personnalisation des couleurs de la boite et des traits
        for box in bp1['boxes']:
            box.set(facecolor=couleur_boite, edgecolor='black', linewidth=1)
        for median in bp1['medians']:
            median.set(color='black', linewidth=1.5)
        for whisker in bp1['whiskers']:
            whisker.set(color='black', linewidth=1)
            
        # Ajout des points individuels (Jitter)
        x_jitter_r2 = np.random.normal(1, 0.03, size=len(df_bilan["R2p_Externe"]))
        ax_r2.scatter(x_jitter_r2, df_bilan["R2p_Externe"], color='black', s=8, alpha=0.8, zorder=3)
        
        # Textes
        ax_r2.set_title(f"Boxplot des R2p obtenus pour {compose}\nsur les {NB_ITERATIONS} iterations", fontsize=12, fontweight='bold', pad=15)
        ax_r2.set_ylabel("R2p", fontsize=12)
        ax_r2.set_xticks([1])
        ax_r2.set_xticklabels([compose], fontsize=11)
        
        plt.tight_layout()
        pdf.savefig(fig_r2, bbox_inches="tight")
        plt.close(fig_r2)

        # ---------------------------------------------------------
        # PAGE 2 : RMSEP
        # ---------------------------------------------------------
        fig_rmse, ax_rmse = plt.subplots(figsize=(8, 6))
        
        # Style du fond et de la grille
        ax_rmse.set_facecolor('white')
        ax_rmse.grid(True, color='#EBEBEB', linestyle='-', linewidth=0.8, zorder=0)
        for spine in ax_rmse.spines.values():
            spine.set_color('#CCCCCC')
            
        # Boxplot
        bp2 = ax_rmse.boxplot([df_bilan["RMSEP_Externe"]], patch_artist=True, zorder=2,
                              widths=0.3, showfliers=False, showcaps=False)
        
        # Personnalisation
        for box in bp2['boxes']:
            box.set(facecolor=couleur_boite, edgecolor='black', linewidth=1)
        for median in bp2['medians']:
            median.set(color='black', linewidth=1.5)
        for whisker in bp2['whiskers']:
            whisker.set(color='black', linewidth=1)
            
        # Ajout des points individuels (Jitter)
        x_jitter_rmse = np.random.normal(1, 0.03, size=len(df_bilan["RMSEP_Externe"]))
        ax_rmse.scatter(x_jitter_rmse, df_bilan["RMSEP_Externe"], color='black', s=8, alpha=0.8, zorder=3)
        
        # Textes
        ax_rmse.set_title(f"Boxplot des RMSEP obtenus pour {compose}\nsur les {NB_ITERATIONS} iterations", fontsize=12, fontweight='bold', pad=15)
        ax_rmse.set_ylabel("RMSEP", fontsize=12)
        ax_rmse.set_xticks([1])
        ax_rmse.set_xticklabels([compose], fontsize=11)
        
        plt.tight_layout()
        pdf.savefig(fig_rmse, bbox_inches="tight")
        plt.close(fig_rmse)
        
    print(f"Rapport graphique final (Boxplots) enregistre : {chemin_boxplot.name}")
except Exception as e_box:
    print(f"Erreur lors de la generation du boxplot final : {e_box}")