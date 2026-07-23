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
import seaborn as sns
from matplotlib.backends.backend_pdf import PdfPages

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RandomizedSearchCV, KFold, train_test_split
from sklearn.base import clone
from sklearn.metrics import r2_score

## pathing
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

## fonctions
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

## Lecture du fichier pretraitements (param)
list_pre_tot = d0 / "commun" / "diy_functions" / "list_pre_test_tot.R"
with open(list_pre_tot, "r", encoding="utf-8") as f:
    contenu_r = f.read()

# extrait chaque ligne rbind(...)
liste_pretraitements_r = re.findall(r"rbind\((.*)\)", contenu_r)

## Data
df_data = pd.read_csv(DATA)
# nettoyage
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

print(f"{len(df_data)} echantillons au total")
print(f"{len(col_spectres)} longueurs d'ondes")
print(f"{len(liste_pretraitements_r)} pretraitements a tester par iteration")

# %% CONFIG MOD

## RandomizedSearchCV
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [None, 10, 20],
    "min_samples_split": [2, 5, 10],
    "min_samples_leaf": [1, 2, 4],
    "max_features": ["sqrt", "log2", 0.3],
}

print(f"\nDebut de la boucle Monte-Carlo (10 iterations) pour: {compose}")

# Nettoyage et isolation du compose
df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
df_propre = df_data.dropna(subset=[compose])
print(f"Echantillons valides pour ce compose : {len(df_propre)} / {len(df_data)}\n")

# Trackers globaux pour le resume final
historique_seeds = []
bilan_monte_carlo = []

NB_ITERATIONS = 10

# %% BOUCLE MONTE-CARLO
for iteration in range(1, NB_ITERATIONS + 1):
    
    # 1. Tirage du SEED aleatoire pour cette iteration
    SEED_ITER = random.randint(1, 999999)
    np.random.seed(SEED_ITER)
    random.seed(SEED_ITER)
    
    historique_seeds.append({"Iteration": iteration, "Seed": SEED_ITER, "Compose": compose})
    
    print(f"\n{'='*60}")
    print(f" ITERATION {iteration}/{NB_ITERATIONS} - SEED: {SEED_ITER}")
    print(f"{'='*60}")

    kf = KFold(n_splits=5, shuffle=True, random_state=SEED_ITER)

    # 2. Separation 100% independante et aleatoire
    df_train_val, df_test_externe = train_test_split(df_propre, test_size=30, random_state=SEED_ITER)

    # Sauvegarde du jeu de test dans le sous-dossier specifique a cette iteration
    dossier_test_iter = d0 / "random_forest" / "jeux_test" / idparam / compose / f"iter_{iteration}"
    dossier_test_iter.mkdir(parents=True, exist_ok=True)
    
    chemin_test_externe = dossier_test_iter / f"valid_externe_{compose}.csv"
    df_test_externe.to_csv(chemin_test_externe, index=False)

    # matrices x et y
    y = df_train_val[compose].values.astype(float) + np.random.normal(0, 1e-5, size=len(df_train_val))
    X = df_train_val[col_spectres].values
    X[X <= 0] = 1e-5

    # Trackers locaux pour cette iteration
    meilleur_rmsecv_global = float("inf")
    meilleur_modele_joblib = None
    meilleur_pipeline_pre = []
    rapport_du_champion = None
    tableau_compose = []

    # 3. BOUCLE SUR LES PReTRAITEMENTS
    for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
        try:
            etapes_pretraitement = pre_translation(chaine_r_brute)
            
            ## pretraitements via nirs4all
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
                    if "0 feature" in str(e_run):
                        raise ValueError("le pretraitement a supprime tout le spectre")
                    pass 
                    
                if 'X_transforme' in panier_donnees:
                    X_transforme = panier_donnees['X_transforme']
                else:
                    raise ValueError("nirs4all a echoue avant de generer la matrice transformee")
                    
            else:
                X_transforme = X
                
            if X_transforme.shape[1] == 0:
                raise ValueError(f"Le pretraitement a supprime toutes les variables du spectre.")

            ## train avec scikit learn
            random_search = RandomizedSearchCV(
                estimator=RandomForestRegressor(random_state=SEED_ITER, n_jobs=-1),
                param_distributions=param_grid,
                n_iter=30,
                cv=kf,
                scoring="neg_mean_squared_error",
                random_state=SEED_ITER,
            )
            
            random_search.fit(X_transforme, y)
            
            ## extraction + metriques
            meilleurs_params = str(random_search.best_params_)
            score_neg_mse = random_search.best_score_
            rmsecv = float(np.sqrt(abs(score_neg_mse)))

            pred_train = random_search.predict(X_transforme)
            rc, _, rmsec, _, _ = calculer_metriques(y, pred_train, y, pred_train)
            
            variance_y = np.var(y)
            r2cv = 1 - (abs(score_neg_mse) / variance_y) if variance_y != 0 else 0

            ligne_resultat = {
                "Compose": compose,
                "ID_Pretraitement": id_pre + 1,
                "Code_R_Pretraitement": chaine_r_brute,
                "Meilleurs_Hyperparam_RF": meilleurs_params,
                "Rc": round(rc, 4),
                "R2cv": round(r2cv, 4),
                "RMSEC": round(rmsec, 4),
                "RMSECV": round(rmsecv, 4),
            }
            tableau_compose.append(ligne_resultat)

            if id_pre == 0 and rmsecv == 0.0:
                print("\n securite")
                sys.exit(1)

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
                        "R2cv": round(r2cv, 4),
                        "RMSEC": round(rmsec, 4),
                        "RMSECV": round(rmsecv, 4),
                    },
                }

        except Exception as e:
            print(f"error pretraitement {id_pre + 1} ({chaine_r_brute}) ignore {e}")
            continue

    # 4. SAVE + TEST POUR CETTE ITERATION (Sous-dossier iter_x)
    dossier_results_iter = d0 / "random_forest" / "moyennes" / "Results" / idparam / compose / f"iter_{iteration}"
    dossier_results_iter.mkdir(parents=True, exist_ok=True)

    df_compose = pd.DataFrame(tableau_compose)
    chemin_csv_compose = dossier_results_iter / f"RANDOMSEARCH_DETAILS_{compose}.csv"
    df_compose.to_csv(chemin_csv_compose, sep=";", index=False)

    df_predictions_ext = pd.DataFrame()

    if meilleur_modele_joblib is not None:
        print(f"\n meilleur mod RF Iteration {iteration} (RMSECV: {meilleur_rmsecv_global:.4f})")

        try:
            X_ext = df_test_externe[col_spectres].values
            y_ext_true = df_test_externe[compose].values
            
            if len(meilleur_pipeline_pre) > 0:
                panier_ext = {}
                class InterceptorPredict(BaseEstimator, RegressorMixin):
                    def fit(self, X_t, y_t, **kwargs):
                        panier_ext['X_transforme'] = X_t
                        return self
                    def predict(self, X_t): return np.zeros(len(X_t))
                        
                pipeline_ext = meilleur_pipeline_pre + [{"model": InterceptorPredict()}]
                
                try: nirs4all.run(dataset=(X_ext, y_ext_true), pipeline=pipeline_ext)
                except: pass 
                    
                X_ext_transforme = panier_ext.get('X_transforme', X_ext)
            else:
                X_ext_transforme = X_ext

            pred_ext = meilleur_modele_joblib.predict(X_ext_transforme)
            _, _, _, rmsep_ext, rpd_ext = calculer_metriques(y_ext_true, pred_ext, y_ext_true, pred_ext)
            r2p_ext = r2_score(y_ext_true, pred_ext)

            rapport_du_champion["Crash_Test_Externe"] = {
                "R2p_Externe": round(r2p_ext, 4),
                "RMSEP_Externe": round(rmsep_ext, 4),
                "RPD_Externe": round(rpd_ext, 4),
            }
            print(f"Resultats Externe -> R2p: {r2p_ext:.4f} | RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")

            # Collecte pour le bilan final
            bilan_monte_carlo.append({
                "Iteration": iteration,
                "Seed": SEED_ITER,
                "Pretraitement_Gagnant": rapport_du_champion["Pretraitement_Gagnant"],
                "R2cv": rapport_du_champion["Metriques_Internes"]["R2cv"],
                "RMSECV": rapport_du_champion["Metriques_Internes"]["RMSECV"],
                "R2p_Externe": round(r2p_ext, 4),
                "RMSEP_Externe": round(rmsep_ext, 4),
                "RPD_Externe": round(rpd_ext, 4)
            })

            df_predictions_ext = pd.DataFrame({
                "Echantillon": df_test_externe['ech'].values if 'ech' in df_test_externe.columns else range(1, len(y_ext_true) + 1),
                "Vraie_Valeur": y_ext_true,
                "Valeur_Predite": pred_ext,
                "SEP": np.abs(y_ext_true - pred_ext)
            })
            
            try:
                if hasattr(meilleur_modele_joblib.best_estimator_, "estimators_"):
                    predictions_arbres = []
                    for arbre in meilleur_modele_joblib.best_estimator_.estimators_:
                        predictions_arbres.append(arbre.predict(X_ext_transforme))
                    predictions_arbres = np.array(predictions_arbres)
                    ecart_type_pred = np.std(predictions_arbres, axis=0)
                    df_predictions_ext["Ecart_Type_Incertitude"] = ecart_type_pred
                    df_predictions_ext["IC_Bas_95%"] = pred_ext - (1.96 * ecart_type_pred)
                    df_predictions_ext["IC_Haut_95%"] = pred_ext + (1.96 * ecart_type_pred)
            except Exception as e_ic:
                print(f"pas de calcul d'incertitude par arbre ({e_ic})")

            chemin_csv_pred_ext = dossier_results_iter / f"PREDICTIONS_EXTERNES_{compose}.csv"
            df_predictions_ext.to_csv(chemin_csv_pred_ext, sep=";", index=False)

        except Exception as e_test:
            print(f"error test : {e_test}")

        df_params_testes = pd.DataFrame(meilleur_modele_joblib.cv_results_['params'])
        df_params_testes.insert(0, "Iteration", range(1, len(df_params_testes) + 1))
        df_params_testes.to_csv(dossier_results_iter / f"PARAMETRES_TESTES_{compose}.csv", sep=";", index=False)

        if len(meilleur_pipeline_pre) > 0:
            panier_cv_folds = {}
            class InterceptorCV(BaseEstimator, RegressorMixin):
                def fit(self, X_t, y_t, **kwargs):
                    panier_cv_folds['X_transforme'] = X_t
                    return self
                def predict(self, X_t): return np.zeros(len(X_t))
            pipeline_cv = meilleur_pipeline_pre + [{"model": InterceptorCV()}]
            try: nirs4all.run(dataset=(X, y), pipeline=pipeline_cv)
            except: pass
            X_champ_transforme = panier_cv_folds.get('X_transforme', X)
        else:
            X_champ_transforme = X

        model_clone = clone(meilleur_modele_joblib.best_estimator_)
        for fold_idx, (train_idx, val_idx) in enumerate(kf.split(X_champ_transforme, y)):
            X_train_f, X_val_f = X_champ_transforme[train_idx], X_champ_transforme[val_idx]
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
            df_fold.to_csv(dossier_results_iter / f"DETAILS_CV_FOLD_{fold_idx+1}_{compose}.csv", sep=";", index=False)

        chemin_modele = dossier_results_iter / f"modele_RF_A_{compose}.joblib"
        joblib.dump(meilleur_modele_joblib, chemin_modele)

        with open(dossier_results_iter / f"rapport_A_{compose}.json", "w", encoding="utf-8") as f:
            json.dump(rapport_du_champion, f, indent=4)
    else:
        print(f"aucun mod train pour {compose} a l'iteration {iteration}.")

    # 5. GRAPHIQUES POUR CETTE ITERATION
    if meilleur_modele_joblib is not None:
        chemin_rapport_pdf = dossier_results_iter / f"RAPPORT_GRAPHIQUES_{compose}.pdf"
        chemin_rapport_png = dossier_results_iter / f"RAPPORT_GRAPHIQUES_{compose}.png"
        figures_generees = []

        with PdfPages(chemin_rapport_pdf) as pdf:
            # graph 1 robustesse
            try:
                with plt.style.context('ggplot'):
                    fig_robustesse, ax_rob = plt.subplots(figsize=(8, 6))
                    ax_rob.scatter(df_compose["RMSECV"], df_compose["RMSEC"], facecolors='none', edgecolors='black', s=40, alpha=0.7, label="pretraitements testes", zorder=3)
                    champion_row = df_compose.loc[df_compose["RMSECV"].idxmin()]
                    ax_rob.scatter(champion_row["RMSECV"], champion_row["RMSEC"], color='none', s=70, edgecolor="black", linewidth=1.5, label="meilleur pretraitement", zorder=5)
                    min_val = min(df_compose["RMSECV"].min(), df_compose["RMSEC"].min())
                    max_val = max(df_compose["RMSECV"].max(), df_compose["RMSEC"].max())
                    lims = [min_val * 0.9, max_val * 1.1]
                    ax_rob.plot(lims, lims, color="red", linestyle="--", linewidth=1.5, label="y = x", zorder=2)
                    ax_rob.set_title(f"Overfitting pretraitements : {compose} (Iter {iteration})", fontsize=15, loc='left', pad=15, color='black')
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
                print(f"error : {e_g1}")

            # graph 2 pred vs mesure
            if not df_predictions_ext.empty:
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
                        ax_pub.set_title(f"Predictions vs Mesures : {compose} (Iter {iteration})", fontsize=15, loc='left', pad=15, color='black')
                        ax_pub.set_xlabel("Valeurs predites", fontsize=13, color='black')
                        ax_pub.set_ylabel("Valeurs mesurees", fontsize=13, color='black')
                        ax_pub.tick_params(colors='black')
                        pdf.savefig(fig_publi, bbox_inches="tight") 
                        figures_generees.append(fig_publi)
                except Exception as e_papier:
                    print(f"error : {e_papier}")

            # graph 3 feature importance
            try:
                with plt.style.context('default'):
                    fig_stem, ax_stem = plt.subplots(figsize=(8, 5))
                    plt.rcParams['font.family'] = 'serif'
                    plt.rcParams['font.serif'] = ['Times New Roman'] + plt.rcParams['font.serif']

                    toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
                    pre_gagnant = rapport_du_champion["Pretraitement_Gagnant"]
                    x_values = toutes_longueurs.copy()

                    match_reduction = re.search(r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", pre_gagnant)
                    if match_reduction:
                        drop_start, drop_end, step = int(match_reduction.group(1)), int(match_reduction.group(2)), int(match_reduction.group(3))
                        end_idx = len(x_values) - drop_end
                        x_values = x_values[drop_start:end_idx:step]
                        
                    match_sder = re.search(r"list\('sder',\s*c\(\s*\d+\s*,\s*\d+\s*,\s*(\d+)\s*\)", pre_gagnant)
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
                    ax_stem.set_title(f"RF : {compose} (Iter {iteration})", fontsize=16, fontweight='bold', pad=15, fontfamily='serif')
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
                print(f"error : {e_graph}")

        # Save PNG
        if figures_generees:
            from matplotlib.backends.backend_agg import FigureCanvasAgg
            images = []
            for f in figures_generees:
                canvas = FigureCanvasAgg(f)
                canvas.draw()
                s, (width, height) = canvas.print_to_buffer()
                img = np.frombuffer(s, np.uint8).reshape((height, width, 4))
                images.append(img)
            
            largeur_max = max(img.shape[1] for img in images)
            hauteur_totale = sum(img.shape[0] for img in images)
            image_finale = np.ones((hauteur_totale, largeur_max, 4), dtype=np.uint8) * 255

            y_offset = 0
            for img in images:
                h, w, _ = img.shape
                x_offset = (largeur_max - w) // 2
                image_finale[y_offset:y_offset+h, x_offset:x_offset+w] = img
                y_offset += h

            try:
                from PIL import Image
                Image.fromarray(image_finale).save(chemin_rapport_png)
            except ImportError:
                print("error PNG generation")
                
        # Liberation stricte de la memoire graphique
        plt.close('all')

# %% SAUVEGARDE DES BILANS FINAUX (HORS BOUCLE)
print("\n" + "="*60)
print("FIN DES 10 ITERATIONS - SAUVEGARDE DES BILANS GLOBAUX")
print("="*60)

dossier_global = d0 / "random_forest" / "moyennes" / "Results" / idparam / compose

# 1. Sauvegarde de l'historique des Seeds
df_seeds = pd.DataFrame(historique_seeds)
chemin_seeds = dossier_global / f"SEEDS_UTILISES_{compose}.csv"
df_seeds.to_csv(chemin_seeds, sep=";", index=False)

# 2. Sauvegarde du grand bilan de Monte Carlo
df_bilan = pd.DataFrame(bilan_monte_carlo)
chemin_bilan = dossier_global / f"BILAN_MONTE_CARLO_{compose}.csv"
df_bilan.to_csv(chemin_bilan, sep=";", index=False)

print(f"Bilan enregistre dans le dossier global : {dossier_global.name}")