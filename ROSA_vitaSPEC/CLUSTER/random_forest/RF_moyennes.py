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
from sklearn.base import BaseEstimator, RegressorMixin

# Seed globale pour la reproductibilité absolue
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
# Nettoyage de sécurité
colonnes_a_retirer = ["campagne", "source", "etat"]
df_data = df_data.drop(columns=[col for col in colonnes_a_retirer if col in df_data.columns])

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

print(f"{len(df_data)} échantillons au total.")
print(f"{len(col_spectres)} longueurs d'ondes")
print(f"{len(liste_pretraitements_r)} prétraitements à tester")

# %% CONFIG MOD

## RandomizedSearchCV (Grille Random Forest)
param_grid = {
    "n_estimators": [100, 200, 300, 500],
    "max_depth": [None, 10, 20],
    "min_samples_split": [2, 5, 10],
    "min_samples_leaf": [1, 2, 4],
    "max_features": ["sqrt", "log2", 0.3],
}

kf = KFold(n_splits=5, shuffle=True, random_state=SEED)

# %% SÉLECTION DU COMPOSÉ ET TRAIN/TEST SPLIT

print(f"Exécution RF Moyennes pour le composé : {compose}")

## X et Y
# 1. FORÇAGE NUMÉRIQUE
df_data[compose] = pd.to_numeric(df_data[compose], errors="coerce")
df_propre = df_data.dropna(subset=[compose])
print(f"Echantillons valides pour ce composé : {len(df_propre)} / {len(df_data)}")

# 2. ISOLATION DU SANCTUAIRE (Modification : 30 échantillons externes)
df_train_val, df_test_externe = train_test_split(
    df_propre, test_size=30, random_state=SEED
)

# Sauvegarde du jeu de validation (pour que les scripts Répétitions puissent le lire)
chemin_test_externe = d0 / "commun" / idparam / f"valid_externe_{compose}.csv"
chemin_test_externe.parent.mkdir(parents=True, exist_ok=True)
df_test_externe.to_csv(chemin_test_externe, index=False)

# 3. EXTRACTION MATRICES (avec bruit de fond pour forcer la régression dans nirs4all)
y = df_train_val[compose].values.astype(float) + np.random.normal(0, 1e-5, size=len(df_train_val))
X = df_train_val[col_spectres].values

# variables pour traquer le meilleur prétraitement
meilleur_rmsecv_global = float("inf")
meilleur_modele_joblib = None
meilleur_pipeline_pre = []
rapport_du_champion = None

# la liste qui va contenir les résultats
tableau_compose = []

## BOUCLE PRINCIPALE SUR LES PRÉTRAITEMENTS
for id_pre, chaine_r_brute in enumerate(liste_pretraitements_r):
    try:
        etapes_pretraitement = pre_translation(chaine_r_brute)
        
        # ---------------------------------------------------------
        # ÉTAPE 1 : PRÉTRAITEMENT OFFICIEL NIRS4ALL + INTERCEPTEUR
        # ---------------------------------------------------------
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
                    raise ValueError("Le prétraitement a supprimé 100% du spectre.")
                pass 
                
            if 'X_transforme' in panier_donnees:
                X_transforme = panier_donnees['X_transforme']
            else:
                raise ValueError("nirs4all a échoué avant de générer la matrice transformée.")
                
        else:
            X_transforme = X
            
        if X_transforme.shape[1] == 0:
            raise ValueError(f"Le prétraitement a supprimé toutes les variables du spectre.")

        # ---------------------------------------------------------
        # ÉTAPE 2 : ENTRAÎNEMENT PUR ET TRANSPARENT AVEC SCIKIT-LEARN
        # ---------------------------------------------------------
        random_search = RandomizedSearchCV(
            estimator=RandomForestRegressor(random_state=SEED, n_jobs=-1),
            param_distributions=param_grid,
            n_iter=30,
            cv=kf,
            scoring="neg_mean_squared_error",
            random_state=SEED,
        )
        
        random_search.fit(X_transforme, y)
        
        # ---------------------------------------------------------
        # ÉTAPE 3 : EXTRACTION DIRECTE ET CALCUL DES MÉTRIQUES
        # ---------------------------------------------------------
        meilleurs_params = str(random_search.best_params_)
        score_neg_mse = random_search.best_score_
        rmsecv = float(np.sqrt(abs(score_neg_mse)))

        pred_train = random_search.predict(X_transforme)
        rc, _, rmsec, _, _ = calculer_metriques(y, pred_train, y, pred_train)
        
        variance_y = np.var(y)
        r2cv = 1 - (score_neg_mse / variance_y) if variance_y != 0 else 0

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
            print("\n🚨 SÉCURITÉ FAIL-FAST DÉCLENCHÉE 🚨")
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
        print(f"Erreur Prétraitement {id_pre + 1} ({chaine_r_brute}) ignorée : {e}")
        continue

# %% SAUVEGARDES ET TEST EXTERNE

dossier_compose = d0 / "random_forest" / "moyennes" / "Results" / idparam / compose
dossier_compose.mkdir(parents=True, exist_ok=True)

# 1. Sauvegarde du tableau complet de l'exploration des prétraitements
df_compose = pd.DataFrame(tableau_compose)
chemin_csv_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_{compose}.csv"
df_compose.to_csv(chemin_csv_compose, sep=";", index=False)

try:
    chemin_excel_compose = dossier_compose / f"RANDOMSEARCH_DETAILS_{compose}.xlsx"
    df_compose.to_excel(chemin_excel_compose, index=False)
except ModuleNotFoundError:
    pass

# 2. Sauvegarde du modèle champion et Test Externe
df_predictions_ext = pd.DataFrame() # Initialisation sécurisée pour les graphiques

if meilleur_modele_joblib is not None:
    print(f"\n🏆 Meilleur modèle RF validé (RMSECV: {meilleur_rmsecv_global:.4f})")

    try:
        X_ext = df_test_externe[col_spectres].values
        y_ext_true = df_test_externe[compose].values
        
        if len(meilleur_pipeline_pre) > 0:
            panier_ext = {}
            class InterceptorPredict(BaseEstimator, RegressorMixin):
                def fit(self, X_t, y_t, **kwargs):
                    panier_ext['X_transforme'] = X_t
                    return self
                def predict(self, X_t):
                    return np.zeros(len(X_t))
                    
            pipeline_ext = meilleur_pipeline_pre + [{"model": InterceptorPredict()}]
            
            try:
                nirs4all.run(dataset=(X_ext, y_ext_true), pipeline=pipeline_ext)
            except:
                pass 
                
            X_ext_transforme = panier_ext.get('X_transforme', X_ext)
        else:
            X_ext_transforme = X_ext

        # Prédiction Scikit-Learn
        pred_ext = meilleur_modele_joblib.predict(X_ext_transforme)
        
        _, _, _, rmsep_ext, rpd_ext = calculer_metriques(
            y_ext_true, pred_ext, y_ext_true, pred_ext
        )
        
        # Calcul du R2p (Validation Externe)
        from sklearn.metrics import r2_score
        r2p_ext = r2_score(y_ext_true, pred_ext)

        rapport_du_champion["Crash_Test_Externe"] = {
            "R2p_Externe": round(r2p_ext, 4),
            "RMSEP_Externe": round(rmsep_ext, 4),
            "RPD_Externe": round(rpd_ext, 4),
        }
        print(f"🔥 SCORE INVIOLABLE -> R2p: {r2p_ext:.4f} | RMSEP: {rmsep_ext:.4f} | RPD: {rpd_ext:.4f}")

        # --- REPOSITIONNEMENT DU BLOC TABLEAU DE PRÉDICTIONS (RÉSOLUTION DU BUG) ---
        df_predictions_ext = pd.DataFrame({
            "Echantillon": df_test_externe['ech'].values if 'ech' in df_test_externe.columns else range(1, len(y_ext_true) + 1),
            "Vraie_Valeur": y_ext_true,
            "Valeur_Predite": pred_ext,
            "Erreur_Absolue": np.abs(y_ext_true - pred_ext)
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
            print(f"Note: Impossible de calculer les incertitudes d'arbres ({e_ic})")

        # Sauvegarde du CSV complet des prédictions
        chemin_csv_pred_ext = dossier_compose / f"PREDICTIONS_EXTERNES_{compose}.csv"
        df_predictions_ext.to_csv(chemin_csv_pred_ext, sep=";", index=False)
        print(f"📊 Fichier de prédictions détaillées sauvegardé : {chemin_csv_pred_ext.name}")

    except Exception as e_test:
        print(f"❌ Erreur critique lors du test externe : {e_test}")
        
    # Export des fichiers du champion
    chemin_modele = dossier_compose / f"modele_RF_A_{compose}.joblib"
    joblib.dump(meilleur_modele_joblib, chemin_modele)

    with open(dossier_compose / f"rapport_A_{compose}.json", "w", encoding="utf-8") as f:
        json.dump(rapport_du_champion, f, indent=4)
else:
    print(f"❌ Aucun modèle n'a pu être entraîné pour {compose}.")


# %% GRAPHIQUES DE DIAGNOSTIC ET DE PUBLICATION
from matplotlib.backends.backend_pdf import PdfPages

if meilleur_modele_joblib is not None:
    print(f"\n🎨 Génération du rapport graphique consolidé pour {compose}...")
    
    chemin_rapport_pdf = dossier_compose / f"RAPPORT_GRAPHIQUES_{compose}.pdf"
    chemin_rapport_png = dossier_compose / f"RAPPORT_GRAPHIQUES_{compose}.png"

    figures_generees = []

    with PdfPages(chemin_rapport_pdf) as pdf:

        # =========================================================================
        # GRAPH 1 : ROBUSTESSE ET EXPLORATION (Style Publication Académique Strict)
        # =========================================================================
        try:
            with plt.style.context('default'):
                fig_robustesse, ax_rob = plt.subplots(figsize=(7, 6))
                
                plt.rcParams['font.family'] = 'serif'
                plt.rcParams['font.serif'] = ['Times New Roman'] + plt.rcParams['font.serif']

                # Points d'exploration : cercles vides gris foncé
                ax_rob.scatter(
                    df_compose["RMSECV"], df_compose["RMSEC"],
                    facecolors='none', edgecolors='#555555', s=35, alpha=0.6,
                    label="Prétraitements testés", zorder=3
                )

                # Point champion rouge vif détouré
                champion_row = df_compose.loc[df_compose["RMSECV"].idxmin()]
                ax_rob.scatter(
                    champion_row["RMSECV"], champion_row["RMSEC"],
                    color="crimson", s=110, edgecolor="black", linewidth=1.2,
                    label="Champion Absolu", zorder=5
                )

                # Bissectrice y = x
                min_val = min(df_compose["RMSECV"].min(), df_compose["RMSEC"].min())
                max_val = max(df_compose["RMSECV"].max(), df_compose["RMSEC"].max())
                lims = [min_val * 0.9, max_val * 1.1]
                ax_rob.plot(lims, lims, color="black", linestyle="--", linewidth=1.2, alpha=0.5, label="y = x", zorder=2)

                # Formatage du cadre complet noir fermé (style Origin/R base)
                ax_rob.set_title(f"Overfitting des Prétraitements - {compose}", fontsize=13, fontweight="bold", pad=12, fontfamily='serif')
                ax_rob.set_xlabel("RMSECV (Validation)", fontsize=11, fontfamily='serif')
                ax_rob.set_ylabel("RMSEC (Calibration)", fontsize=11, fontfamily='serif')
                
                for spine in ax_rob.spines.values():
                    spine.set_visible(True)
                    spine.set_color('black')
                    spine.set_linewidth(1)
                
                ax_rob.tick_params(direction='in', length=5, width=1, colors='black')
                ax_rob.set_xlim(lims)
                ax_rob.set_ylim(lims)
                
                ax_rob.legend(frameon=True, facecolor='white', edgecolor='black', loc="upper left", fontsize=10)

                pdf.savefig(fig_robustesse, bbox_inches="tight")  
                figures_generees.append(fig_robustesse)
        except Exception as e_g1:
            print(f"❌ Erreur Graphique 1 : {e_g1}")


        # =========================================================================
        # GRAPH 2 : PUBLICATION SCIENTIFIQUE (Style R ggplot2 - Predicted vs Measured)
        # =========================================================================
        if not df_predictions_ext.empty:
            try:
                with plt.style.context('ggplot'):
                    fig_publi, ax_pub = plt.subplots(figsize=(8, 6))

                    x_pred = df_predictions_ext["Valeur_Predite"].values
                    y_vrai = df_predictions_ext["Vraie_Valeur"].values

                    # Nuage de points (Style R: cercles vides)
                    ax_pub.scatter(x_pred, y_vrai, facecolors='none', edgecolors='black', s=35, alpha=0.7, zorder=3)

                    # Droite de régression linéaire bleue
                    z = np.polyfit(x_pred, y_vrai, 1)
                    p = np.poly1d(z)
                    x_line = np.linspace(x_pred.min(), x_pred.max(), 100)
                    ax_pub.plot(x_line, p(x_line), color='blue', linestyle='-', linewidth=1.5, zorder=2)

                    # Diagonale parfaite 1:1 rouge pointillée
                    min_val = min(x_pred.min(), y_vrai.min())
                    max_val = max(x_pred.max(), y_vrai.max())
                    marge = (max_val - min_val) * 0.05
                    limites = [min_val - marge, max_val + marge]
                    ax_pub.plot(limites, limites, color="red", linestyle="--", linewidth=1.5, zorder=1)

                    # Extraction et affichage complet des métriques requises
                    r2p = rapport_du_champion["Crash_Test_Externe"]["R2p_Externe"]
                    rmsep = rapport_du_champion["Crash_Test_Externe"]["RMSEP_Externe"]
                    rpd = rapport_du_champion["Crash_Test_Externe"]["RPD_Externe"]
                    
                    texte_metriques = (
                        f"Modèle = RF\n"
                        f"R2p_val = {r2p:.3f}\n"
                        f"RMSEp = {rmsep:.3f}\n"
                        f"RPD = {rpd:.3f}\n"
                        f"n_ech = {len(y_vrai)}"
                    )
                    
                    ax_pub.text(0.02, 0.96, texte_metriques, transform=ax_pub.transAxes, fontsize=13, 
                                verticalalignment='top', horizontalalignment='left', color='black')

                    ax_pub.set_title(f"Prédictions vs Mesures : {compose}", fontsize=15, loc='left', pad=15, color='black')
                    ax_pub.set_xlabel("Valeurs Prédites", fontsize=13, color='black')
                    ax_pub.set_ylabel("Valeurs Mesurées", fontsize=13, color='black')
                    ax_pub.tick_params(colors='black')

                    pdf.savefig(fig_publi, bbox_inches="tight") 
                    figures_generees.append(fig_publi)

            except Exception as e_papier:
                print(f"❌ Erreur Graphique 2 : {e_papier}")


        # =========================================================================
        # GRAPH 3 : PUBLICATION SCIENTIFIQUE (Feature Importance - Stem Plot)
        # =========================================================================
        try:
            with plt.style.context('default'):
                fig_stem, ax_stem = plt.subplots(figsize=(8, 5))
                
                plt.rcParams['font.family'] = 'serif'
                plt.rcParams['font.serif'] = ['Times New Roman'] + plt.rcParams['font.serif']

                toutes_longueurs = [float(str(c).replace("x.", "")) for c in col_spectres]
                pre_gagnant = rapport_du_champion["Pretraitement_Gagnant"]

                match_reduction = re.search(r"list\('red',\s*c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", pre_gagnant)
                if match_reduction:
                    drop_start = int(match_reduction.group(1))
                    drop_end = int(match_reduction.group(2))
                    step = int(match_reduction.group(3))
                    end_idx = len(toutes_longueurs) - drop_end
                    x_values = toutes_longueurs[drop_start:end_idx:step]
                else:
                    x_values = toutes_longueurs

                if len(x_values) != len(importances):
                    x_values = list(range(len(importances)))
                    xlabel_text = "Index des variables (Spectre retaillé)"
                else:
                    xlabel_text = "Wavelengths (nm)"

                couleur_barres = "#4169E1"  # RoyalBlue
                
                ax_stem.vlines(x=x_values, ymin=0, ymax=importances, color=couleur_barres, linewidth=1.5, alpha=0.8)
                ax_stem.plot(x_values, importances, marker='o', markersize=2, color=couleur_barres, linestyle='None')
                ax_stem.axhline(y=0, color='gray', linewidth=0.8, linestyle='-')

                ax_stem.set_title("RF", fontsize=16, fontweight='bold', pad=15, fontfamily='serif')
                ax_stem.set_ylabel("Importance", fontsize=14, fontfamily='serif')
                ax_stem.set_xlabel(xlabel_text, fontsize=12, fontfamily='serif')
                
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
            print(f"❌ Erreur Graphique 3 : {e_graph}")

    # =========================================================================
    # ASSEMBLAGE DE LA COPIE PNG MULTI-PAGES VERTICALE
    # =========================================================================
    if figures_generees:
        from matplotlib.backends.backend_agg import FigureCanvasAgg
        images = []
        for f in figures_generees:
            canvas = FigureCanvasAgg(f)
            canvas.draw()
            s, (width, height) = canvas.print_to_buffer()
            img = np.frombuffer(s, np.uint8).reshape((height, width, 4))
            images.append(img)
            plt.close(f)

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
            print(f"✅ Rapport consolidé généré :\n- {chemin_rapport_pdf.name} (3 pages)\n- {chemin_rapport_png.name} (1 image longue)")
        except ImportError:
            print("⚠️ Module PIL manquant pour assembler le PNG combiné.")