# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import joblib
import json
import numpy as np

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin

## pathing
d0 = Path(
    "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/"
)
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

## Fonctions maison
from diy_functions.pre_translation import pre_translation


# %% GESTION DES ARGUMENTS

# verif
if len(sys.argv) < 3:
    print("error miss arguments.")
    sys.exit(1)

fichier_data = sys.argv[1]
idparam = sys.argv[2]

DATA = d0 / "commun" / fichier_data
dossier_resultats = d0 / "random_forest" / "moyennes" / "Results" / idparam

if not dossier_resultats.exists():
    print(f"error pas de {dossier_resultats}")
    sys.exit(1)


# %% CHARGEMENT DU DATASET GLOBAL

df_data = pd.read_csv(DATA)

# extrac spectres
col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]

df_data[col_spectres] = df_data[col_spectres].apply(pd.to_numeric, errors='coerce')
df_data = df_data.replace([np.inf, -np.inf], np.nan)

## destection NA
std_spectres = df_data[col_spectres].std(axis=1)

mask_nan = df_data[col_spectres].isna().any(axis=1)
mask_flat = (std_spectres == 0)

# Un spectre est invalide s'il a un trou OU s'il est tout plat
mask_invalid = mask_nan | mask_flat

df_anomalies = df_data[mask_invalid].copy()

# tableau spectre invalides
cols_id_presentes = [col for col in ["ech", "rep", "num_spectre", "campagne"] if col in df_data.columns]

if len(df_anomalies) > 0:
    df_recap_enleves = df_anomalies[cols_id_presentes].copy()
    
    raisons = []
    vars_nan_list = []
    
    for index, row in df_anomalies.iterrows():
        if mask_flat.loc[index]:
            raisons.append("spectre invalide (flatline / division par zero)")
            vars_nan_list.append("Toutes (ecart-type nul)")
        else:
            raisons.append("NaN")
            colonnes_fautives = row[col_spectres][row[col_spectres].isna()].index.tolist()
            texte_fautives = ", ".join(colonnes_fautives[:10])
            vars_nan_list.append(texte_fautives)
            
    df_recap_enleves["raison_suppression"] = raisons
    df_recap_enleves["variables_NaN"] = vars_nan_list
else:
    ligne_propre = {col: ["aucun"] for col in cols_id_presentes}
    ligne_propre["raison_suppression"] = ["aucune anomalie"]
    ligne_propre["variables_NaN"] = ["aucune"]
    df_recap_enleves = pd.DataFrame(ligne_propre)

# Save
dossier_inf = d0 / "random_forest" / "moyennes" / "predictions"
dossier_inf.mkdir(parents=True, exist_ok=True)
chemin_recap_modalite = dossier_inf / f"spectres_NaN_{idparam}.csv"

df_recap_enleves.to_csv(chemin_recap_modalite, sep=";", index=False)
try:
    df_recap_enleves.to_excel(chemin_recap_modalite.with_suffix('.xlsx'), index=False)
except ModuleNotFoundError:
    pass

print(f"{idparam} : {len(df_anomalies)} spectres retires")


# garde les spectres sans NaN
df_data = df_data[~mask_invalid].reset_index(drop=True)

X_global = df_data[col_spectres].values

# tableau de pred
colonnes_identifiants = [col for col in ["ech", "rep", "campagne", "etat", "source"] if col in df_data.columns]

if colonnes_identifiants:
    df_predictions_globales = df_data[colonnes_identifiants].copy()
else:
    df_predictions_globales = pd.DataFrame({"ID_Ligne": range(1, len(df_data) + 1)})

# 'y' factice car nirs4all demande un tuple (X, y) 
y_dummy = np.zeros(len(X_global))

print(f" {X_global.shape[0]} ech sur {X_global.shape[1]} longueurs d'ondes\n")
print(f"spectres : {X_global.shape[0]} ech sur {X_global.shape[1]} longueurs d'ondes.\n")


# %% BOUCLE SUR TOUS LES COMPOSES

# on liste les dossiers
dossiers_composes = [d for d in dossier_resultats.iterdir() if d.is_dir()]
print(f"{len(dossiers_composes)} dossiers (composes) trouves dans : {dossier_resultats.name}")

# compteur 
nb_succes = 0

for dossier_compose in dossiers_composes:
    compose = dossier_compose.name
    chemin_modele = dossier_compose / f"modele_RF_A_{compose}.joblib"
    chemin_json = dossier_compose / f"rapport_A_{compose}.json"
    
    # Verif meilleur mod
    if not chemin_modele.exists() or not chemin_json.exists():
        print(f"{compose:<20} ignore mod ou json manquant")
        continue
        
    try:
        # chargement mod
        modele = joblib.load(chemin_modele)
        with open(chemin_json, "r", encoding="utf-8") as f:
            rapport = json.load(f)
            
        code_pre_gagnant = rapport["Pretraitement_Gagnant"]
        etapes_pretraitement = pre_translation(code_pre_gagnant)
        
        # pretrait
        if len(etapes_pretraitement) > 0:
            panier_inf = {}
            
            class InterceptorInference(BaseEstimator, RegressorMixin):
                def fit(self, X_t, y_t, **kwargs):
                    panier_inf['X_transforme'] = X_t
                    return self
                def predict(self, X_t):
                    return np.zeros(len(X_t))
                    
            pipeline_inf = etapes_pretraitement + [{"model": InterceptorInference()}]
            
            predictions = []
            
            # On boucle sur chaque spectre individuellement
            for i in range(len(X_global)):
                X_seul = X_global[i].reshape(1, -1)
                
                try:
                    # Traitement nirs4all
                    panier_inf = {}
                    class InterceptorInference(BaseEstimator, RegressorMixin):
                        def fit(self, X_t, y_t, **kwargs):
                            panier_inf['X_transforme'] = X_t
                            return self
                        def predict(self, X_t): return np.zeros(len(X_t))
                    
                    pipeline_inf = etapes_pretraitement + [{"model": InterceptorInference()}]
                    nirs4all.run(dataset=(X_seul, y_dummy[:1]), pipeline=pipeline_inf)
                    
                    X_propre = panier_inf.get('X_transforme', X_seul)
                    
                    # pred
                    pred_val = modele.predict(X_propre)[0]
                    predictions.append(pred_val)
                    
                except Exception:
                    # Si un spectre explose, on le remplace par NaN et on continue
                    predictions.append(np.nan)
        else:
            predictions = modele.predict(X_global)
        
        # on ajoute la colonne
        nom_colonne = f"{compose}_predit"
        df_predictions_globales[nom_colonne] = predictions
        
        print(f"{compose:<20}  pred reussie")
        nb_succes += 1
        
    except Exception as e:
        print(f"{compose:<20} error ({e})")

print("-" * 50)

# %% SAUVEGARDE DU TABLEAU GLOBAL

if nb_succes > 0:
    dossier_inf = d0 / "random_forest" / "moyennes" / "predictions"
    dossier_inf.mkdir(parents=True, exist_ok=True)

    chemin_csv_final = dossier_inf / f"{idparam}_pred.csv"
    df_predictions_globales.to_csv(chemin_csv_final, sep=";", index=False)

    try:
        chemin_excel_final = dossier_inf / f"{idparam}_pred.xlsx"
        df_predictions_globales.to_excel(chemin_excel_final, index=False)
        msg_excel = f" et .xlsx"
    except ModuleNotFoundError:
        msg_excel = ""
else:
    print("\n error aucune pred")

## moy + stats descriptives
if 'ech' in df_predictions_globales.columns:

    # isole uniquement les colonnes predites (on retire 'ech', 'rep', etc.)
    colonnes_chimiques = [col for col in df_predictions_globales.columns if col not in colonnes_identifiants]

    # tableau moy
    df_moyennes = df_predictions_globales.groupby('ech')[colonnes_chimiques].mean().reset_index()

    df_moyennes.columns = [str(col).replace('_predit', '') for col in df_moyennes.columns]

    chemin_moyennes_csv = dossier_inf / f"{idparam}_pred_moy.csv"
    df_moyennes.to_csv(chemin_moyennes_csv, sep=";", index=False)


    # tableau stats descriptives
    df_long = df_predictions_globales.melt(
        id_vars=['ech'], 
        value_vars=colonnes_chimiques,
        var_name='Variable', 
        value_name='Valeur'
    )
    
    # clean
    df_long['Variable'] = df_long['Variable'].str.replace('_predit', '')

    df_stats = df_long.groupby(['ech', 'Variable'])['Valeur'].agg(
        mean='mean',
        sd='std',
        min='min',
        max='max'
    ).reset_index()

    chemin_stats_csv = dossier_inf / f"{idparam}_stats_descriptives.csv"
    df_stats.to_csv(chemin_stats_csv, sep=";", index=False)