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
    "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/Results/"
)
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

## Fonctions maison
from diy_functions.pre_translation import pre_translation


# %% GESTION DES ARGUMENTS

# Vérification stricte des arguments en ligne de commande
fichier_data = sys.argv[1]
idparam = sys.argv[2]

DATA = d0 / "commun" / fichier_data
dossier_resultats = d0 / "random_forest" / "moyennes" / "Results" / idparam

if not dossier_resultats.exists():
    print(f"error : pas de dossier résultats {dossier_resultats}")
    sys.exit(1)


# %% CHARGEMENT DU DATASET GLOBAL

df_data = pd.read_csv(DATA)

# Extraction des colonnes spectrales (les variables X)
col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]
X_global = df_data[col_spectres].values

# Création du tableau final de prédictions
# On sécurise la récupération des identifiants (s'ils existent dans le CSV)
colonnes_identifiants = [
    col for col in ["ech", "campagne", "etat", "source"] if col in df_data.columns
]

if colonnes_identifiants:
    df_predictions_globales = df_data[colonnes_identifiants].copy()
else:
    # Si aucun identifiant n'est trouvé, on crée un simple index numérique
    df_predictions_globales = pd.DataFrame({"ID_Ligne": range(1, len(df_data) + 1)})

# On crée un 'y' factice car nirs4all requiert un tuple (X, y) pour fonctionner
y_dummy = np.zeros(len(X_global))

print(
    f"📊 Matrice spectrale chargée : {X_global.shape[0]} échantillons analysés sur {X_global.shape[1]} longueurs d'ondes.\n"
)


# %% BOUCLE D'INFÉRENCE SUR TOUS LES COMPOSÉS

# On liste uniquement les dossiers (qui correspondent aux composés testés)
dossiers_composes = [d for d in dossier_resultats.iterdir() if d.is_dir()]
print(
    f"🔍 {len(dossiers_composes)} dossiers (composés) trouvés dans : {dossier_resultats.name}"
)
print("-" * 50)

# Compteur pour le bilan final
nb_succes = 0

for dossier_compose in dossiers_composes:
    compose = dossier_compose.name
    chemin_modele = dossier_compose / f"modele_RF_A_{compose}.joblib"
    chemin_json = dossier_compose / f"rapport_A_{compose}.json"

    # Vérification de l'intégrité : Le modèle champion a-t-il bien été sauvegardé ?
    if not chemin_modele.exists() or not chemin_json.exists():
        print(f"⏩ {compose:<20} | IGNORÉ (Modèle ou JSON manquant)")
        continue

    try:
        # 1. Chargement de l'IA et de son rapport d'entraînement
        modele = joblib.load(chemin_modele)
        with open(chemin_json, "r", encoding="utf-8") as f:
            rapport = json.load(f)

        code_pre_gagnant = rapport["Pretraitement_Gagnant"]
        etapes_pretraitement = pre_translation(code_pre_gagnant)

        # 2. Application du Prétraitement Chimique Gagnant
        if len(etapes_pretraitement) > 0:
            panier_inf = {}

            class InterceptorInference(BaseEstimator, RegressorMixin):
                def fit(self, X_t, y_t, **kwargs):
                    panier_inf["X_transforme"] = X_t
                    return self

                def predict(self, X_t):
                    return np.zeros(len(X_t))

            pipeline_inf = etapes_pretraitement + [{"model": InterceptorInference()}]

            try:
                # nirs4all transforme la matrice globale avec la recette spécifique du composé
                nirs4all.run(dataset=(X_global, y_dummy), pipeline=pipeline_inf)
            except Exception as e_run:
                # On ignore les erreurs de run interne de nirs4all tant que
                # la matrice a bien été poussée dans le panier par le fit()
                pass

            X_transforme = panier_inf.get("X_transforme", X_global)
        else:
            # Si le prétraitement gagnant était "raw" (brut)
            X_transforme = X_global

        # 3. Prédiction globale par le Random Forest
        predictions = modele.predict(X_transforme)

        # 4. Ajout de la colonne au grand tableau consolidé
        nom_colonne = f"{compose}_predit"
        df_predictions_globales[nom_colonne] = predictions

        print(f"✅ {compose:<20} | Prédiction réussie")
        nb_succes += 1

    except Exception as e:
        print(f"❌ {compose:<20} | ÉCHEC ({e})")

print("-" * 50)

# %% SAUVEGARDE DU TABLEAU GLOBAL CONSOLIDÉ

if nb_succes > 0:
    dossier_inf = d0 / "random_forest" / "moyennes" / "Inferences_Globales"
    dossier_inf.mkdir(parents=True, exist_ok=True)

    chemin_csv_final = dossier_inf / f"PREDICTIONS_GLOBALES_{idparam}.csv"
    df_predictions_globales.to_csv(chemin_csv_final, sep=";", index=False)

    try:
        chemin_excel_final = dossier_inf / f"PREDICTIONS_GLOBALES_{idparam}.xlsx"
        df_predictions_globales.to_excel(chemin_excel_final, index=False)
        msg_excel = f" et .xlsx"
    except ModuleNotFoundError:
        msg_excel = ""

    print(f"\n🎉 TERMINÉ ! Le grand tableau consolidé a été généré en .csv{msg_excel}")
    print(f"📂 Emplacement : {chemin_csv_final}")
else:
    print(
        "\n⚠️ AUCUNE prédiction n'a pu être réalisée. Vérifie tes dossiers de modèles."
    )
