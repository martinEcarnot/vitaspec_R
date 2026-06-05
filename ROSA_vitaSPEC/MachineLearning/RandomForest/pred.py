# %% IMPORTATIONS
import pandas as pd
from pathlib import Path
import joblib
import sys

## Pathing (à adapter si besoin)
d0 = Path(
    "C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/vitaspec_R/ROSA_vitaSPEC"
)
sys.path.append(str((d0 / "MachineLearning").resolve()))

import nirs4all  # Indispensable pour que joblib comprenne les objets du pipeline

# %% CHARGEMENT DES DONNEES COMPLETES

# On charge le fichier avec tes 306 spectres (sans retirer les NA cette fois !)
DATA = d0 / "Data" / "dat" / "dat_mean_Meso_sec_2425_DIADE.csv"
df_data = pd.read_csv(DATA)

# On isole uniquement la matrice X (les spectres bruts)
col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]
X_complet = df_data[col_spectres].values

print(f"📊 Données chargées : {len(df_data)} échantillons à prédire.")

# Création du tableau final qui va accueillir les prédictions
# On récupère la colonne d'identification de tes échantillons (ajuste 'ech' si le nom est différent)
if "ech" in df_data.columns:
    df_predictions = df_data[["ech"]].copy()
else:
    # Si pas de colonne ID, on garde l'index
    df_predictions = pd.DataFrame(index=df_data.index)


# %% BOUCLE DE PREDICTION

# La même liste que ton script d'entraînement
liste_composes = ["C18.1n9", "C18.2", "trans.beta.carotene"]

print("\n🚀 Lancement des prédictions...")

for compose in liste_composes:
    # On va chercher le champion dans son dossier respectif
    chemin_modele = (
        d0
        / "MachineLearning"
        / "Results"
        / compose
        / f"modele_RF_CHAMPION_{compose}.joblib"
    )

    if chemin_modele.exists():
        print(f"✔️ Modèle trouvé pour {compose}. Calcul en cours...")

        try:
            # 1. Chargement du pipeline complet (Prétraitement gagnant + Random Forest)
            modele_champion = joblib.load(chemin_modele)

            # 2. Prédiction magique sur l'ensemble de la matrice X brute
            predictions = modele_champion.predict(X_complet)

            # 3. Ajout des résultats dans notre tableau final sous forme de nouvelle colonne
            df_predictions[f"Pred_{compose}"] = predictions

        except Exception as e:
            print(f"❌ Erreur lors de la prédiction pour {compose} : {e}")
    else:
        print(f"⚠️ Aucun modèle sauvegardé trouvé pour {compose}.")


# %% SAUVEGARDE DU TABLEAU DE PREDICTIONS

print("\n💾 Sauvegarde du grand tableau de prédictions...")

# On crée un dossier spécifique pour ranger ces prédictions globales
dossier_sauvegarde = d0 / "MachineLearning" / "Results" / "Predictions_Globales"
dossier_sauvegarde.mkdir(parents=True, exist_ok=True)

# Sauvegarde en CSV
chemin_csv = dossier_sauvegarde / "Predictions_306_Echantillons_Meso.csv"
df_predictions.to_csv(chemin_csv, sep=";", index=False)

# Sauvegarde en Excel (Optionnel)
try:
    chemin_excel = dossier_sauvegarde / "Predictions_306_Echantillons_Meso.xlsx"
    df_predictions.to_excel(chemin_excel, index=False)
except ModuleNotFoundError:
    pass

print(f"✅ Terminé ! Retrouve ton tableau ici : {chemin_csv}")
