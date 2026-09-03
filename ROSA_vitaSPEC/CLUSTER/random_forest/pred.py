# -*- coding: utf-8 -*-

# %% IMPORTATIONS
import pandas as pd
import sys
import re
from pathlib import Path
import joblib
import json
import numpy as np
import warnings

import nirs4all
from sklearn.base import BaseEstimator, RegressorMixin

# Masquer les warnings scikit-learn lies aux versions
warnings.filterwarnings("ignore", category=UserWarning)

## Pathing de base
d0 = Path("/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/")
sys.path.append(str((d0 / "commun").resolve()))
sys.path.append(str((d0 / "random_forest").resolve()))

## Fonctions maison
from diy_functions.pre_translation import pre_translation

# %% CONFIGURATION
DOSSIER_MATRICES = d0 / "commun" / "MATRICE_Compilees_CSV"
DOSSIER_MODELES_PACK = d0 / "PRODUCTION" / "modeles_pack" / "random_forest"

# Dossier principal ou seront crees les sous-dossiers de resultats
DOSSIER_SORTIE_GLOBAL = d0 / "PRODUCTION" / "PREDICTIONS_FINALES"
DOSSIER_SORTIE_GLOBAL.mkdir(parents=True, exist_ok=True)

# Mettre a False si tu veux UNIQUEMENT le fichier du tissu identifie dans le titre
PREDIRE_LES_3_TISSUS = True  

# Correspondance Nom de dossier de modeles -> Nom du fichier de sortie
TISSUS_MAPPING = {
    "HR": "HR",
    "meso_frais": "meso_frais",
    "meso_silica": "meso_sec"
}

# %% BOUCLE SUR TOUTES LES MATRICES
fichiers_csv = list(DOSSIER_MATRICES.glob("*.csv"))
print(f"==> {len(fichiers_csv)} matrices spectrales trouvees dans {DOSSIER_MATRICES.name}.\n")
print("==> MODELE UTILISE : RANDOM FOREST\n")

for fichier_matrice in fichiers_csv:
    nom_matrice = fichier_matrice.stem
    
    # 1. Filtre sur le nom des fichiers pour determiner le tissu natif
    tissu_natif = None
    
    if "_Vitaspec_S" in nom_matrice:
        tissu_natif = "meso_silica"
    elif "_Vitaspec_HR" in nom_matrice:
        tissu_natif = "HR"
    elif "_Vitaspec_Pobe" in nom_matrice:
        tissu_natif = "meso_frais"
        
    # Si aucun tag n'est reconnu, on ignore la matrice
    if tissu_natif is None:
        print(f"{'='*60}")
        print(f" MATRICE : {nom_matrice} (IGNORE : aucun tag HR/S/Pobe detecte)")
        continue
        
    print(f"{'='*60}")
    print(f" MATRICE : {nom_matrice} (Detecte : {tissu_natif})")
    print(f"{'='*60}")
    
    # Creation du sous-dossier au nom de la matrice en entree
    dossier_out_matrice = DOSSIER_SORTIE_GLOBAL / nom_matrice
    dossier_out_matrice.mkdir(parents=True, exist_ok=True)
    
    # 2. Chargement et nettoyage des donnees de la matrice
    try:
        df_data = pd.read_csv(fichier_matrice, sep=None, engine='python')
    except Exception as e:
        print(f" Erreur de lecture de la matrice {nom_matrice}: {e}")
        continue
        
    col_spectres = [col for col in df_data.columns if str(col).startswith("x.")]
    df_data[col_spectres] = df_data[col_spectres].apply(pd.to_numeric, errors='coerce')
    
    # Exclusion stricte des spectres avec NaN ou flatlines
    std_spectres = df_data[col_spectres].std(axis=1)
    mask_invalid = df_data[col_spectres].isna().any(axis=1) | (std_spectres == 0)
    df_propre = df_data[~mask_invalid].reset_index(drop=True)
    
    X_global = df_propre[col_spectres].values
    y_dummy = np.zeros(len(X_global))
    
    colonnes_identifiants = [col for col in ["ech", "rep", "campagne", "etat", "source"] if col in df_propre.columns]
    
    # 3. Definition des tissus a predire pour cette matrice
    tissus_a_predire = list(TISSUS_MAPPING.keys()) if PREDIRE_LES_3_TISSUS else [tissu_natif]
        
    # 4. Boucle sur les dossiers de Modeles (HR, meso_frais, meso_silica)
    for tissu_modele in tissus_a_predire:
        dossier_tissu_pack = DOSSIER_MODELES_PACK / tissu_modele
        
        if not dossier_tissu_pack.exists():
            print(f" [Avertissement] Le dossier RF pour {tissu_modele} est introuvable.")
            continue
            
        print(f"\n   -> Application des modeles RF du tissu : {tissu_modele}")
        
        # Initialisation du tableau de resultats pour ce tissu
        if colonnes_identifiants:
            df_predictions_tissu = df_propre[colonnes_identifiants].copy()
        else:
            df_predictions_tissu = pd.DataFrame({"ID_Ligne": range(1, len(df_propre) + 1)})
            
        # 5. Boucle sur chaque compose chimique
        for dossier_compose in dossier_tissu_pack.iterdir():
            if not dossier_compose.is_dir():
                continue
            
            compose = dossier_compose.name
            
            # Recherche des 10 modeles (.joblib) dans ce dossier
            modeles_joblib = list(dossier_compose.rglob("*.joblib"))
            if len(modeles_joblib) == 0:
                continue
                
            predictions_des_10_modeles = []
            
            # 6. Boucle sur les 10 modeles du compose
            for chemin_mod in modeles_joblib:
                fichiers_json = list(chemin_mod.parent.glob("*.json"))
                if not fichiers_json:
                    continue
                chemin_json = fichiers_json[0]
                
                try:
                    modele = joblib.load(chemin_mod)
                    with open(chemin_json, "r", encoding="utf-8") as f:
                        rapport = json.load(f)
                        
                    code_pre_gagnant = rapport.get("Pretraitement_Gagnant", "")
                    etapes_pretraitement = pre_translation(code_pre_gagnant)
                    
                    # Transformation NIRS4ALL
                    if len(etapes_pretraitement) > 0:
                        panier_inf = {}
                        class InterceptorInference(BaseEstimator, RegressorMixin):
                            def fit(self, X_t, y_t, **kwargs):
                                panier_inf['X_transforme'] = X_t
                                return self
                            def predict(self, X_t): return np.zeros(len(X_t))
                        
                        pipeline_inf = etapes_pretraitement + [{"model": InterceptorInference()}]
                        
                        try:
                            nirs4all.run(dataset=(X_global, y_dummy), pipeline=pipeline_inf)
                            X_propre = panier_inf.get('X_transforme', X_global)
                            preds = modele.predict(X_propre)
                        except Exception:
                            # Fallback Ligne par Ligne si crashe
                            preds = []
                            for i in range(len(X_global)):
                                X_seul = X_global[i].reshape(1, -1)
                                panier_seul = {}
                                class InterceptorSeul(BaseEstimator, RegressorMixin):
                                    def fit(self, X_t, y_t, **kwargs):
                                        panier_seul['X_transforme'] = X_t
                                        return self
                                    def predict(self, X_t): return np.zeros(len(X_t))
                                pipe_seul = etapes_pretraitement + [{"model": InterceptorSeul()}]
                                
                                try:
                                    nirs4all.run(dataset=(X_seul, y_dummy[:1]), pipeline=pipe_seul)
                                    preds.append(modele.predict(panier_seul['X_transforme'])[0])
                                except Exception:
                                    preds.append(np.nan)
                            preds = np.array(preds)
                    else:
                        preds = modele.predict(X_global)
                        
                    predictions_des_10_modeles.append(preds)
                    
                except Exception as e:
                    print(f"      [Erreur] Modele {chemin_mod.name} ignore : {e}")
            
            # 7. Calcul de la moyenne finale pour ce compose
            if len(predictions_des_10_modeles) > 0:
                moyenne_finale_compose = np.nanmean(predictions_des_10_modeles, axis=0)
                df_predictions_tissu[f"{compose}_predit"] = moyenne_finale_compose
                print(f"      - {compose:<20} : OK (Moyenne de {len(predictions_des_10_modeles)} modeles RF)")
            else:
                print(f"      - {compose:<20} : ECHEC (Aucun modele valide)")
        
        # 8. Sauvegarde du fichier du tissu avec la mention RF
        nom_fichier_sortie = f"predictions_RF_{TISSUS_MAPPING[tissu_modele]}.csv"
        chemin_sortie_csv = dossier_out_matrice / nom_fichier_sortie
        
        df_predictions_tissu.to_csv(chemin_sortie_csv, sep=";", index=False)
        
print("\n" + "="*60)
print(" TERMINE ! Toutes les matrices valides ont ete traitees avec la methode RF.")
print("="*60)