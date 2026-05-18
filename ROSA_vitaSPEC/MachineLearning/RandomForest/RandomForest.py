# %% IMPORTATION DES BIBLIOTHÈQUES
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Prétraitement et séparation
from scipy.signal import savgol_filter
from sklearn.model_selection import train_test_split, GridSearchCV

# Le modèle
from sklearn.ensemble import RandomForestRegressor

# Métriques d'évaluation
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.inspection import permutation_importance

# %% CHARGEMENT ET PRÉTRAITEMENT DES DONNÉES
# Remplacer 'tes_donnees.csv' par ton fichier.
# On suppose ici que les colonnes spectrales vont de la colonne 1 à l'avant-dernière,
# et que la dernière colonne est ta variable cible (Y).
df = pd.read_csv("tes_donnees.csv")

X = df.iloc[:, :-1].values  # Matrice des spectres
y = df.iloc[:, -1].values  # Vecteur cible (ex: BRIX, taux de protéines, etc.)
longueurs_onde = df.columns[:-1].astype(float)  # Récupérer les nm pour les graphiques

# Prétraitement classique : Lissage Savitzky-Golay
# window_length (impaire) et polyorder à adapter selon le bruit de tes spectres
X_sg = savgol_filter(X, window_length=15, polyorder=2, deriv=0)

# %% PARTITIONNEMENT (Train / Test Split)
# Séparation 70% calibration / 30% prédiction
X_train, X_test, y_train, y_test = train_test_split(
    X_sg,
    y,
    test_size=0.3,
    random_state=42,  # Fixer le seed pour la reproductibilité
)

# %% OPTIMISATION DES HYPERPARAMÈTRES (GridSearchCV)

# Définition de la grille d'hyperparamètres à tester
param_grid = {
    "n_estimators": [100, 200, 300],  # Nombre d'arbres (N)
    "max_depth": [None, 10, 20],  # Profondeur max (D)
    "min_samples_split": [2, 5, 10],  # Échantillons min pour diviser un nœud (S)
    "min_samples_leaf": [1, 2, 4],  # Échantillons min dans une feuille (L)
}

# Initialisation du modèle de base
rf_base = RandomForestRegressor(
    random_state=42, n_jobs=-1
)  # n_jobs=-1 utilise tous les cœurs du CPU

# Recherche sur grille avec validation croisée (5-fold)
print("Recherche des hyperparamètres optimaux en cours...")
grid_search = GridSearchCV(
    estimator=rf_base,
    param_grid=param_grid,
    cv=5,
    scoring="neg_mean_squared_error",
    verbose=1,
)

grid_search.fit(X_train, y_train)

# Récupération du meilleur modèle
best_rf = grid_search.best_estimator_
print(f"Meilleurs hyperparamètres : {grid_search.best_params_}")

# %% ÉVALUATION DU MODÈLE (Prédictions et Métriques)
y_pred_train = best_rf.predict(X_train)
y_pred_test = best_rf.predict(X_test)

# Calcul du R² et de la RMSE
r2_test = r2_score(y_test, y_pred_test)
rmse_test = np.sqrt(mean_squared_error(y_test, y_pred_test))

# Calcul du RPD (Ratio of Performance to Deviation) - Très utile en chimiométrie
sd_test = np.std(y_test)
rpd = sd_test / rmse_test

print("\n--- Performances sur le jeu de Prédiction (Test) ---")
print(f"R² (Coefficient de détermination) : {r2_test:.4f}")
print(f"RMSEP (Erreur de prédiction)      : {rmse_test:.4f}")
print(f"RPD                               : {rpd:.4f}")

# %% INTERPRÉTATION : IMPORTANCE DES VARIABLES

# Méthode A : Importance par pureté (MDI - intrinsèque au modèle)
importances_mdi = best_rf.feature_importances_

# Visualisation (type lignes verticales comme dans l'article)
plt.figure(figsize=(12, 5))
plt.vlines(x=longueurs_onde, ymin=0, ymax=importances_mdi, color="blue", alpha=0.7)
plt.title("Importance des variables spectrales (MDI - Baisse de l'impureté)")
plt.xlabel("Longueur d'onde (nm)")
plt.ylabel("Importance")
plt.grid(True, linestyle="--", alpha=0.5)
plt.show()

# Méthode B : Importance par permutation (MDA - plus robuste)
# Calcule la chute de performance quand on mélange aléatoirement une longueur d'onde
resultats_permutation = permutation_importance(
    best_rf, X_test, y_test, n_repeats=10, random_state=42, n_jobs=-1
)
importances_mda = resultats_permutation.importances_mean

plt.figure(figsize=(12, 5))
plt.vlines(x=longueurs_onde, ymin=0, ymax=importances_mda, color="red", alpha=0.7)
plt.title("Importance des variables spectrales (MDA - Permutation OOB)")
plt.xlabel("Longueur d'onde (nm)")
plt.ylabel("Baisse du R²")
plt.grid(True, linestyle="--", alpha=0.5)
plt.show()
