# %% [1] IMPORTATION DES BIBLIOTHÈQUES
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import make_classification

# %% [2] GÉNÉRATION DE DONNÉES FICTIVES
# On crée un jeu de données pour tester le modèle
X, y = make_classification(
    n_samples=1000,
    n_features=4,
    n_informative=2,
    n_redundant=0,
    random_state=0,
    shuffle=False,
)

df = pd.DataFrame(X, columns=["Feature_A", "Feature_B", "Feature_C", "Feature_D"])
df["Target"] = y

# Affiche les 5 premières lignes
df.head()

# %% [3] ENTRAÎNEMENT DU MODÈLE
clf = RandomForestClassifier(max_depth=2, random_state=0)
clf.fit(X, y)

print("Modèle entraîné avec succès !")
print(f"Score de précision : {clf.score(X, y):.2%}")

# %% [4] VISUALISATION DES RÉSULTATS
# Importance des variables (le truc classique en Random Forest)
importances = clf.feature_importances_
std = np.std([tree.feature_importances_ for tree in clf.estimators_], axis=0)
indices = np.argsort(importances)[::-1]

plt.figure(figsize=(10, 5))
plt.title("Importance des variables")
plt.bar(range(X.shape[1]), importances[indices], color="skyblue", align="center")
plt.xticks(range(X.shape[1]), [df.columns[i] for i in indices])
plt.show()

# %%
