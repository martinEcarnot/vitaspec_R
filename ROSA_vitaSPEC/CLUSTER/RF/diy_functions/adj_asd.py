import numpy as np


def asd_splice_correction_R(X, iadj_python=[650, 1450]):
    """
    Traduction de la fonction R `adj_asd`
    Corrige les sauts de détecteur par extrapolation linéaire locale (fenetre de 5 points)
    Pour nous, les sauts ASD sont à 1001nm et 1451nm (indices Python 650 et 1450)
    """
    Xo = np.copy(X)
    ws = 5  # fenetre de 5 points avant le saut

    for J in iadj_python:
        # coordonnees des points avant le saut
        x = np.arange(J - ws + 1, J + 1)
        Y = Xo[:, x]

        # moyennes pour la regression
        mx = np.mean(x)
        my = np.mean(Y, axis=1)  # moyenne pour chaque spectre

        # Calcul de la pente (b) et de l'ordonnee à l'origine (b0) (=cov(x, Y) / var(x))
        x_centered = x - mx
        Y_centered = Y - my[:, np.newaxis]

        # pente (b) = somme((x - mx)*(y - my)) / somme((x - mx)²)
        b = np.sum(x_centered * Y_centered, axis=1) / np.sum(x_centered**2)
        b0 = my - b * mx

        # prediction de la valeur theorique au point du saut (J + 1)
        pred = b0 + b * (J + 1)

        # Calcul de l'erreur du capteur (dif)
        dif = Xo[:, J + 1] - pred

        # equivalent du "kronecker" R : soustraction sur tout le reste du spectre
        Xo[:, J + 1 :] -= dif[:, np.newaxis]

    return Xo
