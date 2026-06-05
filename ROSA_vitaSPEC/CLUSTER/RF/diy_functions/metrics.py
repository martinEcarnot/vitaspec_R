import numpy as np
from sklearn.metrics import mean_squared_error


def calculer_metriques(y_train, pred_train, y_test, pred_test):
    """
    Calcule les 5 métriques de base (Rc, Rp, RMSEC, RMSEP, RPD)
    """
    # Root Mean Squared Error
    rmsec = np.sqrt(mean_squared_error(y_train, pred_train))
    rmsep = np.sqrt(mean_squared_error(y_test, pred_test))

    # 2. Coefficients de Corrélation (Rc et Rp)
    # [0, 1] permet d'extraire la valeur de la matrice de corrélation
    rc = np.corrcoef(y_train, pred_train)[0, 1]
    rp = np.corrcoef(y_test, pred_test)[0, 1]

    # 3. Ratio of Performance to Deviation (RPD)
    # ddof=1 pour calculer l'écart-type de l'échantillon (standard statistique)
    sd_test = np.std(y_test, ddof=1)
    rpd = sd_test / rmsep

    return rc, rp, rmsec, rmsep, rpd
