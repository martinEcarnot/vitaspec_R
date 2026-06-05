# pretraitements
import re
import numpy as np
from sklearn.base import BaseEstimator, TransformerMixin
from nirs4all.operators.transforms import SNV, SavitzkyGolay, Detrend

from diy_functions.adj_asd import asd_splice_correction_R

# =========================================================
# 1. CRÉATION DES CLASSES DE PRÉTRAITEMENT ROBUSTES
# =========================================================


class ASDTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self  # Ne fait rien à l'entraînement

    def transform(self, X):
        return asd_splice_correction_R(X)  # Applique ta fonction


class RefToAbsTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self

    def transform(self, X):
        return np.log10(1 / (X + 1e-9))


class ReducerTransformer(BaseEstimator, TransformerMixin):
    def __init__(self, start_idx, end_idx, step):
        self.start_idx = start_idx
        self.end_idx = end_idx
        self.step = step

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        return X[:, self.start_idx : self.end_idx : self.step]


# =========================================================
# 2. LA FONCTION DE TRADUCTION
# =========================================================


def pre_translation(r_string):
    """
    Traduit dynamiquement les prétraitements R vers un pipeline NIRS4ALL / scikit-Learn.
    """
    pipeline_steps = []
    blocks = r_string.split("list(")

    for block in blocks[1:]:
        block = block.strip()

        # 'adj' [correction des sauts de détecteur]
        if block.startswith("'adj'") or block.startswith('"adj"'):
            pipeline_steps.append(ASDTransformer())

        # 'ref2abs' [conversion de reflectance à absorbance]
        elif block.startswith("'ref2abs'") or block.startswith('"ref2abs"'):
            pipeline_steps.append(RefToAbsTransformer())

        # 'snv' [normalisation]
        elif block.startswith("'snv'") or block.startswith('"snv"'):
            pipeline_steps.append(SNV())

        # 'detr' [correction des tendances de base]
        elif block.startswith("'detr'") or block.startswith('"detr"'):
            pipeline_steps.append(Detrend())

        # 'red' [reduction de la bande d'absorbtion prise en compte]
        elif block.startswith("'red'") or block.startswith('"red"'):
            match = re.search(r"c\(\s*(\d+)\s*,\s*(\d+)(?:\s*,\s*(\d+))?\s*\)", block)
            if match:
                drop_debut = int(match.group(1))
                drop_fin = int(match.group(2))
                step = int(match.group(3)) if match.group(3) else 1

                start_idx = drop_debut
                end_idx = -drop_fin if drop_fin > 0 else None

                pipeline_steps.append(ReducerTransformer(start_idx, end_idx, step))

        # 'sder' [derivation pour corriger les effets de dispersion]
        elif block.startswith("'sder'") or block.startswith('"sder"'):
            match = re.search(r"c\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)", block)
            if match:
                deriv = int(match.group(1))
                poly = int(match.group(2))
                window = int(match.group(3))

                if window % 2 == 0:
                    window += 1
                pipeline_steps.append(
                    SavitzkyGolay(window_length=window, polyorder=poly, deriv=deriv)
                )

    return pipeline_steps
