# pretraitements
import re
import numpy as np
from sklearn.preprocessing import FunctionTransformer
from nirs4all.operators.transforms import SNV, SavitzkyGolay, Detrend

from diy_functions.adj_asd import asd_splice_correction_R


def pre_translation(r_string):
    """
    Traduit dynamiquement les prétraitements R (fonction 'pre' du package 'nirsextra') vers un pipeline NIRS4ALL / scikit-Learn.
    """
    pipeline_steps = []

    blocks = r_string.split("list(")

    for block in blocks[1:]:
        block = block.strip()

        # 'adj' [correction des sauts de détecteur]
        if block.startswith("'adj'") or block.startswith('"adj"'):
            adjuster = FunctionTransformer(asd_splice_correction_R)
            pipeline_steps.append(adjuster)

        # 'ref2abs' [conversion de reflectance à absorbance]
        elif block.startswith("'ref2abs'") or block.startswith('"ref2abs"'):
            ref_to_abs = FunctionTransformer(lambda X: np.log10(1 / (X + 1e-9)))
            pipeline_steps.append(ref_to_abs)

        # 'snv' [normalisation]
        elif block.startswith("'snv'") or block.startswith('"snv"'):
            pipeline_steps.append(SNV())

        # 'detr' [correction des tendances de base]
        elif block.startswith("'detr'") or block.startswith('"detr"'):
            pipeline_steps.append(Detrend())

        # 'red' [reduction de la bande d'absorbtion prise en compte]
        elif block.startswith("'red'") or block.startswith('"red"'):
            # accepte c(X, Y, Z) ou c(X, Y)
            match = re.search(r"c\(\s*(\d+)\s*,\s*(\d+)(?:\s*,\s*(\d+))?\s*\)", block)
            if match:
                drop_debut = int(match.group(1))  # Ex: 800
                drop_fin = int(match.group(2))  # Ex: 750
                step = int(match.group(3)) if match.group(3) else 1  # Ex: 1

                # Traduction en indexation Python : on commence à l'index `drop_debut` et
                # on s'arrête à l'index `-drop_fin` (en partant de la fin) (si 0 -> 1)
                start_idx = drop_debut
                end_idx = -drop_fin if drop_fin > 0 else None

                reducer = FunctionTransformer(
                    lambda X, s=start_idx, e=end_idx, st=step: X[:, s:e:st]
                )
                pipeline_steps.append(reducer)

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
