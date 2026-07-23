#!/bin/bash

DATA="dat_mean_Meso_frais_2425.csv"
ID_PARAM="meso_frais"

# liste des composes
COMPOSES=(
    "eau"
    "C14.0"
    "C16.0"
    "C18.0"
    "C18.1n9"
    "C18.1n7"
    "C18.2"
    "C18.3"
    "C20.0"
    "tlip.MS"
    "trans.alpha.carotene"
    "trans.beta.carotene"
    "total.trans.carotene.natif"
    "ratio.alpha.beta"
    "ratio.alpha.natif"
    "ratio.beta.natif"
    "X13.cis.beta.carotene"
    "X9.cis.beta.carotene"
    "total.beta.carotene"
    "total.carotene"
)

for c in "${COMPOSES[@]}"; do
    echo "lancement validation de $c"
    sbatch run_validation_rf.slurm "$c" "$DATA" "$ID_PARAM"
done

echo "Jobs frais tous lances"