#!/bin/bash

DATA="dat_HR_25_DIADE_clean.csv"
ID_PARAM="HR"

# liste des composes
COMPOSES=(
    "C14.0"
    "C16.0"
    "C18.0"
    "C18.1n9"
    "C18.2"
    "FFA"
    "trans.alpha.carotene"
    "trans.beta.carotene"
    "total.trans.carotenes.natif"
    "ratio.alpha.beta"
    "ratio.alpha.natif"
    "ratio.beta.natif"
    "X13.cis.beta.carotene"
    "X9.cis.beta.carotene"
    "total.beta.carotene"
    "total.carotene"
    "lycopene"
    "aT"
    "aT3"
    "gT3"
    "dT3"
    "total.T3"
    "total.toco"
)

for c in "${COMPOSES[@]}"; do
    echo "lancement validation de $c"
    sbatch run_validation_rf.slurm "$c" "$DATA" "$ID_PARAM"
done

echo "Jobs HR tous lances"