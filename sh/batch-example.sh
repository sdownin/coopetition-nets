#!/bin/bash

#SBATCH --partition Lewis
#SBATCH --mem 16G

module load miniconda3
if [ ! -d ~/data/miniconda/myr-env ]; then # creates the env if not exist
conda create --yes -n myr-env -c conda-forge r-base r-lme4
fi

source activate myr-env

Rscript test.R
