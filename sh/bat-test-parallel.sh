#!/bin/bash

#SBATCH --partition=Lewis
#SBATCH --nodes=2
#SBATCH --ntasks=20
#SBATCH --mem=16G

# DAT_DIR="/data/sdr8y/coopetition_networks"
R_DIR="/home/sdr8y/coopetition_networks/R"


module load miniconda3
if [ ! -d ~/data/miniconda/envs/myr-env ]; then 
  echo “conda env ‘myr-env’ does not exist. Create environment first.”
else
  source /cluster/software/miniconda3/etc/profile.d/conda.sh
  conda activate myr-env 
  Rscript "$R_DIR/slurm_test_btergm_parallel.R" 
fi


echo "finished test."
