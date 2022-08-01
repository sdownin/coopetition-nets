#!/bin/bash

#SBATCH --partition=Lewis
#SBATCH --nodes=1
#SBATCH --mem=24G
#SBATCH --cpus-per-task=4
## # SBATCH --ntasks=20


# DAT_DIR="/data/sdr8y/coopetition_networks"
R_DIR="/home/sdr8y/coopetition_networks/R"

FILES=(
"multiplex_tergm__Asyst Technologies Inc.R"
"multiplex_tergm__Chiron Corporation.R"
"multiplex_tergm__Cymer Inc.R"
"multiplex_tergm__Spartech Corporation.R"
"multiplex_tergm__Technical Olympic USA, Inc.R"
)

module load miniconda3
if [ ! -d ~/data/miniconda/envs/myr-env ]; then 
  echo “conda env ‘myr-env’ does not exist. Create environment first.”
else
  source /cluster/software/miniconda3/etc/profile.d/conda.sh
  conda activate myr-env 
  for ((i = 0; i < ${#FILES[@]}; i++))
  # for f in ${FILES[@]}
  do 
    printf "\n\nProcessing $R_DIR/${FILES[$i]} \n\n"
    Rscript "$R_DIR/${FILES[$i]}" 
  done
fi


echo "finished batch."
