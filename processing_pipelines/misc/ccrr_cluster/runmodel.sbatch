#/usr/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --output=R-model-parallel.log
#SBATCH --mail-type=ALL

# load the module
ml R/4.1

Rscript run_exp1_model.r

