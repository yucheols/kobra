#!/bin/sh
#SBATCH --job-name=cobra_enm
#SBATCH --nodes=1
#SBATCH --cpus-per-task=30
#SBATCH --time=144:00:00
#SBATCH --mem=350G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yshin@amnh.org
#SBATCH --output=/home/yshin/mendel-nas1/kobra/output/log/cobra_%j-%x.log
#SBATCH --error=/home/yshin/mendel-nas1/kobra/output/error/cobra_%j-%x.err

# load conda in batch mode
source /home/yshin/mendel-nas1/miniconda3/etc/profile.d/conda.sh

# activate conda environment that contains R and all necessary packages
conda activate nsdm_hpc
cd /home/yshin/mendel-nas1/kobra/kobra  # this will be the R working directory on the cluster 

# run the R script
Rscript /home/yshin/mendel-nas1/kobra/kobra/scripts/R/enm_script.R