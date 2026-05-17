#!/bin/bash

# NOTE: Lines starting with "#SBATCH" are valid SLURM commands or statements,
#       while those starting with "#" and "##SBATCH" are comments.  Uncomment
#       "##SBATCH" line means to remove one # and start with #SBATCH to be a
#       SLURM command or statement.


#SBATCH -J counterfactual #Slurm job name

# Set the maximum runtime, uncomment if you need it
#SBATCH -t 1-00:00:00 #Maximum runtime of 48 hours

# Enable email notificaitons when job begins and ends, uncomment if you need it
#SBATCH --mail-user=kohei.kawagucci@gmail.com #Update your email address
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

# Choose partition (queue) to use. Note: replace <partition_to_use> with the name of partition
#SBATCH -p cpu-share

# Use 2 nodes and 80 cores
#SBATCH -N 1 -n 40

# Setup runtime environment if necessary
# For example, setup intel MPI environment
module load R
ROOT=/scratch/PI/kkawaguchi

# Go to the job submission directory and run your application
# cd $ROOT/projects
# R CMD INSTALL --preclean --no-multiarch --with-keep.source --no-lock Dispatching
cd $ROOT/projects/Dispatching
rm slurm*
rm log_counterfactual
Rscript main/05_counterfactual/simulate_counterfactuals_actual_market.R > log_counterfactual 2>&1
