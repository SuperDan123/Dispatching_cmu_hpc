#!/bin/bash
#SBATCH --job-name=test          # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks-per-node=40      # number of tasks per node (adjust when using MPI)
#SBATCH --cpus-per-task=3       # cpu-cores per task (>1 if multi-threaded tasks, adjust when using OMP)
#SBATCH --time=120:00:00          # total run time limit (D-HH:MM:SS)
#SBATCH --partition=intel        # partition(queue) where you submit (amd/intel/gpu-a30/gpu-l20)
#SBATCH --account=empiricalio # slurm account name
#SBATCH --mail-user=dsongaj@connect.ust.hk
#SBATCH --mail-type=ALL

cd $HOME
cd $HOME/Dispatching
R CMD INSTALL --preclean .

# Render the R Markdown file
Rscript -e "rmarkdown::render('~/Dispatching/report/multihome/report_search_parameter_for_positive_costs.Rmd', output_file='report_search_parameter_for_positive_costs_121.html')" > log_rmarkdown_render 2>&1
