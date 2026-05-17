#!/bin/bash
#SBATCH --job-name=min_wage        # create a short name for your job (override with --job-name when submitting)
#SBATCH --nodes=1                # node count
#SBATCH --ntasks-per-node=1      # number of tasks per node (adjust when using MPI)
#SBATCH --cpus-per-task=120       # cpu-cores per task (>1 if multi-threaded tasks, adjust when using OMP)
#SBATCH --time=120:00:00          # total run time limit (D-HH:MM:SS)
#SBATCH --partition=intel        # partition(queue) where you submit (amd/intel/gpu-a30/gpu-l20)
#SBATCH --account=empiricalio # slurm account name
#SBATCH --mail-user=dsongaj@connect.ust.hk
#SBATCH --mail-type=ALL
#
# Usage: sbatch --job-name=min_wage_t1_j1 run_counterfactual_minimum_wage.sh <t> <j>
#   t: market index (1-5)
#   j: zone index (max 75 for t=1,2; max 69 for t=3; max 72 for t=4,5)
# Example: sbatch --job-name=min_wage_t1_j1 run_counterfactual_minimum_wage.sh 1 1

# ---------------------------------------------------------------
# Environment setup
# ---------------------------------------------------------------

# Load conda and activate environment
source ~/miniconda3/etc/profile.d/conda.sh
conda activate dispatching

cd $HOME/Dispatching

# Verify we're using the conda R
echo "R location: $(which R)"
echo "Rscript location: $(which Rscript)"
if [ -z "$(which R)" ]; then
  echo "ERROR: R not found in PATH" >&2
  echo "PATH: $PATH" >&2
  exit 1
fi

# Get market and zone indices from command line arguments
T=$1
J=$2

# Validate arguments
if [ -z "$T" ] || [ -z "$J" ]; then
  echo "Error: Please provide market index (t) and zone index (j) as arguments"
  echo "Usage: sbatch run_counterfactual_minimum_wage.sh <t> <j>"
  echo "  t: market index (1-5)"
  echo "  j: zone index (depends on t: max 75 for t=1,2; max 69 for t=3; max 72 for t=4,5)"
  exit 1
fi

# Validate t range [1, 5]
if [ "$T" -lt 1 ] || [ "$T" -gt 5 ]; then
  echo "Error: t must be in range [1, 5], got $T"
  exit 1
fi

# Validate j range based on t
case $T in
  1|2)
    MAX_J=75
    ;;
  3)
    MAX_J=69
    ;;
  4|5)
    MAX_J=72
    ;;
  *)
    echo "Error: Invalid t value: $T"
    exit 1
    ;;
esac

if [ "$J" -lt 1 ] || [ "$J" -gt "$MAX_J" ]; then
  echo "Error: j must be in range [1, $MAX_J] for t=$T, got $J"
  exit 1
fi

# Optional: print diagnostic info
echo "Running on host: $(hostname)"
echo "Processing market: $T, zone: $J"
echo "Date: $(date)"
echo "R version:"
Rscript -e 'cat(R.version$version.string, "\n")'
echo "R library paths:"
Rscript -e 'cat(.libPaths(), sep="\n")'

# ---------------------------------------------------------------
# Run your R script
# ---------------------------------------------------------------

cd $HOME/Dispatching

# Create output directory if it doesn't exist
mkdir -p output/multihome/counterfactual/minimum_wage

# Run R script with market and zone indices as arguments
echo "Starting R script execution..."
Rscript main/multihome/06_counterfactual/counterfactual_minimum_wage.R $T $J \
  > hpc4/log_t${T}_j${J}.out \
  2> hpc4/log_t${T}_j${J}.err
RSCRIPT_EXIT_CODE=$?

if [ $RSCRIPT_EXIT_CODE -ne 0 ]; then
  echo "ERROR: R script failed with exit code $RSCRIPT_EXIT_CODE" >&2
  echo "Check log files for details:" >&2
  echo "  hpc4log_t${T}_j${J}.out" >&2
  echo "  hpc4/log_t${T}_j${J}.err" >&2
  exit $RSCRIPT_EXIT_CODE
fi

# ---------------------------------------------------------------
# Job end marker
# ---------------------------------------------------------------
echo "Job finished at: $(date)"

