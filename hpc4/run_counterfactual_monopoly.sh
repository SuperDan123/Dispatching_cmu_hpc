#!/usr/bin/env bash
# Run the monopoly counterfactual on ORCHARD inside the `r-dispatching` Enroot
# container.
#
# Unlike run_counterfactual_maximum_margin.sh / run_counterfactual_minimum_wage.sh,
# main/multihome/06_counterfactual/counterfactual_monopoly.R solves
# `solve_equilibrium(...)` across ALL markets in a single call, so this script
# takes no t/j arguments and submits a single (long-running) job.
#
# Submit:        sbatch hpc4/run_counterfactual_monopoly.sh
# Inspect log:   cat slurm-<jobid>.out
#
# --- One-time: build the r-dispatching .sqsh on the login node -------------
# Pick an R base image, then install GSL + R deps + this package inside.
# Example (rocker/r-ver as a starting point):
#
#   enroot import -o r-dispatching.sqsh docker://rocker/r-ver:4.4.0
#   enroot create  --name r-dispatching r-dispatching.sqsh
#   enroot start   --rw  r-dispatching   # install GSL, R packages, and `R CMD INSTALL .`
#   exit
#   enroot export  --output /home/dansong/.local/share/enroot/r-dispatching.sqsh r-dispatching
#
# Required inside the container:
#   - R (>= 4.x)  with Rcpp, RcppEigen, RcppGSL, nleqslv, foreach, doParallel,
#     here, magrittr, codetools, ggplot2, kableExtra
#   - GSL development libs (libgsl-dev on Debian/Ubuntu)
#   - This repo installed as the `Dispatching` package (`R CMD INSTALL .` from repo root)

#SBATCH --job-name=cf-monopoly
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32              # doParallel uses detectCores(); set per ORCHARD node size
#SBATCH --time=24:00:00                 # full multi-market solve; raise/lower as needed
##SBATCH --partition=<set-your-partition>
##SBATCH --account=<set-your-account>

# Email notifications (BEGIN, END, FAIL, REQUEUE, ALL, NONE)
#SBATCH --mail-user=dansong@andrew.cmu.edu
#SBATCH --mail-type=END,FAIL

# Container image — absolute path; #SBATCH does not expand $PWD or ~.
#SBATCH --container-image=/home/dansong/.local/share/enroot/r-dispatching.sqsh

# Bind mounts. --container-mount-home makes /home/$USER (where the repo lives)
# visible inside the container at the same path.
#SBATCH --container-mounts=/tmp:/tmp
#SBATCH --container-mount-home

# If the image's ENTRYPOINT interferes, uncomment:
##SBATCH --no-container-entrypoint

set -euo pipefail
export TMPDIR=/tmp                      # ORCHARD's /mnt/tmp/<jobid> sometimes missing

# ---------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------
echo "=========================================="
echo "Job ID:     ${SLURM_JOB_ID:-n/a}"
echo "Node:       ${SLURMD_NODENAME:-n/a}"
echo "User:       ${USER}"
echo "Host:       $(hostname)"
echo "Date:       $(date)"
echo "PWD:        $(pwd)"
echo "Container:  r-dispatching (Enroot/Pyxis)"
echo "=========================================="

# ---------------------------------------------------------------
# Move into the project (renamed from Dispatching to Dispatching_cmu_hpc)
# ---------------------------------------------------------------
PROJECT_DIR="${HOME}/Dispatching_cmu_hpc"
if [ ! -d "${PROJECT_DIR}" ]; then
  echo "ERROR: project dir not found: ${PROJECT_DIR}" >&2
  exit 1
fi
cd "${PROJECT_DIR}"

# ---------------------------------------------------------------
# Verify R + Dispatching package are available inside the container
# ---------------------------------------------------------------
echo "R location:       $(command -v R || echo 'NOT FOUND')"
echo "Rscript location: $(command -v Rscript || echo 'NOT FOUND')"
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found in container PATH" >&2
  echo "PATH=${PATH}" >&2
  exit 1
fi
echo "R version:"
Rscript -e 'cat(R.version$version.string, "\n")'
echo "R library paths:"
Rscript -e 'cat(.libPaths(), sep = "\n"); cat("\n")'
echo "Dispatching package:"
Rscript -e 'if (requireNamespace("Dispatching", quietly = TRUE)) {
  cat("OK, version", as.character(packageVersion("Dispatching")), "\n")
} else {
  cat("MISSING\n"); quit(status = 1)
}'

# ---------------------------------------------------------------
# Output / log directories
# ---------------------------------------------------------------
OUT_DIR="output/multihome/counterfactual/monopoly"
mkdir -p "${OUT_DIR}"

LOG_OUT="${OUT_DIR}/log_${SLURM_JOB_ID:-local}.out"
LOG_ERR="${OUT_DIR}/log_${SLURM_JOB_ID:-local}.err"

# ---------------------------------------------------------------
# Run the R script
# ---------------------------------------------------------------
echo "Starting Rscript at $(date)"
echo "  stdout -> ${LOG_OUT}"
echo "  stderr -> ${LOG_ERR}"

# `|| RSCRIPT_EXIT_CODE=$?` keeps `set -e` from aborting before we can
# print a useful error message and dump the tail of the R log.
RSCRIPT_EXIT_CODE=0
Rscript main/multihome/06_counterfactual/counterfactual_monopoly.R \
  >  "${LOG_OUT}" \
  2> "${LOG_ERR}" \
  || RSCRIPT_EXIT_CODE=$?

if [ "${RSCRIPT_EXIT_CODE}" -ne 0 ]; then
  echo "ERROR: Rscript failed with exit code ${RSCRIPT_EXIT_CODE}" >&2
  echo "Tail of stderr (${LOG_ERR}):" >&2
  tail -n 60 "${LOG_ERR}" >&2 || true
  echo "Tail of stdout (${LOG_OUT}):" >&2
  tail -n 30 "${LOG_OUT}" >&2 || true
  exit "${RSCRIPT_EXIT_CODE}"
fi

# ---------------------------------------------------------------
# Confirm the saved RDS is on disk
# ---------------------------------------------------------------
SAVED="${OUT_DIR}/equilibrium_monopoly.rds"
if [ -f "${SAVED}" ]; then
  echo "Saved: ${SAVED} ($(du -h "${SAVED}" | cut -f1))"
else
  echo "WARNING: expected output not found: ${SAVED}" >&2
fi

echo "Job finished at $(date)"
