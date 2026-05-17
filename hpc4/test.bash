#!/usr/bin/env bash
# ORCHARD cluster: Slurm + Enroot (Pyxis) job template (CPU-only here).
# Guidance: "Transitioning from Docker to Enroot" (CMU ORCHARD Confluence).
#
# Submit (batch):   sbatch test.bash
# Interactive:     srun --pty ... (same #SBATCH-style flags can be passed on the CLI)
#
# --- One-time: build a local .sqsh from Docker (login node, from hpc4/) ---
#   enroot import -o python311.sqsh docker://python:3.11-slim
#   # private registry: put credentials in ~/.config/enroot/.credentials
# Optional sandbox for debugging:
#   enroot create --name mysandbox myimage.sqsh
#   enroot start mysandbox
#   # or with root + read-write to install packages, then:
#   enroot export --output myimage_new.sqsh mysandbox
#
# --- This file: batch job inside the container ---

#SBATCH --job-name=orchard-test
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=00:30:00
##SBATCH --partition=<set-your-partition>    # ORCHARD: use your queue name
##SBATCH --account=dansong       # if your site requires --account

# Email notifications. mail-type options: BEGIN, END, FAIL, REQUEUE, ALL, NONE
#SBATCH --mail-user=dansong@andrew.cmu.edu
#SBATCH --mail-type=ALL

# Local Enroot image — must be an absolute path (#SBATCH does not expand $PWD or ~).
#SBATCH --container-image=/home/dansong/Dispatching_cmu_hpc/hpc4/python311_custom.sqsh

# Mount host paths into the container as host:container (comma-separated).
#SBATCH --container-mounts=/tmp:/tmp
# ORCHARD Confluence example — uncomment and edit if that layout applies:
##SBATCH --container-mounts=/project/flame/${USER}:/data,/tmp:/tmp
# Mount your cluster home into the container (optional):
#SBATCH --container-mount-home

# If the image ENTRYPOINT breaks your command, try uncommenting:
##SBATCH --no-container-entrypoint

set -euo pipefail

echo "Job ID: ${SLURM_JOB_ID:-n/a}  Node: ${SLURMD_NODENAME:-n/a}"
echo "Running inside Enroot/Pyxis container."

cd "${SLURM_SUBMIT_DIR:-.}"
python train.py

echo "Done."
