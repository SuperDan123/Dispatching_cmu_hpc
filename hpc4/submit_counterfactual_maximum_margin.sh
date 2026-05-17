#!/bin/bash
# Script to submit counterfactual maximum margin jobs one by one
# Run this directly from the login node (not as a SLURM job)
# Usage: bash submit_counterfactual_maximum_margin.sh

set -e  # Exit on error (will disable in loops where we handle errors)

# Change to the correct directory
cd $HOME/Dispatching || {
  echo "ERROR: Failed to change to $HOME/Dispatching" >&2
  exit 1
}

echo "=========================================="
echo "Submit job started at: $(date)"
echo "Current directory: $(pwd)"
echo "User: $USER"
echo "=========================================="

# Verify we're in the right place
if [ ! -f "hpc4/run_counterfactual_maximum_margin.sh" ]; then
  echo "ERROR: Cannot find hpc4/run_counterfactual_maximum_margin.sh" >&2
  echo "Current directory: $(pwd)" >&2
  exit 1
fi

# Verify sbatch is available
if ! command -v sbatch &> /dev/null; then
  echo "ERROR: sbatch command not found" >&2
  exit 1
fi

# Configuration
BATCH_SIZE=5          # Submit this many jobs before checking queue
WAIT_TIME=30          # Wait this many seconds if queue is full
MAX_QUEUE_SIZE=15     # Maximum number of jobs to have in queue at once

# Track submitted jobs
SUBMITTED=0
FAILED=0
TOTAL=0

# Calculate total number of jobs
for t in {1..5}; do
  case $t in
    1|2)
      MAX_J=75
      ;;
    3)
      MAX_J=69
      ;;
    4|5)
      MAX_J=72
      ;;
  esac
  TOTAL=$((TOTAL + MAX_J))
done

echo "Total jobs to submit: $TOTAL"
echo "Max queue size: $MAX_QUEUE_SIZE"
echo "Batch size: $BATCH_SIZE"
echo "Wait time: ${WAIT_TIME}s"
echo ""

# Function to check current queue size
check_queue_size() {
  squeue -u $USER -h 2>/dev/null | wc -l || echo "0"
}

# Function to submit a single job with queue management
submit_job() {
  local t=$1
  local j=$2
  
  # Check queue size before submitting
  local queue_size=$(check_queue_size)
  while [ "$queue_size" -ge "$MAX_QUEUE_SIZE" ]; do
    echo "Queue full ($queue_size >= $MAX_QUEUE_SIZE), waiting ${WAIT_TIME}s..."
    sleep $WAIT_TIME
    queue_size=$(check_queue_size)
  done
  
  # Submit the job
  set +e  # Temporarily disable exit on error
  local submit_output
  submit_output=$(sbatch --job-name=max_margin_t${t}_j${j} \
    hpc4/run_counterfactual_maximum_margin.sh $t $j 2>&1)
  local submit_exit=$?
  set -e  # Re-enable exit on error
  
  if [ $submit_exit -eq 0 ]; then
    # Extract job ID from output (format: "Submitted batch job 12345")
    # Use sed for portability (works on systems without grep -P)
    local job_id=$(echo "$submit_output" | sed -n 's/.*Submitted batch job \([0-9]*\).*/\1/p' 2>/dev/null || echo "")
    if [ -n "$job_id" ]; then
      echo "Submitted job t=${t} j=${j} (job ID: ${job_id})"
    else
      # Still consider it successful even if we can't extract job ID
      echo "Submitted job t=${t} j=${j}"
    fi
    return 0
  else
    echo "ERROR: Failed to submit t=${t} j=${j}" >&2
    echo "Error output: $submit_output" >&2
    return 1
  fi
}

# Main submission loop
echo "Starting job submission..."
echo ""

for t in {1..5}; do
  # Determine max j for this t
  case $t in
    1|2)
      MAX_J=75
      ;;
    3)
      MAX_J=69
      ;;
    4|5)
      MAX_J=72
      ;;
  esac
  
  echo "Processing market t=${t} (zones 1-${MAX_J})..."
  
  for j in $(seq 1 $MAX_J); do
    # Check if output already exists (skip if job already completed)
    output_file="output/multihome/counterfactual/maximum_margin/counterfactual_maximum_margin_t$(printf "%04d" $t)_j$(printf "%04d" $j).rds"
    if [ -f "$output_file" ]; then
      echo "Skipping t=${t} j=${j} (output already exists)"
      continue
    fi
    
    # Submit job
    if submit_job $t $j; then
      SUBMITTED=$((SUBMITTED + 1))
      
      # Check queue size periodically
      if [ $((SUBMITTED % BATCH_SIZE)) -eq 0 ]; then
        queue_size=$(check_queue_size)
        echo "Progress: ${SUBMITTED}/${TOTAL} submitted, ${queue_size} in queue"
        
        # Wait if queue is getting full
        if [ "$queue_size" -ge "$MAX_QUEUE_SIZE" ]; then
          echo "Queue size (${queue_size}) >= max (${MAX_QUEUE_SIZE}), waiting ${WAIT_TIME}s..."
          sleep $WAIT_TIME
        fi
      fi
    else
      FAILED=$((FAILED + 1))
    fi
    
    # Small delay between submissions to avoid overwhelming the scheduler
    sleep 0.5
  done
  
  echo "Completed market t=${t}"
  echo ""
done

# Final summary
echo "=========================================="
echo "Submission completed at: $(date)"
echo "Total jobs: $TOTAL"
echo "Successfully submitted: $SUBMITTED"
echo "Failed: $FAILED"
echo "=========================================="

if [ $FAILED -gt 0 ]; then
  echo "WARNING: $FAILED jobs failed to submit. Check the output above for details." >&2
  exit 1
fi

