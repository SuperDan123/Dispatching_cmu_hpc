#!/bin/bash
# Script to check the status of all counterfactual minimum wage jobs
# Usage: bash check_counterfactual_minimum_wage_status.sh

cd $HOME/Dispatching || exit 1

echo "=========================================="
echo "Checking job status at: $(date)"
echo "=========================================="

# Counters
TOTAL=0
COMPLETED=0
RUNNING=0
PENDING=0
MISSING=0
FAILED=0

# Arrays to track failed and missing jobs
FAILED_JOBS=()
MISSING_JOBS=()

# Check each job
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
  
  for j in $(seq 1 $MAX_J); do
    TOTAL=$((TOTAL + 1))
    output_file="output/multihome/counterfactual/minimum_wage/counterfactual_minimum_wage_t$(printf "%04d" $t)_j$(printf "%04d" $j).rds"
    log_err="hpc4/log_t${t}_j${j}.err"
    
    if [ -f "$output_file" ]; then
      COMPLETED=$((COMPLETED + 1))
    else
      # Check if job is in queue
      job_name="min_wage_t${t}_j${j}"
      if squeue -u $USER -n "$job_name" -h 2>/dev/null | grep -q "$job_name"; then
        job_state=$(squeue -u $USER -n "$job_name" -h -o "%T" 2>/dev/null | head -1)
        if [ "$job_state" = "RUNNING" ]; then
          RUNNING=$((RUNNING + 1))
        else
          PENDING=$((PENDING + 1))
        fi
      else
        # Check if there's an error log (failed job)
        if [ -f "$log_err" ]; then
          FAILED=$((FAILED + 1))
          FAILED_JOBS+=("t=${t} j=${j}")
        else
          MISSING=$((MISSING + 1))
          MISSING_JOBS+=("t=${t} j=${j}")
        fi
      fi
    fi
  done
done

echo ""
echo "=========================================="
echo "Status Summary:"
echo "  Total jobs: $TOTAL"
echo "  Completed: $COMPLETED"
echo "  Running: $RUNNING"
echo "  Pending: $PENDING"
echo "  Failed (error log exists): $FAILED"
echo "  Missing/Not started: $MISSING"
echo "=========================================="

# Show failed jobs with details
if [ ${#FAILED_JOBS[@]} -gt 0 ]; then
  echo ""
  echo "=========================================="
  echo "FAILED JOBS (${#FAILED_JOBS[@]} total):"
  echo "=========================================="
  for job in "${FAILED_JOBS[@]}"; do
    echo "  $job"
  done
fi

# Show missing jobs (optional, can be commented out if too verbose)
if [ ${#MISSING_JOBS[@]} -gt 0 ] && [ ${#MISSING_JOBS[@]} -le 50 ]; then
  echo ""
  echo "=========================================="
  echo "MISSING/NOT STARTED JOBS (${#MISSING_JOBS[@]} total):"
  echo "=========================================="
  # Group by market t
  for t in {1..5}; do
    market_jobs=()
    for job in "${MISSING_JOBS[@]}"; do
      job_t=$(echo "$job" | sed 's/t=\([0-9]*\).*/\1/')
      if [ "$job_t" = "$t" ]; then
        market_jobs+=("$job")
      fi
    done
    if [ ${#market_jobs[@]} -gt 0 ]; then
      echo "Market t=${t}: ${#market_jobs[@]} jobs missing"
      # Show first 10, then summarize if more
      if [ ${#market_jobs[@]} -le 10 ]; then
        printf "  %s\n" "${market_jobs[@]}"
      else
        printf "  %s\n" "${market_jobs[@]:0:10}"
        echo "  ... and $(( ${#market_jobs[@]} - 10 )) more"
      fi
    fi
  done
elif [ ${#MISSING_JOBS[@]} -gt 50 ]; then
  echo ""
  echo "=========================================="
  echo "MISSING/NOT STARTED JOBS: ${#MISSING_JOBS[@]} total"
  echo "(Too many to list individually)"
  echo "=========================================="
fi

# Show current queue status
echo ""
echo "Current queue status:"
squeue -u $USER -o "%.10i %.20j %.8T %.10M %.6D %R" 2>/dev/null | head -20

