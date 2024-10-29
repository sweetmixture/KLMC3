#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=klmc_lis10
#SBATCH --time=3:00:00

# Replace [budget code] below with your budget code (e.g. t01)
#SBATCH --account=e05-power-smw
#SBATCH --partition=serial
#SBATCH --qos=serial

# Set the number of threads to 1
#   This prevents any threaded system libraries from automatically 
#   using threading.
export OMP_NUM_THREADS=1

# Launch the parallel job
#   Using 512 MPI processes and 128 MPI processes per node
#   srun picks up the distribution from the sbatch options

srun --ntasks=1 --cpus-per-task=1 /work/e05/e05/wkjee/Software/KLMC/klmc
    1> stdout 2> stderr

cd run
bash ./../replaceXtoA.sh 49999
cd ..

