#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=scorep
#SBATCH --time=0:20:00
#SBATCH --nodes=1

# Replace [budget code] below with your budget code (e.g. t01)
#SBATCH --account=e05-react-smw
#SBATCH --partition=standard
#SBATCH --qos=short

# Set the number of threads to 1
#   This prevents any threaded system libraries from automatically 
#   using threading.
export OMP_NUM_THREADS=1

# SCOREP ENV
module unload perftools-base/22.12.0
module load other-software
module load PrgEnv-gnu
module load scalasca/2.6.1-gcc11
export SCOREP_MEMORY_RECORDING=true

# Launch the parallel job
#   Using 512 MPI processes and 128 MPI processes per node
#   srun picks up the distribution from the sbatch options

#/work/ta159/ta159/ta159wkjee/Software/checkmem/check_mem.sh 1> check_mem.out 2> check_mem.err &
#sleep 30

EXE=""
srun --ntasks-per-node=128 --cpus-per-task=1 --distribution=block:block --hint=nomultithread --exact ${EXE} 1> stdout 2> stderr

mkdir result
mv A* ./result

mkdir log
mv master.log ./log
mv workgroup*.log ./log
