#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=klmc3py
#SBATCH --time=0:20:00
#SBATCH --nodes=8

# Replace [budget code] below with your budget code (e.g. t01)
#SBATCH --account=e05-react-smw
#SBATCH --partition=standard
#SBATCH --qos=short

# Set the number of threads to 1
#   This prevents any threaded system libraries from automatically 
#   using threading.
export OMP_NUM_THREADS=1

# USE PYTHON
source /work/e05/e05/wkjee/miniconda3/bin/activate
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/work/e05/e05/wkjee/miniconda3/lib/
# USE PYTHON

# Launch the parallel job
#   Using 512 MPI processes and 128 MPI processes per node
#   srun picks up the distribution from the sbatch options

EXE=${EXE}
srun --ntasks-per-node=128 --cpus-per-task=1 --distribution=block:block --hint=nomultithread --exact ${EXE} 1> stdout 2> stderr

mkdir result
mv A* ./result

mkdir log
mv master.log ./log
mv workgroup*.log ./log
