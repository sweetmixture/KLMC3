#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=tf_single
#SBATCH --time=0:20:00
#SBATCH --nodes=2

# Replace [budget code] below with your budget code (e.g. t01)
#SBATCH --account=e05-power-sok
#SBATCH --partition=standard
#SBATCH --qos=short

# Set the number of threads to 1
#   This prevents any threaded system libraries from automatically 
#   using threading.
export OMP_NUM_THREADS=1

# Launch the parallel job
#   Using 512 MPI processes and 128 MPI processes per node
#   srun picks up the distribution from the sbatch options

srun --ntasks-per-node=128 --cpus-per-task=1 --distribution=block:block --hint=nomultithread --exact \
			/work/e05/e05/wkjee/Software/master-worker-C-Fortran-Python/mpi/C/master_worker/archer2_klmcgulp/build/tf.x 1> stdout.out 2> stderr.out
