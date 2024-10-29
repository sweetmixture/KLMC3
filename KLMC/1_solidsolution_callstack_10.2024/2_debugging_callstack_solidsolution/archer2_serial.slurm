#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=klmc_dev
#SBATCH --time=00:20:00

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

# SCOREP ENV
module unload perftools-base/22.12.0
module load other-software
module load PrgEnv-gnu
module load scalasca/2.6.1-gcc11
export SCOREP_MEMORY_RECORDING=true

KLMCEXE="/work/e05/e05/wkjee/Software/KLMC3-PythonInterface-Dev-10.2024/KLMC3/KLMC_DEV.10.2024/klmc.scorep.debug"
srun --ntasks=1 --cpus-per-task=1 ${KLMCEXE}
	1> stdout 2> stderr

#
# KLMC2 post-processing for using generated inputfiles on KLMC3 task-farming interface
#

#cd run
#bash ./../replaceXtoA.sh 49999
#cd ..
