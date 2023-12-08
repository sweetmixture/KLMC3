#!/bin/bash -l

# Batch script to run an MPI parallel job under SGE with Intel MPI.

# Request ten minutes of wallclock time (format hours:minutes:seconds).
#$ -l h_rt=00:20:00

# Request 1 gigabyte of RAM per process (must be an integer)
#$ -l mem=2G

# Set the name of the job.
#$ -N tf_sge

# Select the MPI parallel environment and 16 processes.
#$ -pe mpi 80

# Set the working directory to somewhere in your scratch space.
# Replace "<your_UCL_id>" with your UCL user ID :
#$ -cwd


## using -P Test > limited to 2 nodes for 1 hour wtime
#$ -P Test
#$ -A UCL_chemM_Woodley

# Run our MPI job.  GERun is a wrapper that launches MPI jobs on our clusters.
# ulimit -s unlimited
export OMP_NUM_THREADS=1

# USE PYTHON - CHANGE THIS FOR YOUR ACCOUNT SPECIFICATION
source /home/uccawkj/miniconda3/bin/activate
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/uccawkj/miniconda3/lib/
# USE PYTHON

# IF USING UCL MACHINE WITH 'gerun'
# gerun /path/to/taskfarm.x 1> stdout 2> stderr
# ELSE (that supporting 'mpirun' cmd)
mpirun -np 80 /path/to/tf.x
