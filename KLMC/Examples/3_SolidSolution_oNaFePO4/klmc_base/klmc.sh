#!/bin/bash
# setting up the environment for SGE:
#$ -cwd
#$ -m e
#$ -M uccazq0@master
#$ -N klmc
#$ -e klmc.error
#$ -o klmc.output

echo "LIB CHECK"
echo $LD_LIBRARY_PATH
ldd /home/uccawkj/KLMC/klmc
echo "check done --------------------"

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/gcc/7.2.0/lib64/
echo "LIB LOADING"
ldd /home/uccawkj/KLMC/klmc

#/home/fonz/bin_tmp/klmc_new
/home/uccawkj/KLMC/klmc

cd run
bash ./../aftertouch.sh
cd ..
