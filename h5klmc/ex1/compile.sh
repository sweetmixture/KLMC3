module load cray-hdf5

h5cc t1.c -o t1.x
mv *.o obj
./t1.x
h5dump example.h5 > t1.dump

h5cc t2.c -o t2.x
mv *.o obj
./t2.x
h5dump t2.h5 > t2.dump

h5cc t3.c -o t3.x
mv *.o obj
./t3.x
h5dump t2.h5 > t3.dump

h5cc 1d.c -o 1d.x
mv *.o obj
./1d.x
h5dump 1d.h5 > 1d.dump
