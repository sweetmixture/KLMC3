## KLMC3
* * *
The compilation of this program requires KLMC3-libgulp
Link: https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main
* * *

Once the installation of KLMC3-libgulp is done, following the steps for its compilation.

**STEP 1**. Locate the correct 'CMakeLists.txt' in the root directory

Example CMakeLists.txt files can be found in ```cmakelist``` directory, including...  

(a) ```CMakeLists.txt.cray```  : for the build on Cray  system, for example, ARCHER2  
(b) ```CMakeLists.txt.intel``` : for the build on Intel system, for example, MMM Young, Katheleen etc  

☝️ Depending on your HPC system, copy the relevent script into the root directory.

Example: compilation on Cray, be in the root directory and do  

```$cp cmakelist/CMakeLists.txt.cray CMakeLists.txt```  

**STEP 2**. go to the ```build``` directory and do (make sure you have cmake version 3.11 above)  

```$cmake ..```

which will generate the Makefile  

**STEP 3**. compilation by typing  

```$make```

this will generate the executable ```tf.x``` in the build directory

* * *

**Test Calculation**
