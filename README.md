### Knowledge Led Master Code 3 (KLMC3)

**Compilation Dependency:**  
This program requires GULP 6.1.2 static library, which can be found [here](https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main).  

**Updates ( - 07.24):**  To find the development log, see ```update.list``` in the root directory of this git-repo.  

#### Capabilities
1. Executing Parallel GULP Tasks
- Launch multiple GULP tasks in parallel (large scale GULP tasks across multiple nodes on HPCs based on the 'master-worker' method).  
- Capable of reading KLMC-generated GULP input files and launching GULP (runtime GULP input file generation is not supported yet).  
2. Executing Parallel Python Tasks
- Launch multiple Python (single processor) tasks in parallel.
- Each execution of a Python script occurs within its own directory, similar to using GULP mode.  
- Users are responsible to custom their Python scripts.  

#### Build
1. Using GULP (+PYTHON)
- Make sure to keep the GULP static library [here](https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main).  
- [IMPORTANT] Make sure to keep the same system environment as used for building the static GULP library.  

Once you clone this git-repo on your system, you can find ```CMakeLists.txt``` in the root directory.  

STEP 1. There are a few variables in ```CMakeLists.txt```, which users have to set them correctly.  
For example,  
```
set(COMPILER "CRAY")
set(GULPROOT /path/to/KLMC3-libgulp-6.1.2)
set(ENABLE_PYTHON "FALSE")
set(PYTHONLIB /path/to/lib/libpython3.XX.so)
set(PYTHONINCLUDE /path/to/include/python3.XX/)
```
COMPILER could be set to either 'CRAY' or 'INTEL' depending on your system.  
GULPROOT has to be set to the root of GULP static library.  
ENABLE_PYTHON could be set to either 'TRUE' or 'FALSE'.  

* If ENABLE_PYTHON is set to be 'TRUE',  
PYTHONLIB and PYTHONINCLUDE must be set appropriately (if ENABLE_PYTHON is 'FALSE', these two variables will be ignored).  
The setting could be,  
```
set(PYTHONLIB /opt/cray/pe/python/3.9.13.1/lib/libpython3.so)
set(PYTHONINCLUDE /opt/cray/pe/python/3.9.13.1/include/python3.9)
```
This shows an example of using a central Python module on the ARCHER 2.  
* Or one could use a custom Python environment (e.g., miniconda) and this case will be,  
```
set(PYTHONLIB /work/e05/e05/wkjee/miniconda3/lib/libpython3.11.so)
set(PYTHONINCLUDE /work/e05/e05/wkjee/miniconda3/include/python3.11)
```
If you are using a custom environment, make sure to set library path correctly in your job script,  
(which is for telling a compute node where the library is located and activating the environment)
```
  $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path_to_your_python_lib/
  $ source PathToYourPythonDir/bin/activate PathToYourPythonDir/envs/YourPythonEnv
```

STEP 2. Build  
Once the settings in ```CMakeLists.txt``` is done, please follow the commands to build,  
```
mkdir _build && cd _build
cmake ..
make
```
and this will generate an executable ```klmc3.XXXX.x``` in the ```_build``` directory.  

----
#### Run Examples

1. Launching GULP tasks in parallel (on ARCHER 2)

* In this git-repot, there is an example prepared in ```/root/Example/testrun_template_gulp```.  
  
In the directory, you can find ```taskfarm.config``` file, which is essential, describing a configurtion of how GULP tasks will be parallelised.  

To launch GULP tasks in parallel, the program requires pre-prepared input files, which could be found in the path ```/root/Example/TestRunGinFiles```.  

In the directory, there are a few sets of KLMC generated GULP input files, named as A0.gin - A199.gin.  
  
In this case, let's use input files in ```CeOrun200```, and this must be relocated to the directory where ```taskfarm.config``` file located.  
```
  $ cp -r /root/Example/TestRunGinFiles/CeOrun200 /root/Example/testrun_template/run
```
Or instead, if the file size is too big, you can use symbolic link,  
```
  $ ln -s /root/Example/TestRunGinFiles/CeOrun200 /root/Example/testrun_template/run
```
Make sure to change the name of directory containing GULP input files to 'run', otherwise the program cannot find GULP input files.  

Once everything is set, submitting the given SLURM job script,  
(make sure to do relevant modification before it's submission).  
```
  $ sbatch SLURM_js.slurm
```

By default, one can find some information from the given jobscript and ```taskfarm.config```  
where 4 nodes (512 cores on ARCHER 2) are requested and each workgroup will use 32 processors.  
This means 4 workgroups will be spwaned on each node, and in total 16 workgroups will launch GULP independently until requested number of GULP calculations are finished.  
(since in ```taskfarm.config```, 'task_start 100' and 'task_end 144' are specified, the GULP inputfiles (stored in 'run') from A100.gin to A144.gin will be used).  

2. Launching Python tasks in parallel (on ARCHER 2)

* In this git-repot, there is an example prepared in ```/root/Example/testrun_template_python```.  

```taskfarm.config```, again which is an essential input file, describing a configurtion of how tasks will be processed in parallised.  
In the file, please try to set correct path to your python_module_path, python_module_name and python_function/method.  
Note. in this version (07.2024), the program only support single processor python script, therefore you must set 'cpus_per_worker' equal to 1.   
Otherwise you may waste computing resource and possibly get unexpected outputs.   

