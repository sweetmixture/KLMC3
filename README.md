### Knowledge Led Master Code 3 (KLMC3)

**Compilation Dependency:**  
This program relies on GULP 6.1.2 static library, which can be fount [here](https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main).  
**Updates (as of 07.24):**  
development log, see'update.list' in the root directory.

#### Program Capabilities
1. Parallel GULP Calculations
- Launch multiple GULP tasks in parallel (large scale GULP tasks across multiple nodes on HPCs, using the 'master-worker' method.  
- Capable of reading KLMC-generated GULP input files and launching GULP (run-time GULP input file generation is not supported yet).  
2. Launching Python Tasks
- Launch multiple Python (single processor) tasks in parallel.
- Each execution of the script occurs within its own directory, similar to using GULP mode.  
- Users are responsible to take care of their Python scripts.  

#### Build
1. Default Mode: launching GULP tasks in parallel
- Make sure to keep the GULP static library [here](https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main).  
- Make sure to keep the same system environment as used for the static GULP library.  

Once you clone this git-repo on your system, you can find 
* Once environment setting is done, locate relevant CMakeLists.txt in the root directory. For instance, while being in the root directory,  
```
  $ cp cmakelist_examples/CMakeLists.txt.cray_gnu CMakeLists.txt
```
In the directory, cmakelist_examples, there are three examples prepared for different system environments,  
```
(1) CMakeLists.txt.cray_gnu -> used in this example  
(2) CMakeLists.txt.cray_cce (e.g. using cray-cce compiler on ARCHER 2)  
(3) CMakeLists.txt.intel (e.g. using intel compiler on YOUNG, KATHELEEN)  
```
Here, we are going to use (1) CMakeLists.txt.cray_gnu, however, one can choose a relevant one for the given system enviornment.  

* IMPORTANT: after locating CMakeLists.txt in the root, now it is necessary to set the correct path of 'KLMC3-libgulp', which should be done by modifying the line,  
```
set(GULPROOT /path/to/KLMC3-libgulp-6.1.2)
```

* IMPORTANT: if you are planning to use the python interface go to 1.2  

1.1 To build the program, following the commands,  
```
  $ mkdir _build_gnu && cd _build_gnu
  $ cmake ..
  $ make
```
and this will generate an executable ```tf.x``` in the _build_gnu directory.  

1.2 To build the program, including the functionality of using python interface,

* make sure to set library path correctly, e.g.,
```
  $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path_to_your_python_lib/
```
if you are using miniconda3 on your linux account, this could be done by  
```
  $export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${CONDA_PREFIX}/lib/
```
if you want to make this change permanently, you can put that command in your ~/.bashrc  

* once the python library path set, following the command,
```
  $ mkdir _build_gnu && cd _build_gnu
  $ cmake -DUSE_PYTHON=ON ..
  $ make
```
and this will generate an executable ```tf.x``` in the _build_gnu directory.  




### Run Examples

1. Lanching Example GULP taskfarming  

* On ARCHER2, to carry out a test execution of ```tf.x```, there is an example prepared in ```/root/Example/testrun_template_gulp```.
  
In the path, one can find a file,
  
```taskfarm.config```, which is an essential input file, describes configurtion of task-farm and tasks.  

For the run, the program needs pre-prepared input files, which could be found in the path ```/root/Example/TestRunGinFiles```.  
In the directory, two sets of KLMC generated input files, each containing 200 GULP input files, named with A0.gin - A199.gin.  
  
In this case, let's use input files in ```CeOrun200```, and this must be relocated to the directory where ```taskfarm.config``` file located.  
```
  $ cp -r /root/Example/TestRunGinFiles/CeOrun200 /root/Example/testrun_template/run
```
Note that make sure to change the name of directory containing GULP input files to 'run', otherwise the program cannot find GULP input files.  

Once everything is set, submitting the given SLURM job script,  
```
  $ sbatch SLURM_js.slurm
```
(make sure to do relevant modification befor it's submission).  

By default, one can find some information from the given jobsript and taskfarm.config where 4 nodes (512 cores) are requested and each workgroup will use 32 processors.  
This means that 4 workgroups will be spwaned on each node, and in total 16 workgroups will launch GULP independently until requested number of GULP calculations are finished.  
Since 'task_start 100' and 'task_end 144' are specified, the GULP inputfiles (stored in 'run') from A100.gin to A144.gin will be used.  

2. Lanching Example Python taskfarming  

* On ARCHER2, to carry out a test execution of ```tf.x```, there is an example prepared in ```/root/Example/testrun_template_python```.

```taskfarm.config```, which is an essential input file, describes configurtion of task-farm and tasks.  
In the file, please try to set correct path to your python_module_path, python_module_name and python_function/method.  
Note. in this version (11.2023), the program only support single processor python script only, therefore you must set 'cpus_per_worker' equal to 1.  
Otherwise you may waste significant computing resource and get unexpected outputs.  


```SLURM_js.slurm``` is a slurm jobscript. Importantly, there are two environmental variables that you must set (i.e., you have to modify this for yourself),

2.1 export your python environment to your computing node. This action corresponds to a line similar to the following,  
```
  $source PathToYourPythonDir/bin/activate PathToYourPythonDir/envs/YourPythonEnv
```
2.2 export your python library path to your computing node, This action corresponds to a line similar to the following,
```
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:PathToYourPythonDir/miniconda3/lib/
```


