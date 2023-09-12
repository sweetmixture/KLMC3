### Task farm interface (basic framework) for Knowledge Led Master Code <KLMC>  
* * *
The compilation of this program requires KLMC3-libgulp (libridised version of GULP 6.1.2),  
which can be found in the following link: https://github.com/sweetmixture/KLMC3-libgulp-6.1.2/tree/main
* * *

09.23 update
* Current capability of this program is it is able to launch multiple number of GULP calculations in parallel,
targeting to launch tens of thousands GULP calculations across multiple nodes on HPCs, based on 'master-worker' or 'server-client' algorithm.  
The program is capable of reading KLMC generated GULP inputfiles and launch GULP (run-time GULP inputfile generation is not supported yet).  

### Build (compilation)

* The program requires KLMC3-libgulp, mentioned earlier at the beginning, and see the link above for it's build.  
Assuming the build of KLMC3-libgulp is finished, following the steps to build the program.  

* The following example comes with assumption of using cray-gnu compiler on ARCHER 2.  
Therefore, make sure to load relevant modules before it's build. For instance you type commands,  
```
  $ module restore
  $ module load PrgEnv-gnu
```
to setup your environment.  

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

* To build the program, following the commands,  
```
  $ mkdir _build_gnu && cd _build_gnu
  $ cmake ..
  $ make

```
and this will generate an executable ```tf.x``` in the _build_gnu directory.  


### Run Examples

* On ARCHER2, to carray out a test execution of ```tf.x```, there is an example prepared in ```/root/Example/testrun_template```.
  
In the path, one can find a file,
  
(1) ```taskfarm.config```, which is an essential input file, describes configurtion of task-farm and tasks.  

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


