#=================================================================#
# Top level Makefile 
#=================================================================#

#
# This build system requires GNU make.
#

###################################################################

# Selects an optimised or debug build : debug, intermediate, fast

#F90 := gfortran
F90 := ftn
#BUILD := debug 
BUILD := intermediate
#gfortran

# Sets the communications architecture: Options are serial,omp,mpi

COMMS_ARCH := serial

###################################################################
# Shouldn't need to change anything below here
###################################################################
#
# Strip any extraneous trailing spaces.
#
COMMS_ARCH := $(strip $(COMMS_ARCH))
BUILD      := $(strip $(BUILD))

ROOTDIR = $(PWD)

EXEC := klmc 

#GULP_LIB := -L/home/matthew/software/GULP/4.0/ -lgulp -L/home/matthew/software/GULP/4.0/Src/FoX-4.1.2/objs/lib \
-Wl,--start-group -lFoX_common -lFoX_utils -lFoX_wxml -lFoX_wcml -lFoX_fsys -Wl,--end-group

#LIBS := -L/opt/intel/fce/11.1/073/mkl/lib/em64t -Wl,--start-group -lmkl_core -lmkl_sequential -lmkl_intel_lp64 -lmkl_scalapack_lp64 -lmkl_blacs_lp64 -Wl,--end-group -lpthread -L/home/matthew/software/fhi-aims.071711_6/bin/ -laims $(GULP_LIB) 

ifeq ($(COMMS_ARCH),mpi)
F90 := mpif90-ifort
endif

ifeq ($(BUILD), debug)
  ifeq ($(F90), ifort)
    FLAGS := -g -O0 -check all -pg -traceback -DIFC
  else
    FLAGS := -g -C -fbounds-check -Wall -fbacktrace -finit-real=nan
  endif
#FLAGS := -g -C -Wall -fbacktrace -fcheck=all
endif

ifeq ($(BUILD),intermediate)
FLAGS := -O1   
endif

ifeq ($(BUILD),fast)
FLAGS := -O3 #-g -fbounds-check -Wall -fbacktrace -finit-real=nan
endif


OPTIONS := -DXCOMP=$(F90)

#Stop verbose output. 
#.SILENT:

all:     $(EXEC)

$(EXEC): Config.o File.o Format.o Utilities.o Library.o MonteCarlo.o Population.o SCOTT.o UnitCell.o \
	Serial-Comms.o GeneticAlgorithm.o Timer.o Atoms.o BasinHopping.o Testing.o Algebra.o
	$(F90) $(FLAGS) *.o -o $(EXEC) $(COMM_FLAGS) $(LIBS)

Atoms.o: Atoms.f90
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Atoms.f90

Config.o:  Config.f90
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Config.f90

Library.o:  Library.f90 Config.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Library.f90 

UnitCell.o:  UnitCell.f90 Config.o Library.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c UnitCell.f90

Format.o: Format.f90
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Format.f90

Timer.o:  Timer.f90 Config.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Timer.f90 

Algebra.o: Algebra.f90 Config.o Format.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Algebra.f90 

Utilities.o: Utilities.f90 Atoms.o Algebra.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Utilities.f90

ifeq ($(COMMS_ARCH),mpi)
MPI-Comms.o: MPI-Comms.f90 Config.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c MPI-Comms.f90 
else
Serial-Comms.o: Serial-Comms.f90 Config.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Serial-Comms.f90 
endif

ifeq ($(COMMS_ARCH),mpi)
File.o: File.f90 Config.o Library.o UnitCell.o Format.o MPI-Comms.o Timer.o Algebra.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c File.f90 
else
File.o: File.f90 Config.o Library.o UnitCell.o Format.o Serial-Comms.o Timer.o Algebra.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c File.f90 
endif

ClusterRoutines.o: ClusterRoutines.f90 Config.o Library.o UnitCell.o Format.o File.o Grid.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c ClusterRoutines.f90 

Moveclass.o: Moveclass.f90 Config.o Library.o UnitCell.o ClusterRoutines.o Utilities.o Timer.o File.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Moveclass.f90 

ifeq ($(COMMS_ARCH),mpi)
Environment.o:  Environment.f90 Config.o Library.o Format.o MPI-Comms.o File.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Environment.f90 
else
Environment.o:  Environment.f90 Config.o Library.o Format.o Serial-Comms.o File.o Atoms.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Environment.f90 
endif

Population.o: Population.f90 Config.o Library.o Format.o File.o ClusterRoutines.o Moveclass.o Environment.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Population.f90 

Master.o: Master.f90 Config.o Library.o UnitCell.o File.o ClusterRoutines.o Population.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Master.f90 

MonteCarlo.o: MonteCarlo.f90 Config.o Library.o UnitCell.o Format.o File.o \
              ClusterRoutines.o Moveclass.o Population.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c MonteCarlo.f90 

BasinHopping.o: BasinHopping.f90 Config.o Library.o UnitCell.o Format.o File.o \
                ClusterRoutines.o Moveclass.o Environment.o Population.o Master.o MonteCarlo.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c BasinHopping.f90 

Annealing.o: Annealing.f90 Config.o Library.o Format.o File.o ClusterRoutines.o Environment.o \
              Population.o Master.o MonteCarlo.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Annealing.f90 

Thresholds.o: Thresholds.f90 Config.o Library.o Format.o File.o ClusterRoutines.o Environment.o \
              Population.o Master.o MonteCarlo.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Thresholds.f90 

ProductionRun.o: ProductionRun.f90 Config.o Library.o File.o ClusterRoutines.o Environment.o RdfRoutines.o Master.o \
								 GeneticAlgorithm.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c ProductionRun.f90 

RdfRoutines.o: RdfRoutines.f90 Config.o Library.o UnitCell.o File.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c RdfRoutines.f90 

ScanBox.o: ScanBox.f90 Config.o Library.o File.o ClusterRoutines.o Moveclass.o Environment.o \
           MonteCarlo.o Population.o Master.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c ScanBox.f90 

SolidSolution.o: SolidSolution.f90 Config.o Library.o Format.o File.o ClusterRoutines.o \
                 Moveclass.o Environment.o Population.o Master.o MonteCarlo.o RdfRoutines.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c SolidSolution.f90 

RefineSprings.o: RefineSprings.f90 Config.o Library.o UnitCell.o ClusterRoutines.o Population.o Master.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c RefineSprings.f90 

GeneticAlgorithm.o: GeneticAlgorithm.f90 Config.o Library.o UnitCell.o Format.o File.o \
                    ClusterRoutines.o Moveclass.o Environment.o Population.o Master.o Utilities.o Timer.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c GeneticAlgorithm.f90 

Grid.o: Grid.f90 Config.o Format.o Atoms.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Grid.f90

Testing.o: Testing.f90 Config.o Format.o ClusterRoutines.o File.o Utilities.o Grid.o Population.o Master.o Moveclass.o
	$(F90) $(FLAGS) $(COMM_FLAGS) -c Testing.f90


ifeq ($(COMMS_ARCH),mpi)
SCOTT.o: SCOTT.f90 Config.o Library.o Format.o Utilities.o MPI-Comms.o File.o Environment.o \
                   Population.o Master.o BasinHopping.o Annealing.o Thresholds.o ProductionRun.o \
                   RdfRoutines.o ScanBox.o SolidSolution.o RefineSprings.o GeneticAlgorithm.o \
                   Timer.o Atoms.o Testing.o Grid.o Algebra.os
                   
                   
	$(F90) $(FLAGS) $(COMM_FLAGS) -c SCOTT.f90
else
SCOTT.o: SCOTT.f90 Config.o Library.o Format.o Utilities.o Serial-Comms.o File.o Environment.o \
                   Population.o Master.o BasinHopping.o Annealing.o Thresholds.o ProductionRun.o \
                   RdfRoutines.o ScanBox.o SolidSolution.o RefineSprings.o GeneticAlgorithm.o \
                   Timer.o Atoms.o Testing.o Grid.o Algebra.o Testing.o
                   
	$(F90) $(FLAGS) $(COMM_FLAGS) -c SCOTT.f90
endif

rebuild: clean all

clean: 
	@rm -vf *.mod *.o > /dev/null

tar:
	@tar cvf scott_code.tar	        *.f90 \
					Makefile 

