/*
	Author:		Woongkyu Jee / woong.jee.16@ucl.ac.uk
	Affiliation:	University College London
	Date:		2023.05.25 - 

	Description:
*/
#include <stdio.h>
#include <Python.h>

void python_serial_init(
	PyObject** sysPath,			// IN-OUT	: python module path
	PyObject** pModule,			// IN-OUT	: python module
	const char* module_path,	// IN
	const char* module_name		// IN
){
	// attach interpreter
	Py_Initialize();

	// python module path setting: module_path - expected to be absolute path
	*sysPath = PySys_GetObject("path");
	PyList_Insert(*sysPath,0,PyUnicode_DecodeFSDefault(module_path));

	// python module loading: module_name - without .py extension but its name
	*pModule = PyImport_ImportModule(module_name);

	// if module loaded
	if( *pModule == NULL ){
		fprintf(stderr,"Error> python module load failed ...\n check out the path/module name: %s | %s\n",module_path,module_name);
		Py_XDECREF(*sysPath);
		Py_Finalize();
	}

	return;
}

void python_serial_final(
	PyObject* sysPath,			// IN
	PyObject* pModule			// IN
){
	// free Pyobjects
	Py_XDECREF(sysPath);
	Py_XDECREF(pModule);
	// release interpreter
	Py_Finalize();
	
	return;
}
