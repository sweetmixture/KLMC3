/*
    Author:     Woongkyu Jee / woong.jee.16@ucl.ac.uk
    Affiliation:    University College London
    Date:       2023.05.25 - 

    Description:
*/
#ifndef __PYTHON_SERIAL
#define __PYTHON_SERIAL

#include <stdbool.h>
#include <Python.h>

/* from: call_python_serial.c */
bool call_python_serial(
PyObject* pModule,          // Python Module (*.py)                 // IN
const char *method_name     // Python Function within *pModule      // IN
/*
    07.11.23 wkjee
    User responsible function: user can feed relevant python function/method to deploy on the taskfarm
    pModule     : in the upper interface possible format is: PyObject *pModule = PyImport_ImportModule("my_python_module");
    method_name : name of the method/function within the module

    Note. this function is desigend to call a python method/function, which does not accept any *args.
        1. Basic Error check: if 'pModule' exists, else return 'false'
        2. DOES NOT DO if the python call is successful (maybe changed later).
*/
);

/* from: python_serial_initfinal.c */
void python_serial_init(
PyObject** sysPath,         // IN-OUT   : python module path
PyObject** pModule,         // IN-OUT   : python module
const char* module_path,    // IN
const char* module_name     // IN
);

void python_serial_final(
PyObject* sysPath,			// IN
PyObject* pModule			// IN
);


#endif
