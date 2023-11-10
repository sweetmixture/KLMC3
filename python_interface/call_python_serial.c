/*
	Author:		Woongkyu Jee / woong.jee.16@ucl.ac.uk
	Affiliation:	University College London
	Date:		2023.05.25 - 

	Description:
*/

#include <stdbool.h>
#include <Python.h>

bool call_python_serial(
	PyObject* pModule,			// Python Module (*.py)				// IN
	const char* method_name		// Python Function within *pModule	// IN
){
	/* * *
		load python method/function from pModule (Python Module)

		here no modification on pModule<PyObject*>: simply the pointer object value passed into this scope
	* * */
	PyObject *pMethod = PyObject_GetAttrString(pModule,method_name);
	if (!PyCallable_Check(pMethod)) {
		fprintf(stderr,"Error> 'call_python_serial()' Method/Function Found Err\n");
		PyErr_Print();
		return false;
	}

	/* * *
		example of packing python arguments - kept here for later use (WKJEE, 07.11.2023)
	* * */
	/*
	PyObject *pArgs = PyTuple_Pack(2, PyLong_FromLong(arg1), PyLong_FromLong(arg2));	// Python args 'arg1' 'arg2' packing
	PyObject *pRes = PyObject_CallObject(pMethod, pArgs);								// Call Python method - by default using Tuple *args
	*/

	/* * *
		call python: PyObject *PyObject_CallObject(PyObject *callable, PyObject *args)
		wkjee: 07.11.23 - aiming to develope prototype that carries out independent python calls - push resposibilities to the user (who wrote python script)
							form: return nothing
	* * */
	PyObject_CallObject(pMethod,NULL);

	/* * *
	PyObject* pRes = PyObject_CallObject(pMethod,pArgs);	// pRest -> NULL if fails (default) - not tested if there is genuinely nothing returned case
	if (pRes != NULL) {
		long res = PyLong_AsLong(pRes);
		printf(" -- in python wrapper -- Sum of two integers: %ld\n", res);
		Py_XDECREF(pRes);
	} else {
		PyErr_Print();
	}
	*/

	/* * *
		python object release: preventing memory leaking
	* * */
	Py_XDECREF(pMethod);
	return true;
}

