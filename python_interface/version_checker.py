#include <Python.h>
#include <stdio.h>

int main() {
    // Initialize the Python interpreter
    Py_Initialize();

    // Get the Python version
    const char* pythonVersion = Py_GetVersion();

    // Print the Python version
    printf("Python Version: %s\n", pythonVersion);

    // Finalize the Python interpreter
    Py_Finalize();

    return 0;
}

