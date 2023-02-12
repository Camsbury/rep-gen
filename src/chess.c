#include <Python.h>
#include <stdio.h>
#include <string.h>

// take a comma separated list of UCI moves and return a FEN string
char* ucis_to_fen(const char* ucis) {
    PyObject *pModule, *pFunc, *pArgs, *pValue;
    char *result;
    wchar_t *path;
    char *py_base;
    char *py_base_raw;
    char *py_pre;
    char search_path[1024];

    // eventually abstract everything out here
    Py_Initialize();
    py_base = getenv("PY_BASE");
    py_base_raw = getenv("PY_BASE_RAW");
    py_pre = getenv("PY_PRE");

    // NOTE: can print(sys.path) to find all these!!

    // skipped the zipped version...
    strcpy(search_path, py_base_raw);
    strcat(search_path, "/lib/");
    strcat(search_path, py_pre);
    strcat(search_path, ":");

    strcat(search_path, py_base_raw);
    strcat(search_path, "/lib/");
    strcat(search_path, py_pre);
    strcat(search_path, "/lib-dynload:");

    strcat(search_path, py_base_raw);
    strcat(search_path, "/lib/");
    strcat(search_path, py_pre);
    strcat(search_path, "/site-packages:");

    strcat(search_path, py_base);
    strcat(search_path, "/lib/");
    strcat(search_path, py_pre);
    strcat(search_path, "/site-packages:");

    strcat(search_path, "./src/");

    path = Py_DecodeLocale(search_path, NULL);
    PySys_SetPath(path);
    PyMem_RawFree(path);

    pModule = PyImport_ImportModule("notation");
    pFunc = PyObject_GetAttrString(pModule, "ucis_to_fen");
    pArgs = PyTuple_Pack(1, PyUnicode_FromString(ucis));
    pValue = PyObject_CallObject(pFunc, pArgs);

    if (pValue != NULL) {
        result = (char*)PyUnicode_AsUTF8(pValue);
    } else {
        PyErr_Print();
        result = NULL;
    }

    Py_Finalize();
    return result;
}
