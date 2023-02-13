#include <Python.h>
#include <stdio.h>
#include <string.h>

void py_setup() {
    wchar_t *path;
    char *py_base;
    char *py_base_raw;
    char *py_pre;
    char search_path[1024];

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
}

// take a comma separated list of UCI moves and return a FEN string
char* ucis_to_fen(const char* ucis) {
    PyObject *pModule, *pFunc, *pArgs, *pValue;
    char *result;

    py_setup();
    pModule = PyImport_ImportModule("chess_helpers");
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

// take a comma separated list of SAN moves and return a comma separated list of UCI moves
char* sans_to_ucis(const char* sans) {
    PyObject *pModule, *pFunc, *pArgs, *pValue;
    char *result;

    py_setup();

    pModule = PyImport_ImportModule("chess_helpers");
    pFunc = PyObject_GetAttrString(pModule, "sans_to_ucis");
    pArgs = PyTuple_Pack(1, PyUnicode_FromString(sans));
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

// take a comma separated list of UCI moves, a string color, an int depth, and an int move_count, and return JSON of ucis and scores
char* ucis_to_engine_candidates(
  const char* ucis,
  const int depth,
  const int move_count
) {
    PyObject *pModule, *pFunc, *pArgs, *pValue;
    char *result;

    py_setup();

    pModule = PyImport_ImportModule("chess_helpers");
    pFunc = PyObject_GetAttrString(pModule, "ucis_to_engine_candidates");
    pArgs = PyTuple_Pack(
      3,
      PyUnicode_FromString(ucis),
      PyLong_FromLong(depth),
      PyLong_FromLong(move_count)
    );
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
