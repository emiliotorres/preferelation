#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP expand_grid_cpp(SEXP, SEXP, SEXP);
extern SEXP get_transitivity_cpp(SEXP);
extern SEXP get_transitivity_mpoints_cpp(SEXP, SEXP, SEXP);
extern SEXP get_upper_bound_transitivity_cpp(SEXP);
extern SEXP get_upper_bound_transitivity_mpoints_cpp(SEXP, SEXP, SEXP);
extern SEXP order_matrix_cpp(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"expand_grid_cpp",                          (DL_FUNC) &expand_grid_cpp,                          3},
    {"get_transitivity_cpp",                     (DL_FUNC) &get_transitivity_cpp,                     1},
    {"get_transitivity_mpoints_cpp",             (DL_FUNC) &get_transitivity_mpoints_cpp,             3},
    {"get_upper_bound_transitivity_cpp",         (DL_FUNC) &get_upper_bound_transitivity_cpp,         1},
    {"get_upper_bound_transitivity_mpoints_cpp", (DL_FUNC) &get_upper_bound_transitivity_mpoints_cpp, 3},
    {"order_matrix_cpp",                         (DL_FUNC) &order_matrix_cpp,                         3},
    {NULL, NULL, 0}
};

void R_init_preferelation(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
