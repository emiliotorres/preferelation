// [[Rcpp::plugins(cpp11)]]
#ifndef PAQUETE_H
#define PAQUETE_H


#include <R.h>
//#include <Rcpp.h>
#define R_NO_REMAP
#include <Rinternals.h>
#include <Rdefines.h>




#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


// using std::vector;
// using std::cout;
// using std::endl;


#ifdef _OPENMP
#include <omp.h>
#include <pthread.h>
#endif

// ============================================================

#define IDX(i,j,dim0) ( (i) + (j) * (dim0) )

// ============================================================

// openmp-utils.cpp
extern "C" int getDTthreads();
extern "C" SEXP getDTthreads_R(SEXP verbose);
extern "C" SEXP setDTthreads(SEXP threads);
extern "C" void when_fork();
extern "C" void avoid_openmp_hang_within_fork(); // see openmp-utils.cpp


// ============================================================





// calculartransitividad.cpp
extern "C" SEXP get_transitivity_cpp(SEXP x);
extern "C" SEXP get_upper_bound_transitivity_cpp(SEXP x);



// ============================================================

// ============================================================




#endif
