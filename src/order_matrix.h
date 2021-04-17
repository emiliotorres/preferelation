// Emilio Torres Manzanera
// University of Oviedo
// Time-stamp: <2020-08-14 19:48 emilio on emilio-XPS-15-9570>
// ============================================================

#ifndef ORDER_MATRIX_H
#define ORDER_MATRIX_H


#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>

#include <algorithm> // std::sort
//#include  <iostream>

//#include <Rinternals.h>


#ifdef __cplusplus
#define EXTERN_C_BEGIN extern "C" {
#define EXTERN_C_END   }
#define EXTERN_C       extern "C"
#else
#define EXTERN_C_BEGIN /* Nothing */
#define EXTERN_C_END   /* Nothing */
#define EXTERN_C       extern /* Or Nothing */
#endif /* __cplusplus */

// ============================================================

extern "C" inline int compare_etm_int(const void *ap, const void *bp){
  const int *a = (int *) ap;
  const int *b = (int *) bp;
  if(*a < *b)
    return -1;
  else if(*a > *b)
    return 1;
  else
    return 0;
}

// ============================================================

extern "C" inline int compare_etm_real(const void *ap, const void *bp){
  const double *a = (double *) ap;
  const double *b = (double *) bp;
  int nax = ISNAN(*a), nay = ISNAN(*b);
  if (nax && nay)	return 0;
  else if (nax)		return -1;
  else if (nay)		return 1;
  else if (*a < *b)
    return -1;
  else if(*a > *b)
    return 1;
  else
    return 0;
}





// order_matrix.cpp
extern "C" SEXP order_matrix_cpp(SEXP m, SEXP cols, SEXP rows);


// ============================================================
  
#endif // ORDER_MATRIX_H
