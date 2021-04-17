#ifndef CALCULARTRANSITIVIDAD_CPP
#define CALCULARTRANSITIVIDAD_CPP

#include "paquete.h"




// ============================================================

extern "C" int get_transitivity_cpp_int(int *px, int n){
  int result = 0;
  for(R_len_t i = 0; i < n ; i++){
    for(R_len_t k = 0; k < n ; k++){
      if( i != k) {
        R_len_t ik = IDX(i,k,n);
        for(R_len_t j = 0; j < n ; j++){
          if( j!=i && j!=k) {
            if( px[ik] >= px[ IDX(i,j,n)] || px[ik] >= px[ IDX(j,k,n)]) {
              result++;
            }
          }
        }
      }
    }
  }
  return result;
}

// ============================================================

extern "C" int get_transitivity_cpp_real(double *px, int n){
  int result = 0;
  for(R_len_t i = 0; i < n ; i++){
    for(R_len_t k = 0; k < n ; k++){
      if( i != k) {
        R_len_t ik = IDX(i,k,n);
        for(R_len_t j = 0; j < n ; j++){
          if( j!=i && j!=k) {
            if( px[ik] >= px[ IDX(i,j,n)] || px[ik] >= px[ IDX(j,k,n)]) {
              result++;
            }
          }
        }
      }
    }
  }
  return result;
}



// ============================================================


extern "C" SEXP get_transitivity_cpp(SEXP x){

  if(!Rf_isMatrix(x)) Rf_error("Object is not a matrix.");

  SEXP dims = Rf_getAttrib(x, R_DimSymbol);
  R_len_t n = INTEGER(dims)[0]; //nrow
  R_len_t ncol = INTEGER(dims)[1];

  if(n!= ncol) Rf_error("Object is a matrix of diferent dimensions: %d %d",n,ncol);

  //Rprintf("Numero de filas %d \n",n);

  R_len_t result = 0;
  int protecti=0;

  switch(TYPEOF(x))
    {
    case INTSXP:
      {
        //  Rprintf("dentro de integer \n");

        int *px =INTEGER(x) ;
        result = get_transitivity_cpp_int(px,  n);

        break;
      }

    case REALSXP:
      {

        double *px =REAL(x) ;
        result = get_transitivity_cpp_real(px,  n);
        break;

      }

    default:
      UNPROTECT(protecti);
      Rf_error("Unsupported type for matrix.");
      return R_NilValue;
    }



  // SEXP ans;
  // PROTECT(ans = allocVector(INTSXP, 1)); protect++;
  // INTEGER(ans)[0] = result;
  // UNPROTECT(protect);
  // return ans;

  //Rprintf("en el return \n");

  UNPROTECT(protecti);
  return Rf_ScalarInteger(result);

}


// ============================================================

extern "C" int get_upper_bound_transitivity_cpp_int(int *px,int n){
  int result = n*(n-1)*(n-2);
  for(R_len_t i = 0; i < n ; i++){
    for(R_len_t k = 0; k < n ; k++){
      if( i != k) {
        R_len_t ik = IDX(i,k,n);
        if( px[ik] != NA_INTEGER) {
          for(R_len_t j = 0; j < n ; j++){
            if( j!=i && j!=k) {
              if( px[ IDX(i,j,n)] != NA_INTEGER &&  px[ IDX(j,k,n)] != NA_INTEGER ) {
                if( px[ik] < px[ IDX(i,j,n)] && px[ik] < px[ IDX(j,k,n)]) {
                  result--;
                }
              }
            }
          }
        }
      }
    }
  }
  return result;
}


// ============================================================
extern "C" int get_upper_bound_transitivity_cpp_real(double *px,int n){
  int result = n*(n-1)*(n-2);
  for(R_len_t i = 0; i < n ; i++){
    for(R_len_t k = 0; k < n ; k++){
      if( i != k) {
        R_len_t ik = IDX(i,k,n);
        if( !R_IsNA(px[ik])) {
          for(R_len_t j = 0; j < n ; j++){
            if( j!=i && j!=k) {
              if( !R_IsNA(px[ IDX(i,j,n)]) &&  !R_IsNA(px[ IDX(j,k,n)]) ) {
                if( px[ik] < px[ IDX(i,j,n)] && px[ik] < px[ IDX(j,k,n)]) {
                  result--;
                }
              }
            }
          }
        }
      }
    }
  }
  return result;
}

// ============================================================

extern "C" SEXP get_upper_bound_transitivity_cpp(SEXP x){

  if(!Rf_isMatrix(x)) Rf_error("Object is not a matrix.");

  SEXP dims = Rf_getAttrib(x, R_DimSymbol);
  R_len_t n = INTEGER(dims)[0]; //nrow
  R_len_t ncol = INTEGER(dims)[1];

  if(n!= ncol) Rf_error("Object is a matrix of diferent dimensions: %d %d",n,ncol);

  //Rprintf("Numero de filas %d \n",n);

  R_len_t result = n*(n-1)*(n-2);
  int protecti=0;

  switch(TYPEOF(x))
    {
    case INTSXP:
      {
        //  Rprintf("dentro de integer \n");

        int *px =INTEGER(x) ;

        result = get_upper_bound_transitivity_cpp_int(px, n);

        //Rprintf("pasamos px \n");
        break;
      }

    case REALSXP:
      {

        double *px =REAL(x) ;
        result = get_upper_bound_transitivity_cpp_real(px, n);
        break;

      }

    default:
      UNPROTECT(protecti);
      Rf_error("Unsupported type for matrix.");
      return R_NilValue;
    }



  // SEXP ans;
  // PROTECT(ans = allocVector(INTSXP, 1)); protect++;
  // INTEGER(ans)[0] = result;
  // UNPROTECT(protect);
  // return ans;

  //Rprintf("en el return \n");

  UNPROTECT(protecti);
  return Rf_ScalarInteger(result);

}


// ============================================================


extern "C" SEXP get_function_transitivity_matrix_points_cpp(SEXP x, SEXP placesna, SEXP mpoints, int tipotransitividad){

  if(!Rf_isMatrix(x)) Rf_error("X is not a matrix.");
  SEXP dims = Rf_getAttrib(x, R_DimSymbol);
  R_len_t n = INTEGER(dims)[0]; //nrow
  R_len_t ncol = INTEGER(dims)[1];
  if(n!= ncol) Rf_error("X is a matrix of diferent dimensions: %d %d",n,ncol);

  int protecti=0;



  if (!Rf_isVectorAtomic(placesna) || TYPEOF(placesna) != INTSXP)
    Rf_error("PLACESNA must be an integer atomic vector.");
  int lplacesna1 = LENGTH(placesna);
  int* pplacesna1 = INTEGER(placesna);

  // Convertimos los places de 1,2,...,n a 0,1,2,,n-1
  SEXP placesna0 = PROTECT(Rf_allocVector(TYPEOF(placesna),lplacesna1)); protecti++;
  int lplacesna = LENGTH(placesna0);
  int* pplacesna = INTEGER(placesna0);
  int tempint = n*n;
  for(R_len_t l =0; l < lplacesna; l++ ){
    pplacesna[l] = pplacesna1[l] - 1;
    if(pplacesna[l] < 0 || pplacesna[l]>= tempint ){
      Rf_error("PLACESNA has an incorrect position: %d.", pplacesna[l] + 1);
    }
  }


  if(!Rf_isMatrix(mpoints)) Rf_error("MPOINTS is not a matrix.");

  if(TYPEOF(x)!=TYPEOF(mpoints))                                              \
    Rf_error("X and MPOINTS are of different types: '%s' and '%s', respectively.",
             Rf_type2char(TYPEOF(x)),Rf_type2char(TYPEOF(mpoints)));

  if(lplacesna !=Rf_ncols(mpoints)){
    Rf_error("PLACESNA and MPOINTS are of different length: %d places and %d columns, respectively.",
             lplacesna,Rf_ncols(mpoints));
  }

  R_len_t nrowsmpoints = Rf_nrows(mpoints);

  //Rf_PrintValue(y);

  if(nrowsmpoints < 1){
    Rf_error("MPOINTS has  %d rows.", nrowsmpoints);
  }




  SEXP result = PROTECT(Rf_allocVector(INTSXP,nrowsmpoints)); protecti++;
  int* presult= INTEGER(result);


  switch(TYPEOF(x))
    {
	case LGLSXP:
    case INTSXP:
      {
        //  Rprintf("dentro de integer \n");
        int* pmpoints = INTEGER(mpoints);
        int* px = INTEGER(x);

#pragma omp parallel shared(presult, pmpoints,x)
        {
          // Creamos una copia local de x

          int *px_parallel = (int*) (malloc(n*n*sizeof(int)));
          for(R_len_t i = 0; i < n*n ; i++)  px_parallel[i] = px[i];

          switch(tipotransitividad)
            {
            case 1: // Transitividad de matriz completa
              {
#pragma omp for schedule(static,1)
                for(R_len_t i = 0 ; i < nrowsmpoints; i++){
                  // FAlla          Rprintf("Thread %d iteration %d\n", omp_get_thread_num(),i);
                  //std::cout<<"Hilo"<<omp_get_thread_num()<<" "<<i<<std::endl;

                  for(R_len_t il = 0; il < lplacesna; il++){
                    px_parallel[ pplacesna[il] ] = pmpoints[ IDX(i,il,nrowsmpoints)];
                  }
                  //Rf_PrintValue(y);
                  presult[i] = get_transitivity_cpp_int(px_parallel, n);
                  // INTEGER(functiontransitivity(y))[0];
                }

                break;
              }
            case 2: // La Cota superior de una matriz
              {
#pragma omp for schedule(static,1)
                for(R_len_t i = 0 ; i < nrowsmpoints; i++){
                  // FAlla          Rprintf("Thread %d iteration %d\n", omp_get_thread_num(),i);
                  //std::cout<<"Hilo"<<omp_get_thread_num()<<" "<<i<<std::endl;

                  for(R_len_t il = 0; il < lplacesna; il++){
                    px_parallel[ pplacesna[il] ] = pmpoints[ IDX(i,il,nrowsmpoints)];
                  }
                  //Rf_PrintValue(y);
                  presult[i] = get_upper_bound_transitivity_cpp_int(px_parallel, n);
                  // INTEGER(functiontransitivity(y))[0];
                }
                break;
              }
            default:
              free(px_parallel);
              UNPROTECT(protecti);
              Rf_error("No existe esta opción, ¡Burro!.");
            }

          free(px_parallel);
        }// parallel

        break;
      }
    case REALSXP:
      {

        double* pmpoints = REAL(mpoints);
        double* px = REAL(x);

#pragma omp parallel shared(presult, pmpoints,x)
        {
          // Creamos una copia local de x

          double *px_parallel = (double*) (malloc(n*n*sizeof(double)));
          for(R_len_t i = 0; i < n*n ; i++)  px_parallel[i] = px[i];

          switch(tipotransitividad)
            {
            case 1: // Transitividad de matriz completa
              {
#pragma omp for schedule(static,1)
                for(R_len_t i = 0 ; i < nrowsmpoints; i++){
                  // FAlla          Rprintf("Thread %d iteration %d\n", omp_get_thread_num(),i);
                  //std::cout<<"Hilo"<<omp_get_thread_num()<<" "<<i<<std::endl;

                  for(R_len_t il = 0; il < lplacesna; il++){
                    px_parallel[ pplacesna[il] ] = pmpoints[ IDX(i,il,nrowsmpoints)];
                  }
                  //Rf_PrintValue(y);
                  presult[i] = get_transitivity_cpp_real(px_parallel, n);
                  // INTEGER(functiontransitivity(y))[0];
                }

                break;
              }
            case 2: // La Cota superior de una matriz
              {
#pragma omp for schedule(static,1)
                for(R_len_t i = 0 ; i < nrowsmpoints; i++){
                  // FAlla          Rprintf("Thread %d iteration %d\n", omp_get_thread_num(),i);
                  //std::cout<<"Hilo"<<omp_get_thread_num()<<" "<<i<<std::endl;

                  for(R_len_t il = 0; il < lplacesna; il++){
                    px_parallel[ pplacesna[il] ] = pmpoints[ IDX(i,il,nrowsmpoints)];
                  }
                  //Rf_PrintValue(y);
                  presult[i] = get_upper_bound_transitivity_cpp_real(px_parallel, n);
                  // INTEGER(functiontransitivity(y))[0];
                }
                break;
              }
            default:
              free(px_parallel);
              UNPROTECT(protecti);
			  Rf_error("Unsupported type for matrix '%s'",Rf_type2char(TYPEOF(x)));
            }

          free(px_parallel);
        }// parallel

        break;

      }

    default:
      UNPROTECT(protecti);
      Rf_error("Unsupported type for matrix.");
      return R_NilValue;
    }


  //  Rprintf("en el return \n");


  UNPROTECT(protecti);
  return result;

}


// ============================================================

extern "C" SEXP get_upper_bound_transitivity_mpoints_cpp(SEXP x, SEXP placesna, SEXP mpoints){
  return get_function_transitivity_matrix_points_cpp(x, placesna,  mpoints, 2);
 
}


// ============================================================

extern "C" SEXP get_transitivity_mpoints_cpp(SEXP x, SEXP placesna, SEXP mpoints){
  return get_function_transitivity_matrix_points_cpp(x, placesna,  mpoints, 1);
}



#endif
