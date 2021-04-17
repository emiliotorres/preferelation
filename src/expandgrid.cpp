#include "paquete.h"

/*
  
Create a grid of length(x)^n, with the values of x
and add (ntotal - n) columns.
Return a matrix with dimensions (length(x)^n , ntotal)
Particular case of expand.grid

x <- 1:3
n <- 4L
ntotal <- 7L

mpoints <- .Call("expand_grid_cpp",x,  n, ntotal)

listainicial <- vector("list", ntotal)
for(i in 1:ntotal){
  if(i <= n) {
 listainicial[[i]]  <- x
                     } else {
 listainicial[[i]]  <- NA
                     }
 }
mpointsR <-as.matrix(expand.grid(listainicial, KEEP.OUT.ATTRS = FALSE))
dimnames(mpointsR) <- NULL

all.equal(mpointsR,mpoints) # TRUE

head(mpoints)
     [,1] [,2] [,3] [,4] [,5] [,6] [,7]
[1,]    1    1    1    1   NA   NA   NA
[2,]    2    1    1    1   NA   NA   NA
[3,]    3    1    1    1   NA   NA   NA
[4,]    1    2    1    1   NA   NA   NA
[5,]    2    2    1    1   NA   NA   NA
[6,]    3    2    1    1   NA   NA   NA
...

*/
  

extern "C" SEXP expand_grid_cpp(SEXP x, SEXP n, SEXP ntotal){

  // X is a vector of integer or real numbers.
  // N and NTOTAL are scalar integers such that 0< N <= NTOTAL.
  // Return a matrix of the permutations with n repetitions of x,
  // and add (ntotal -n ) NA columns.

  if (!Rf_isVectorAtomic(x) || !LENGTH(x))
    Rf_error("X must be an atomic vector with length greater than 0 (%d).", LENGTH(x));

  if(!Rf_isInteger(n) || LENGTH(n) != 1)
    Rf_error("N is either not an integer value ('%s') or its length is not 1 (%d).", Rf_type2char(TYPEOF(n)), LENGTH(n));

  if(!Rf_isInteger(ntotal) || LENGTH(ntotal) != 1)
    Rf_error("NTOTAL is either not an integer value ('%s') or its length is not 1 (%d).", Rf_type2char(TYPEOF(ntotal)), LENGTH(n));

  const R_len_t nrepetitions = INTEGER(n)[0];
  const R_len_t ncolumns = INTEGER(ntotal)[0];

  if( nrepetitions < 1 || ncolumns < nrepetitions || nrepetitions == NA_INTEGER || ncolumns == NA_INTEGER )
    Rf_error("N is '%d', lesser than 1 or greater than the number of columns NCOL (%d).", nrepetitions,ncolumns);

  // Number of combinations. It can become a very, very, very big magnitude.
  const R_len_t lx=LENGTH(x);
  const R_len_t nrows = pow(lx, nrepetitions);

  // BE CAREFUL! ISNAN(nrows) does not work here!
  if(nrows < 0 )
    Rf_error("The number of combinations (%.0f) is too big! ('nrows'). Check it within R: Can you create a 'matrix(NA, nrow= %d^%d, ncol= %d)'?",pow(lx,nrepetitions),  lx, nrepetitions, ncolumns);

  // We create the matrix. If it is too big, it gives the error 'Not enough memory for your dreams'.
  int protecti=0;
  SEXP result = PROTECT(Rf_allocMatrix(TYPEOF(x), nrows , ncolumns)); protecti++;

  // vcount is the count/value for each column.
  // vcount[jcol] = 0,1,2,..., lx-1
  R_len_t *vcount = (R_len_t*) (R_alloc(nrepetitions,sizeof(R_len_t)));
  for(R_len_t i = 0; i < nrepetitions ; i++)  vcount[i] = 0;

  const R_len_t maxcount = lx -1; // The maximum count that can appear in a column
  const R_len_t lastcolumn  = nrepetitions -1; // Last column of the repetition submatrix.
  const R_len_t firstcolumn = 0; // First column.

  switch(TYPEOF(x))
    {
    case LGLSXP:
    case INTSXP:
      {

        const int* px = INTEGER(x);
        int* presult = INTEGER(result);

        // for each row
        for(R_len_t irow = 0; irow < nrows ; irow++){

          // for the columns to fill with values of x
          for(R_len_t jcol = 0; jcol < nrepetitions ; jcol++){

            // When we are at the first column,
            // we decide that a count must vary
            // when the first column gets an excessive count.
            if(vcount[firstcolumn] > maxcount && jcol == firstcolumn ) {

              // Numk is the first position/column which its count/value can be increased.
              // Therefore, the previous columns have the maximum count/value.
              // So we reset the count/values for the previous columns
              // and increase the count/values of the position Numk.
              R_len_t numk = 0;
              for( numk =0;  numk < nrepetitions && vcount[numk] >= maxcount; numk++);
              for(R_len_t k =0;  k < numk; k++){
                vcount[k]=0;
              }

              // If Numk is greater than the last column, we have finished the grid:
              // All the columns have the maximum count/value.
              // Otherwise, increase the value of the column numk
              if(numk < nrepetitions)  vcount[numk]++; // numk <= lastcolumn

            }

            // Fill this position with the value of X that corresponds to
            // the count of this column.
            presult[ IDX(irow,jcol,nrows) ] = px[ vcount[jcol]  ] ;

            // When it is the last column, the count of the first column always increases.
            // It is the signal to reset some columns.
            if(jcol == lastcolumn)  vcount[firstcolumn]++;

          } // End of columns of permutations

          // Fill the others columns with NA
          for(R_len_t jcol = nrepetitions; jcol < ncolumns ; jcol++){
            presult[ IDX(irow,jcol,nrows) ] = NA_INTEGER;
          }
        }

        break;
      }
    case REALSXP:
      {

        const double* px = REAL(x);
        double* presult = REAL(result);

        for(R_len_t irow = 0; irow < nrows ; irow++){

          for(R_len_t jcol = 0; jcol < nrepetitions ; jcol++){

            if(vcount[firstcolumn] > maxcount && jcol == firstcolumn ) {

              int numk = 0;
              for( numk =0;  numk < nrepetitions && vcount[numk] >= maxcount; numk++);
              for(R_len_t k =0;  k < numk; k++){
                vcount[k]=0;
              }

              if(numk < nrepetitions){
                vcount[numk ]++;
              }

            }

            presult[ IDX(irow,jcol,nrows) ] = px[ vcount[jcol]  ] ;
            if(jcol == lastcolumn){
              vcount[firstcolumn]++;
            }
          }

          for(R_len_t jcol = nrepetitions; jcol < ncolumns ; jcol++){
            presult[ IDX(irow,jcol,nrows) ] = NA_REAL;
          }
        }
        break;
      }

    default:
      UNPROTECT(protecti);
      Rf_error("Unsupported type for X ('%s')",Rf_type2char(TYPEOF(x)));
      return R_NilValue;
    }

  UNPROTECT(protecti);
  return result;

}



// ============================================================
// ============================================================
