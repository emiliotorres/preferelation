// Emilio Torres Manzanera
// University of Oviedo
// Time-stamp: <2020-08-14 19:16 emilio on emilio-XPS-15-9570>
// ============================================================

#include "order_matrix.h"


// ============================================================
 
struct CMP_ROWS_M_INT {
  int* m; // m matrix
  int* cols; // cols: vector of columns
  int ncols;
  CMP_ROWS_M_INT(int* m, int* cols, int ncols): m(m), cols(cols), ncols(ncols) {};
  bool operator()(int row_1, int row_2) const { //a<b ?
	for(int j=0; j < ncols; j++ ){
	  if(  *(m + row_1 + cols[j] ) != *(m + row_2  + cols[j]) ) {
		return *(m + row_1 + cols[j] ) < *(m + row_2  + cols[j]);
	  } 
	}
	// Now, row1 and row2 are equal. Now we sort by rows row_1 and row_2
	return false; // a==b, and row_1 > row_2, so a<b is false
  }; // a < b
};


struct CMP_ROWS_M_REAL {
  double* m;
  int* cols;
  int ncols;
  CMP_ROWS_M_REAL(double* m,  int* cols, int ncols): m(m), cols(cols), ncols(ncols) {};
  bool operator()(int row_1, int row_2) const { // a < b?
	for(int j=0; j < ncols; j++ ){
	  //double x = *(start + row_1 + cols[j] );
	  //double y = *(start + row_2 + cols[j] );
	  int compare= compare_etm_real(m + row_1 + cols[j], m + row_2 + cols[j] );
	  if(compare != 0) {
		return compare < 0;
	  }
	}
	return false; // a==b, and row_1 > row_2, so a<b is false
  }; 
};



// ============================================================
// ============================================================

// set.seed(123)
// R <- matrix(sample(c(NA,1:10),16,TRUE),ncol=4)
// print(R)

// filasfiltro <- sort(unique(sample(1:nrow(R),4)))
// oo <- order(R[filasfiltro,1],R[filasfiltro,2],R[filasfiltro,3],R[filasfiltro,4],na.last=FALSE)
// oocpp <- .Call("order_matrix_cpp",filasfiltro,R,1:ncol(R))
// all.equal(oo,oocpp)


extern "C" SEXP order_matrix_cpp( SEXP m,  SEXP cols, SEXP rows){

  // We will order the rows rows of the matrix m using the cols columns
  // rows: a integer vector, within the range of 1:nrow(m)
  // m: a matrix
  // cols: a integer vector, within the range of 1:ncol(m)
  // It returns a integer vector: rows ordered.
  
  if(!Rf_isMatrix(m))
    Rf_error("M must be a matrix");
  SEXP dimx = Rf_getAttrib( m, R_DimSymbol ) ;
  int nrowx = INTEGER(dimx)[0];
  int ncolx = INTEGER(dimx)[1];

  if (!Rf_isVectorAtomic(cols) || TYPEOF(cols) != INTSXP || LENGTH(cols) < 1)
    Rf_error("COLS must be an integer atomic vector: '%s', length %d", Rf_type2char(TYPEOF(cols)), LENGTH(cols));
  const int *pcols = INTEGER(cols);
  const int l_pcols= LENGTH(cols);
  R_len_t *pos_columns = (R_len_t*) (R_alloc(l_pcols,sizeof(R_len_t)));
  for(R_len_t i=0; i< l_pcols; i++ ){
	if(*(pcols + i) < 1 || *(pcols + i) > ncolx || ISNAN(*(pcols +i)))
	  Rf_error("COLS has a non valid value at position %d: %d. It mus be between 1 and the number of columns of M, %d",i+1,*(pcols+i),ncolx);
    pos_columns[i] = nrowx*( *(pcols + i)-1); // Shift
  }

  if (!Rf_isVectorAtomic(rows) || TYPEOF(rows) != INTSXP || LENGTH(rows) < 1 || LENGTH(rows) > nrowx)
	Rf_error("ROWS must be an integer atomic vector of length inferior to the rows of M (%d): '%s', length %d.",nrowx, Rf_type2char(TYPEOF(rows)), LENGTH(rows));
	 
  int protecti=0;
  
  // We will use rows as new index
  rows = PROTECT(Rf_duplicate(rows)); protecti++;
  R_len_t length_rows = LENGTH(rows);
  int* prows = INTEGER(rows);
  for(R_len_t i=0; i<length_rows; i++) {
	if( *(prows + i) < 1 || *(prows + i) > nrowx || ISNAN(*(prows +i)))
	  Rf_error("ROWS has no valid value at position %d (%d). It must be between 1 and the rows of M, %d.", i+1, *(prows + i), nrowx);
	*(prows + i) = *(prows + i)- 1; // Shift
  } 

  switch(TYPEOF(m))
	{
	case INTSXP:
	case LGLSXP:
	  {
		int *mxpoint = INTEGER(m) ;
		// C++: algorithm. In C use qsort, but it is very slow!!
		std::stable_sort(prows, prows + length_rows, CMP_ROWS_M_INT(mxpoint, pos_columns, l_pcols));
		for(R_len_t i=0; i<length_rows; i++) *(prows + i) = *(prows + i) + 1; // Shift
		break;
	  }
	  
	case REALSXP:
	  {
		double *mxpoint = REAL(m) ;
		// C++: algorithm. In C use qsort
		std::stable_sort(prows, prows + length_rows, CMP_ROWS_M_REAL(mxpoint,pos_columns, l_pcols));
		for(int i=0L; i<length_rows; i++) *(prows + i) = *(prows + i) + 1;
		break;
	  }
	  
	default:
	  UNPROTECT(protecti);
      Rf_error("Unsupported type for M ('%s')",Rf_type2char(TYPEOF(m)));
      return R_NilValue;
	};

  UNPROTECT(protecti);
  return rows;
}
