##' .. content for  (no empty lines) ..
##'
##' .. content for ..
##' @title Check if it is a relation matrix
##' @param R Matrix
##' @param verbose verbose
##' @return a matrix
##' @author emilio
##' @export
chequear_matriz <- function(R, verbose=0L){
    verbose <- as.integer(verbose)
    if(verbose>2L){
        cat("\nChecking preference matrix...\n")
    }
    len <- length(R)
    if(length(R) < 1){
        stop("R has length inferior to 1:", length(R))
    }
    ndimension <- as.integer(sqrt(len))
    if(ndimension*ndimension != len){
        print(R)
        stop("R is not a squared matrix. The length of R is ", len, " != ", ndimension,"x", ndimension," = ", ndimension^2 )
    }
    if(ndimension < 3L){
        print(R)
        stop("R dimension must be at least 3 and its dimension is ", ndimension)
    }
    if(all(is.na(R))){
        print(R)
        stop("R only contains NA ")
    }
    dim(R) <- c(ndimension,ndimension)
    minimo <- min(R,na.rm=TRUE)
    if(minimo <0) {
        print(R)
        cat(sum(R[R<0 & !is.na(R)]), " values of R that are lesser than 0:", R[R<0])
        stop("R has negative numbers.")
    }
    if(anyNA(diag(R))){
        print(R)
        cat( sum(is.na(diag(R)))," values of diagonal are missing.")
        stop("R has missing values on the diagonal.")
    }
    maximo <- max(R,na.rm=TRUE)
    if(any(diag(R) != maximo)){
        print(R)
        cat( sum(diag(R) != maximo)," values of diagonal are lesser that maximum ", maximo,":", diag(R)[diag(R)!=maximo])
        stop("R has no maximum values on the diagonal.")
    }
    if(is.integer(R)) {
        R  <- 1.0*R
    }
    
    if(verbose>2L){
        cat("Checking preference matrix...done\n")
    }
    R
}
