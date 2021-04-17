##' .. content for 
##'
##' .. content for 
##' @title Compute the transitivity of R filled with the rows of mpoints
##' @param R 
##' @param mpoints a matrix. It cannot have missing data. Number of columns must be the number of missing values in R.
##' @return 
##' @author emilio
##' @export
calcular_transitividad_matriz_puntos <-
function(R,mpoints){
    R <- chequear_matriz(R)
    if(anyNA(mpoints)){
        stop("Hay datos perdidos en la matriz de puntos. Usa la funcion cota maxima.")
    }
    if(class(R[1]) != class(mpoints[1])){
        R <- 1.0*R
        mpoints <- 1.0*mpoints
    }
    huecosna <- seq_along(R)[is.na(R)]
    get_transitivity_mpoints(R,huecosna,mpoints)
}
