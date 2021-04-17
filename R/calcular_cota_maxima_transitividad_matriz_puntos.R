##' .. content for 
##'
##' .. content for 
##' @title Calculate the upper bound of R with filled with each row of mpoints
##' @param R 
##' @param mpoints a matrix with ncol equals to the missing data. It may contains missing values.
##' @return 
##' @author emilio
##' @export
calcular_cota_maxima_transitividad_matriz_puntos <-
function(R,mpoints){
    R <- chequear_matriz(R)
    if(class(R[1]) != class(mpoints[1])){
        R <- 1.0*R
        mpoints <- 1.0*mpoints
    }
    huecosna <- seq_along(R)[is.na(R)]
    get_upper_bound_transitivity_mpoints(R,huecosna,mpoints)
}
