##' .. content for 
##'
##' .. content for 
##' @title q
##' @param R 
##' @return q
##' @author emilio
##' @export
calcular_cota_maxima_transitividad <-
function(R){
    R <- chequear_matriz(R)
    get_upper_bound_transitivity(R)
}
