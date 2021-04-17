##' .. content for 
##'
##' .. content for 
##' @title Calculate the transitivity of a non empty relationship matrix
##' @param R 
##' @return the transitivity
##' @author emilio
##' @export
calcular_transitividad <-
function(R){
    R <- chequear_matriz(R)
    get_transitivity(R)
}
