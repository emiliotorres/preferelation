##' .. content for  (no empty lines) ..
##'
##' .. content for ..
##' @title Create a random integer relation matrix
##' @param ndimension dimension
##' @param valordiagonal the value of the diagonal
##' @return an integer matrix
##' @author emilio
##' @export
crear_matriz_aleatoria <-function(ndimension,valordiagonal=10){

    valordiagonal <- as.numeric(valordiagonal)
    if(valordiagonal < 0 || is.na(valordiagonal)) {
        stop("El valor de la diagonal tiene que ser >= 0, y es", valordiagonal)
    }
    if(ndimension < 3 || is.na(ndimension)) {
        stop("La dimension tiene que ser mayor o igual que 3, y es",ndimension)
    }
    ## Crea una matriz de numeros enteros entre 0L y valordiagonal
    ndatosperdidosinicial <- sample( 1L:(ndimension*ndimension))
    ## Construimos una matriz con los datos que facilita el provedor.
    ## La diagonal es 10.
    valoresentreceroydiez <- 0L:valordiagonal
    R <- sample(valoresentreceroydiez,ndimension*ndimension,replace=TRUE)
    R[sample(seq_along(R),ndatosperdidosinicial)] <- NA
    dim(R) <- c(ndimension,ndimension)
    diag(R) <- valordiagonal
    1.0*R
}
