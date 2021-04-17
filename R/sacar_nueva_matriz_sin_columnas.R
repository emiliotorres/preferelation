## #+TITLE: 
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-09-24 10:28 emilio on emilio-despacho>
## #+TAGS: 
## #+PROPERTY: header-args :results output :exports both :session 

##
##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title .
##' @param R r 
##' @param verbose v
##' @return .
##' @author emilio
##' @export 
##' @examples
##' 1
sacar_nueva_matriz_sin_columnas_o_filas_perdidas <- function(R,verbose=1L){
    R <- chequear_matriz(R)
    ndimension <- as.integer(sqrt(length(R)))
    for(j in 1:ndimension){
        valoresperdidosoceros <- R[,j]
        f <- is.na(valoresperdidosoceros) | (!is.na(valoresperdidosoceros) & valoresperdidosoceros == 0L)
        n <- sum(f)
        if(verbose>2L)cat("Columna ",j," número de huecos ",n,".\n")
        if(n == ndimension -1L){
            if(verbose>0L) cat("Relleno con ceros la columna",j,".\n")
            R[f,j]  <- 0L
        } 
    }
    for(j in 1:ndimension){
        valoresperdidosoceros <- R[j,]
        f <- is.na(valoresperdidosoceros) | (!is.na(valoresperdidosoceros) & valoresperdidosoceros == 0L)
        n <- sum(f)
                if(verbose>2L)cat("Fila ",j," número de huecos ",n,".\n")
        if(n == ndimension -1L) {
            if(verbose>0L) cat("Relleno con ceros la fila",j,".\n")
            R[j,f]  <- 0L
        }
    }
    R
}
