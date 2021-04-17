## #+TITLE: 
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-09-17 22:18 emilio on emilio-XPS-15-9570>
## #+TAGS: 
## #+PROPERTY: header-args :results output :exports both :session 


##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title Saca los puntos canónicos, representantes de cada clase.
##' @param R una matriz
##' @param mpoints un matriz, normalmente la que sale de 'obtener_puntos_optimos(R)'.
##' @return Una matriz con los puntos canónicos.
##' @author emilio
##' @export 
##' @examples
##' 1
obtener_puntos_canonicos <- function(R,mpoints){
    mr <- matrix(R,nrow(mpoints),ncol=length(R),byrow=TRUE)
    colnames(mr) <- seq_along(R)
    huecosna <- seq_along(R)[is.na(R)]
    mr[,huecosna] <- mpoints
    matrizcomparacionesrecubrimientosr <- hacer_comparaciones(mr)
    frecubrimientosr <- duplicated(matrizcomparacionesrecubrimientosr)
    mpoints[!frecubrimientosr,,drop=FALSE]
    }



hacer_comparaciones <- function(mpoints){
    matrizcomparaciones <- matrix(NA,nrow=nrow(mpoints),ncol=ncol(mpoints)*(ncol(mpoints)-1)/2L)
    l <- 0L
    for(j in 1L:(ncol(mpoints)-1L))
        for(k in (j+1L):ncol(mpoints)){
            l <- l +1L
            ##        print(l)
            matrizcomparaciones[,l]  <- mpoints[,j] - mpoints[,k]
        }
    matrizcomparaciones[matrizcomparaciones>0L]  <- 1L
    matrizcomparaciones[matrizcomparaciones<0L]  <- -1L
    matrizcomparaciones
}
