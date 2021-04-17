## #+TITLE: 
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-08-11 13:08 emilio on emilio-XPS-15-9570>
## #+TAGS: 
## #+PROPERTY: header-args :results output :exports both :session 



##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title Generate a cover of the first missing places of R
##' @param R 
##' @param nhuecos Number of (the first missing) places of R to be filled.
##' @return A matrix
##' @author emilio
##' @export 
##' @examples
##' set.seed(1234)
##' R <- crear_matriz_aleatoria(ndimension=5L,valordiagonal=10L)
##' print(R)
##' cat("Number of missing places: ", sum(is.na(R)),".\n",sep="")
##' cat("Number of values of R: ", length(obtener_valores_posibles(R)),".\n",sep="")
##' cat("Unique values of R:", obtener_valores_posibles(R),".\n")
##' cat("Number of total covers: ", length(obtener_valores_posibles(R)),"^",sum(is.na(R)),".\n",sep="")
##' 
##' ## Saca todas las combinaciones rellenando los 4 primeros huecos
##' mrecubrimiento <- obtener_recubrimiento(R,nhuecos=4L)
##' head(mrecubrimiento)
##' dim(mrecubrimiento) # 1296 13
obtener_recubrimiento <- function(R,nhuecos = 1L){
    R <- chequear_matriz(R)
    nhuecos <- as.integer(nhuecos)
    huecosna <- seq_along(R)[is.na(R)]
    valoresunicos <- obtener_valores_posibles(R)
    if(length(huecosna)==0){
        print(R)
        stop("There is no missing data in R.")
    }
    if(nhuecos <=0){
        stop("NHUECOS must be at leas 1, but it is", nhuecos,".")
    }
    if(nhuecos > length(huecosna)){
        nhuecos <- length(huecosna)
    }
    mpoints <- expandmatrix(valoresunicos,nhuecos,length(huecosna))
    colnames(mpoints) <- huecosna
    mpoints
    }

expandmatrix <- function(valoresunicos,nveces,ncolumnas){
    nveces <- as.integer(nveces)
    ncolumnas <- as.integer(ncolumnas)
    .Call("expand_grid_cpp",valoresunicos,nveces,ncolumnas)
}

