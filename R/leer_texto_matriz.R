##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title dd
##' @param texto 
##' @return dd
##' @author emilio
##' @importFrom data.table fread
##' @export
##' @examples
##' texto <- "
##'         [,1] [,2] [,3] [,4] [,5]
##'    [1,]   10   NA   NA   NA   NA
##'    [2,]    9   10   NA   NA    6
##'    [3,]   NA   NA   10   NA   NA
##'    [4,]   NA   NA   NA   10   NA
##'    [5,]   NA   NA    4   NA   10
##'    "
##'    R <- leer_texto_matriz(texto)
##'   print(R)
leer_texto_matriz <- function(texto){
    pp <- as.matrix(fread(text=texto,sep=" ",header=FALSE)[,-1])
    colnames(pp) <- NULL
    chequear_matriz(pp,verbose=1L)
}
