##' .. content for  (no empty lines) ..
##'
##' .. content for ..
##' @title Get a matrix with the solutions
##' @param R matrix
##' @param todaslassoluciones TRUE saca todas las soluciones. FALSE: solo saca las primeras que encuentra.
##' @param solovaloresexistentes Usa solo los valores que existen en la matriz para rellenar los huecos. 
##' @param combinacionesmaxi Cuanto mas grande, mas rapido ira. Problemas de memoria del PC
##' @param verbose verbose
##' @return .
##' @author emilio
##' @export
obtener_puntos_optimos <-
function(R,todaslassoluciones=TRUE,solovaloresexistentes=TRUE,solominimos=TRUE,combinacionesmaxi=10^7,verbose=0L){
    R <- chequear_matriz(R)
    verbose <- as.integer(verbose)
    if(is.na(verbose)) verbose <- 0L
    if(isTRUE(as.logical(todaslassoluciones))) todaslassoluciones  <-  TRUE
    else todaslassoluciones <- FALSE
    r1 <- sacar_nueva_matriz_sin_columnas_o_filas_perdidas(R,verbose=0L)
    if(!identical(r1,R)){
        print(R)
        stop("Revisa la matriz R, tiene una columna o fila con todos los valores NA o 0.\nUsa previamente: 'R <- sacar_nueva_matriz_sin_columnas_o_filas_perdidas(R)' ")
    }
    valoresunicos <- obtener_valores_posibles(R,solovaloresexistentes)
    if(verbose>0L) {
        cat("Los valores que utilizamos para rellenar son: ", valoresunicos,".\n")
    }
    if(combinacionesmaxi < length(valoresunicos) || is.na(combinacionesmaxi)) combinacionesmaxi  <- length(valoresunicos)


    
    mpoints <- sacarpuntosoptimos(R=R,valoresunicos=valoresunicos,todaslassoluciones=todaslassoluciones,combinacionesmaxi=combinacionesmaxi,verbose=verbose)
    if(solominimos){
        s <- rowSums(mpoints)
        mpoints[s==min(s),,drop=FALSE]
    } else {
        mpoints
        }
}
