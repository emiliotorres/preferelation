##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title Admissible values to fill the missing places.
##' @param R 
##' @param solovaloresexistentes usa solo los valores existentes en R para rellenar el resto de los huecos
##' @return .
##' @author emilio
##' @export
##' @examples
##' 1
obtener_valores_posibles <-
    function(R,solovaloresexistentes=TRUE){
        ##    print(R)
        tempR <- R
        diag(tempR)  <- NA
        res <- sort(unique(tempR[!is.na(tempR)]))
        if(length(res) == 0)res <- c(0L,res)
        res <- unique(res)
        if(solovaloresexistentes)
            return(res)
        ## ¿Cuántos huecos hay?
        nhuecos <- sum(is.na(R))
        res0 <- unique(c(0,res,R[1L]))
        res <- res0
        if(length(res)>1L){
            for(i in 2L:length(res0)){
                paso  <- (res0[i]-res0[i-1L])/(nhuecos+1L)
                res <- c(res, res0[i-1L]+ paso*(1:(nhuecos+1L)))
            }
        }
        res <- sort(unique(res))
        res
    }
