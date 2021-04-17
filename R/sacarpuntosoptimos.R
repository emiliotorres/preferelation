
sacarpuntosoptimos <- function(R,    valoresunicos,todaslassoluciones=TRUE,combinacionesmaxi=1e7,verbose=0L, transiposiblesolucion= -1L){

    huecosna <- seq_along(R)[is.na(R)]


    if(verbose>0L) cat("\n\nFuncion sacar puntos optimos. Por ahora hay",length(huecosna),"huecos en la matriz de entrada R.\n")
    if(verbose>10L) {
        cat("Datos de entrada:\n")
        cat("Todas las soluciones",todaslassoluciones, "combinacionesmaxi",combinacionesmaxi,"verbose",verbose,"transiposiblesolucion",transiposiblesolucion,"\n")
        print(R)
    }


    if(length(huecosna)==0) stop("Deberia haber algun hueco.")

    ## Determinamos el numero de huecos que vamos a rellenar
    ## Depende del numero de valores, que
    ## las combinaciones se disparan en seguida.
    ## valor double
    lv <- 1.0*length(valoresunicos)
    combinacionesposibles <- lv^length(huecosna)
    if(length(valoresunicos) == 1L){ # Si solo hay un valor disponible, rellenamos todos los huecos
        intentos <- length(huecosna)
    } else {
        ## Solo rellenamos 'intentos' huecos si lv^intentos < combinacionesmaxi
        intentos <- as.integer(floor(log(combinacionesmaxi)/log(length(valoresunicos))))
    }
    if(length(huecosna) < intentos) intentos <- length(huecosna)
    if(intentos == 0L) intentos <- 1L
    if(verbose>2L) cat("Escogemos",intentos,"huecos para rellenar.\n")

    ## Permutaciones con repeticion. Aqui esta explicado
    ## https://torres.epv.uniovi.es/centon/creacion-biblioteca-dinamica-r.html
    mpoints <- expandmatrix(valoresunicos,intentos,length(huecosna))
    colnames(mpoints) <- huecosna

    if(verbose>10L){
        cat("Hemos creado un grid rellenando",intentos,"huecos, que corresponden a las posiciones", huecosna[1:intentos],".\n")
        cat("Grid inicial. Hay", dim(mpoints)[1],"filas en el grid.\n")
        cat("Las primeras filas del grid son:\n")
        print(head(mpoints))
    }
    goverliner_valorestransitivosupper <- get_upper_bound_transitivity_mpoints(R,huecosna,mpoints)

    ## Ordenamos de mayor a menor las cotas upper
    ## Asi, si todas las combinaciones con cota upper maxima
    ## no llegan a ella cuando se rellenan los huecos que les faltan
    ## significa que la cota real upper no es esa, sino una inferior.
    ## ya que las combinaciones con cota upper inferior no pueden tener una cota
    ## upper mas alta que la ya tienen (cuanto mas rellenan los huecos,
    ## la cota upper va disminuyendo o igual, nunca aumentando)

    oo <- order(-goverliner_valorestransitivosupper)
    mpoints <- mpoints[oo,,drop=FALSE]
    goverliner_valorestransitivosupper <- goverliner_valorestransitivosupper[oo]
    tl <- max(goverliner_valorestransitivosupper)

    if(verbose>10L){
        cat("Con este grid, la frecuencia de cotas maximas (upper bounds) son (primera fila es la cota upper, la segunda el numero de veces):\n")
        print(table(goverliner_valorestransitivosupper))
    }

    posiblesolucion <-  matrix(NA,nrow=0,ncol=length(huecosna))
    
    ## Eliminamos los recubrimientos parciales que tengan menor cota upper que la transiposiblesolucion
    f <- goverliner_valorestransitivosupper >= transiposiblesolucion
    if(any(f)){ ## Si hay alguna fila viva...
        mpoints <- mpoints[f,,drop=FALSE]
        goverliner_valorestransitivosupper <- goverliner_valorestransitivosupper[f]
        if(verbose > 10L){
            cat("Como la transitividad de la posible solucion es ", transiposiblesolucion,", nos quedamos solo con las ", nrow(mpoints)," filas del grid que igualan o superan dicha transitividad.\n", sep="" )
        }
    } else { ## Todas las filas son menores que la transitividad que nos meten. Abortamos esta rama.
        posiblesolucion <-  matrix(NA,nrow=0,ncol=length(huecosna))
        if(verbose > 10L){
            cat("Como la transitividad de la posible solucion es ", transiposiblesolucion,", no existe ninguna fila que sea igual o mayor. Devolvemos una patata pinchada en un palo.\n")
            cat("BingoPalo. Rama truncada.\n")
        }
        return(posiblesolucion)
    }



    ## Si ya hemos hecho todo lo que teniamos que hacer
    if(intentos == length(huecosna)){
        ## Es decir, el grid tenia todos los puntos rellenados
        ## no habia ningún valor perdido
        ## En este caso el upper es igual a la cota
        f <- goverliner_valorestransitivosupper == max(goverliner_valorestransitivosupper)
        mpoints <- mpoints[f,,drop=FALSE]
        colnames(mpoints) <- huecosna
        if(verbose>10L){
            cat("Como no hay ningun hueco libre en el grid, hemos completado el recubrimiento.\n")
            cat("Bingo1. La solución son las",nrow(mpoints),"filas del grid de maxima transitividad (",max(goverliner_valorestransitivosupper),").\n")
        }
        return(mpoints)
    } else {
        tstar_maxvalortransiupper <- max(goverliner_valorestransitivosupper)
        nvecesdestetstar  <- sum(goverliner_valorestransitivosupper >= tstar_maxvalortransiupper)
        if(verbose>1L){
            cat("La cota upper maxima de ese grid es", tstar_maxvalortransiupper,". Cualquier solucion tendra una transitividad menor o igual.\n")
            cat("La cota upper minima de ese grid es", min(goverliner_valorestransitivosupper),". Cualquier solucion tendra una transitividad mayor o igual.\n")
            cat("La tabla de frecuencia de las cotas upper de este grid son:\n")
            print(table(goverliner_valorestransitivosupper))
        }
        tempR <- R
        for(i in 1:nrow(mpoints)){

            if(verbose > 2L){
                if(!i %% 100){
                    cat("Fila",i,"(faltan como mucho ",nrow(mpoints) -i,". Lo más seguro es que queden ",nvecesdestetstar-i,").\n")
                }
            
            
            if(verbose>10L){
                cat("Fila",i,"(faltan ",nrow(mpoints) -i,"). Analizamos la fila",i," de ese grid.\n")
                cat("Fila",i,". Cota upper de esta fila ",goverliner_valorestransitivosupper[i],".\n")
            }

            }

            if( goverliner_valorestransitivosupper[i] < tstar_maxvalortransiupper){
                if(verbose>10L){
                    cat("Fila",i,". En esta fila ya descendemos de nivel de upper.\n")
                    cat("Fila",i,". Es decir, al estar ordenados, no ha habido ninguna combinacion que haya alcanzado ", tstar_maxvalortransiupper,"\n")
                    cat("Fila",i,". Y ya con esta fila, lo maximo que optamos es a ",goverliner_valorestransitivosupper[i],"\n")
                }
                tstar_maxvalortransiupper <- goverliner_valorestransitivosupper[i]
                if(todaslassoluciones){
                    ## Si queremos todas las soluciones,
                    ## tenemos que probar todas lineas hasta
                    ## bajar a una que cuyo upper  no alcance a las
                    ## soluciones que ya tenemos
                    ## Buscamos todas las soluciones
                    ## La desigualdad tiene que ser estricta
                    if(tstar_maxvalortransiupper < transiposiblesolucion){
                        if(verbose>10L){
                            cat("Fila",i,". Bingo2. Ya no hay mas puntos que alcancen la transitividad de las soluciones guardadas (",transiposiblesolucion,").\n\n")
                        }
                        return(posiblesolucion)
                    }

                } else{
                    ## Nos vale con alguna solucion
                    if(tstar_maxvalortransiupper <= transiposiblesolucion){
                        if(verbose>10L){
                            cat("Fila",i,". Bingo2a. Ya teníamos puntos con esta cota.\n\n")
                        }
                        return(posiblesolucion)
                    }
                }
            }

            if(verbose > 10L){
               cat("Fila",i,". Rellenamos tempR con los siguientes valores y procedemos el algortimo con esta matriz.\n")
                print(mpoints[i,])
                ##print(tempR)
            }
            tempR[huecosna] <- mpoints[i,]
            mpointsresto <- sacarpuntosoptimos(tempR,valoresunicos=valoresunicos,todaslassoluciones=todaslassoluciones,combinacionesmaxi=combinacionesmaxi,verbose=verbose-11L,transiposiblesolucion=transiposiblesolucion)

            if(verbose > 10L) cat("\nFila",i,". VOLVEMOS del algoritmo con ",nrow(mpointsresto)," posibilidades.\n")
            
            if(nrow(mpointsresto) < 1L ){
                if(verbose > 10L) cat("Fila",i,". Rama abortada. Pasamos a la siguiente fila.\n")
                next ## Pasamos a la siguiente fila
            }

            ## TODO. Implementar esto mejor.
            m <- mpoints[i, 1:intentos,drop=FALSE]
            m <- matrix(m,nrow=nrow(mpointsresto),ncol=ncol(m),byrow=TRUE)
            colnames(m) <- colnames(mpoints)[1:intentos]
            newmpoints <- cbind(m,mpointsresto)
            if(verbose > 10L){
                cat("Fila",i,"y nos quedan los puntos así:")
                cat( " new points \n")
                print(head(newmpoints))
            }

            transi <- get_transitivity_mpoints(R,huecosna,newmpoints)
            maxitransinewpoints <- max(transi)
            if(verbose > 10L){
                cat("Fila",i,". La transitividad maxima en estos puntos es ", maxitransinewpoints,".\n")
            }

            ## Archivamos si la mejoramos o igualamos lo que
            ## ya esta archivado
            if(maxitransinewpoints > transiposiblesolucion){
                ## Si este punto mejor las posibles soluciones ya
                ## almacenadas, las elimnamos
                ## y ponemos este punto
                if(verbose > 10L){
                    cat("Fila",i,". La transitividad maxima obtenida (",maxitransinewpoints,") es mejor que la de las soluciones que teniamos archivas (", transiposiblesolucion,").\n    Borramos las anteriores soluciones guardadas y nos quedamos con las que tienen maxima transitividad.\n")
                }

                transiposiblesolucion <- maxitransinewpoints
                f <- transi == maxitransinewpoints
                posiblesolucion <- newmpoints[f,,drop=FALSE]
            } else if(maxitransinewpoints == transiposiblesolucion){

                ## Si este punto iguala las posibles soluciones,
                ## lo añadimos.
                f <- transi == maxitransinewpoints
                posiblesolucion <- rbind(posiblesolucion,
                                         newmpoints[f,,drop=FALSE])
                if(verbose > 10L){
                    cat("Fila",i,". La transitividad obtenida",maxitransinewpoints,"es igual a la de las que teniamos archivadas.\n    Las anadimos. Ahora tenemos", nrow(posiblesolucion), "recubrimientos (posibles soluciones) con esta cota.\n")
                }

            }
            if(maxitransinewpoints >= tstar_maxvalortransiupper && !todaslassoluciones ){
                ## Si solo buscamos una solucion, ya la tenemos.
                ## Su cota alcanza el maximo upper posible, por lo que es solución
                return(posiblesolucion)
            }

        }

        ## Aqui, si solo buscamos una solucion, no deberiamos llegar
        ## Y si buscamos todas, es muy raro que hayamos llegado hasta
        ## aqui: la maxima cota se alcanza en las ultimas filas.
        ## Devolvemos lo que tengamos almacenado
        if(verbose>10L)cat("Bingo3. Hemos revisado todas las filas del grid. Devolvemos",nrow(posiblesolucion)," posibles soluciones.\n\n")
        return(posiblesolucion)

    }
}
