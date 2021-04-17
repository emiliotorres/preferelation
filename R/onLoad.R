## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2020-08-08 23:21 emilio on emilio-XPS-15-9570>
## ============================================================

## http://r-pkgs.had.co.nz/src.html

##' .. content for description{} (no empty lines) ..
##'
##' .. content for details{} .. @rawNamespace useDynLib(data.table); useDynLib(emiliocpp)
##' @title SIEMPRE PON ESTE FICHERO
##' @param libpath 1 
##' @return 1
##' @author emilio
##' @useDynLib preferelation
.onUnload <- function(libpath) {
  library.dynam.unload("preferelation", libpath)
}
