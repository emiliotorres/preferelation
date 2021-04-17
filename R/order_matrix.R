## #+TITLE: 
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-08-14 17:55 emilio on emilio-XPS-15-9570>
## #+TAGS: 
## #+PROPERTY: header-args :results output :exports both :session 

##

##' .. content for  (no empty lines) ..
##'
##' .. content for  ..
##' @title Get the order of a matrix by columns, selecting some rows.
##' @param m Matrix
##' @param cols Positions or names of the columns to order.
##' @param rows Integer vector with the rows to order.
##' @return An integer vector with the order of the selected rows.
##' @author emilio
##' @export
##' @examples
##' m <- data.matrix(iris[,1:4])
##' m[2,1] <- NA
##' oom <- order_matrix(m,c("Sepal.Length","Sepal.Width"))
##' head(m[oom,])
##'
##' rows <- 1:10
##' oom <- order_matrix(m,c("Sepal.Length","Sepal.Width"),rows)
##' m[rows,][oo,]
##'
##' oom <- order_matrix(m,c("Sepal.Length","Sepal.Width"))
##' oo <- order(m[,"Sepal.Length"],m[,"Sepal.Width"],na.last=FALSE)
##' all.equal(oom,oo)
order_matrix  <- function(m, cols=1L:ncol(m), rows=1L:nrow(m)){
    if(!is.matrix(m)) stop("M is not matrix")
    if(is.character(cols)){
        cols <- match(cols,colnames(m))
        cols <- cols[!is.na(cols)]
    } else if(is.numeric(cols)){
        cols <- as.integer(cols)
    }
    if(nrow(m) < 1L || length(rows) < 1L || length(cols) < 1L){
        return(integer(0)) ## same as order
    }
    if(!is.integer(rows)) rows <- as.integer(rows)
    .Call("order_matrix_cpp",rows,m,cols)
}

