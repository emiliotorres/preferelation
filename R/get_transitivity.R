get_transitivity <-
function(R){
    if(anyNA(R)) stop("Valores perdidos")

    .Call("get_transitivity_cpp",R)
}
