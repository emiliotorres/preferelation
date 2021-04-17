get_upper_bound_transitivity <-
function(R){
    .Call("get_upper_bound_transitivity_cpp",R)
}
