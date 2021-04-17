get_upper_bound_transitivity_mpoints <-
function(R,huecosna,mpoints){
    .Call("get_upper_bound_transitivity_mpoints_cpp",R, huecosna,mpoints)
}
