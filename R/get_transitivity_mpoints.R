get_transitivity_mpoints <-
function(R,huecosna,mpoints){
    .Call("get_transitivity_mpoints_cpp",R, huecosna,mpoints)
}
