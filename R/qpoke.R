
qpoke <- function(n=1){
  
  for(I in 1:n){
    system(
      "echo \"sleep 1\" | qsub"
    )
  }
}
