
highest_nonempty_set_composition <- function(sets){
  if(length(sets)<2) return(1)
  
  for(i in length(sets):2){
    com <- combn(length(sets),i)
    tar <- apply(com, MARGIN = 2, function(idx) length(Reduce(intersect,sets[idx])))
    if(max(tar)>0)break
  }
  tar_max <- which(tar==max(tar))
  com_max<-com[,tar_max]
  compsitions <- as.list(as.data.frame(com_max))
  names(compsitions) <- NULL
  return(compsitions)
}