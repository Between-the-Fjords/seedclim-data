

traits.for.species<-function(trait, spp, char=NULL){
  if(is.null(char)){
    sapply(spp, function(sp)traits[traits$species==sp, trait])
  }else{
    sapply(spp, function(sp)traits[traits$species==sp, trait]==char)
    
  }
}
