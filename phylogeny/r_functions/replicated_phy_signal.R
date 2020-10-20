


replicated_signal <- function(phylogeny_list = phylos, traits){

out_signal<-NULL

for(i in 1:length(phylos)){
  
  #load data
  traits_i <- traits
  phy_i <- read.tree(phylogeny_list[i])
  
  #make sure trait and phylogeny have identical species
  phy_i <- keep.tip(phy = phy_i,tip = traits_i$species)
  traits_i <- traits_i[which(traits_i$species %in% phy_i$tip.label),]
  
  #reorder traits to match phylogeny
  
  traits_i <- traits_i[match(phy_i$tip.label,traits_i$species),]
  rownames(traits_i)<-traits_i$species
  
  traits_i <- traits_i[which(colnames(traits_i)!="species")]
  
  for(j in 1:ncol(traits_i)){
    
    traits_j <- as.vector(traits_i[,j])    
    names(traits_j) <- rownames(traits_i)
    phy_out_j <- phylosignal(x = traits_j,phy = phy_i)  
    out_signal <- rbind(out_signal,cbind(i,colnames(traits_i[j]),phy_out_j))
    
    
    
    
  }#j loop
  
  
 
  
  
  
  
  
}#i loop

return(out_signal)  


}