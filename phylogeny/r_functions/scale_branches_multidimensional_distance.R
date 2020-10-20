library(Rphylopars)
library(ape)

#traits=A data frame with the first column labeled "species" (with species names matching tips on the phylogeny)
#and one column per trait. Each row corresponds to a single observation, and multiple observations for species
#are allowed. Missing data should be represented with NA.

#tree=a phylo class phylogeny

#Scale traits, simulate ancestral state seperately, calculate distance between tip and each node in n-dimensional space


# Variable weighting can be accomplished by transforming scaled values so that more important values have wider ranges
# and therefore account for more of the distance matrix

#multiplier adjusts branch lengths so that negative rates don't appear when log transforming

##########

scale_branches_multidimensional<-function(tree,traits,rate=F,log_tf=F, branch_length_multiplier = NULL){
  
  
  #First, remove species from trait data that aren't in the phylogeny:
  traits<-traits[which(traits$species%in%tree$tip.label),]
  
  #Next, do ancestral state reconstruction on all traits at once using BM with Rphylopars  
  anc_recon<-phylopars(trait_data = traits,tree = tree)$anc_recon
  row.names(anc_recon)[1:length(tree$tip.label)]<-1:length(tree$tip.label)
  
      output_branches<-matrix(data=NA,nrow=length(tree$edge.length),ncol = 1)
      tree_x<-tree
      for(i in 1:length(tree$edge.length)){
        node_1<- tree$edge[i,][1]
        node_2<- tree$edge[i,][2]
        
        value_1<-anc_recon[which(row.names(anc_recon)==node_1),]
        value_2<-anc_recon[which(row.names(anc_recon)==node_2),]
        
        bl<-dist(rbind(value_1,value_2))[1]
        
        if(rate & !log_tf){
        bl<- (bl/tree$edge.length[i])
        }
        
        
        if(rate & log_tf & is.null(multiplier)){
          bl <- log10(bl/(tree$edge.length[i]))
        }
        
        if(rate & log_tf & !is.null(multiplier)){
          bl <- log10(bl*multiplier/(tree$edge.length[i]))
        }
        
        
        output_branches[i]<-bl
        
      }#i loop
      

      
      tree_x$edge.length<-output_branches
      
    
  
  return(tree_x)  
  
  
}#function
