#helper functions for sunplin and phylogenies
library(BIEN)

#sp_fam: dataframe where 1st column is species, second is family

#function to generate puts file
get_put_info<-function(sp_fam,phylogeny){

sp_fam$put_level<-NA
sp_fam$put<-NA

taxonomy<-BIEN_taxonomy_species(species = gsub(pattern = "_",replacement = " ",x = phylogeny$tip.label))

for( i in 1:nrow(sp_fam)){
  genus<-unlist(strsplit(x = as.character(sp_fam[i,1]),split = " "))[1]
  
  #check whether species is in phylogeny
  if(gsub(pattern = " ",replacement = "_",x = as.character(sp_fam[i,1])) %in% phylogeny$tip.label  ){
    sp_fam$put_level[i]<-"present"
    sp_fam$put[i]<-"present"
  }
  
  
  
  #Check whether genus is present multiple times
  if(length(grep(pattern = paste(genus,"_",sep = ""),x = phylogeny$tip.label))>1 & is.na(sp_fam$put[i])){
    sp_fam$put_level[i]<-"mrca_genus"
    sp_fam$put[i]<-genus }#if
  
  #Check whether genus is present at least once
  if(length(grep(pattern = paste(genus,"_",sep = ""),x = phylogeny$tip.label))==1 & is.na(sp_fam$put[i])){
    sp_fam$put_level[i]<-"congener"
    sp_fam$put[i]<-phylogeny$tip.label[grep(pattern = paste(genus,"_",sep = ""),x = phylogeny$tip.label)] }#if
  
  
  #Check whether family is present multiple times (and genus is not present)
  
  fam_i<-as.character(sp_fam[i,2])
  
  if(length(fam_i)>=1 & is.na(sp_fam$put_level[i])){
    
    if(length(fam_i)>1 | fam_i == "Unknown"){stop("too many families or only unknown")}  
    
    
    
    spp_in_fam_i <- 
      phylogeny$tip.label[which(phylogeny$tip.label %in% 
                                  gsub(pattern = " ",replacement = "_",taxonomy$scrubbed_species_binomial[which(taxonomy$scrubbed_family==fam_i)]) )]
    
    
    #if genus attempts havent worked and there are multiple confamilials, use family mrca
    if( length(spp_in_fam_i)>1 &  is.na(sp_fam$put_level[i])){
      sp_fam$put_level[i]<-"mrca_family"
      sp_fam$put[i]<-fam_i  }#if
    
    
    if( length(spp_in_fam_i)==1 &  is.na(sp_fam$put_level[i])){
      sp_fam$put_level[i]<-"confamilial"
      sp_fam$put[i]<-spp_in_fam_i  }#if
    
    
    #If none of these have worked, the species will retain an NA, and be removed later
    
    rm(spp_in_fam_i,fam_i)
  }#only go through family stuff if we have a family in the taxonomy

  if(is.na(sp_fam$put_level[i])){
    sp_fam$put_level[i]<-"remove"
    sp_fam$put[i]<-"remove"
  }
  
  
    
}#for i loop
rm(fam_i,i)

return(sp_fam)

}#end function


#######################################################################################

#######################################################################################

#function to generate annotated phylogeny file and 


make_puts_input<-function(puts_info,phylogeny,phylogeny_filename="phylogeny.tre",puts_filename="taxa.puts"){
  
taxonomy<-BIEN_taxonomy_species(species = gsub(pattern = "_",replacement = " ",x = phylogeny$tip.label))
  
phylogeny$node.label[1:length(phylogeny$node.label)]<-""

genera_to_add<-unique(puts_info$put[which(puts_info$put_level=="mrca_genus")])
families_to_add<-unique(puts_info$put[which(puts_info$put_level=="mrca_family")])

#Label genera nodes
for(i in 1:length(genera_to_add)){
  
  genus_i<-genera_to_add[i]  
  
  mrca_i<-getMRCA(phy = phylogeny,
                  tip = grep(pattern = paste(genus_i,"_",sep = ""),x = phylogeny$tip.label)   )
  
  #if the node isnt labelled yet, label it.
  if(phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]==""){
    
    phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]<-genus_i
    
  }else{
    
    #if the node IS labelled, modify the put of the genus in question to match the node label
    if(phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]!=""){
      
      label_i<-phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]
      puts_info$put[which(puts_info$put_level=="mrca_genus" & puts_info$put==genus_i)]<-label_i
      rm(label_i)
      
    }else{stop("Something weird happened")}
    
  }#if the node is labelled
  
  rm(genus_i,mrca_i)
  
}


#Label family nodes

for(i in 1:length(families_to_add)){
  
  fam_i<-families_to_add[i]  
  spp_in_family <- taxonomy$scrubbed_species_binomial[which(taxonomy$scrubbed_family == fam_i)]
  
  
  mrca_i<-getMRCA(phy = phylogeny,
                  tip = which(phylogeny$tip.label %in% gsub(pattern = " ",replacement = "_", x = spp_in_family   ) )) 
  
  
  
  #if the node isnt labelled yet, label it.
  if(phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]==""){
    
    phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]<-fam_i
    
  }else{
    
    #if the node IS labelled, modify the put of the genus in question to match the node label
    if(phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]!=""){
      
      label_i<-phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]
      puts_info$put[which(puts_info$put_level=="mrca_family" & puts_info$put==fam_i)]<-label_i
      rm(label_i)
      
    }else{stop("Something weird happened")}
    
  }#if the node is labelled
  
  
  rm(fam_i,mrca_i,spp_in_family)
  
}
rm(i,genera_to_add,families_to_add)



#3) Need to prepare a .puts (phylogenetically uncertain taxa) file containing all species to be added

puts_info_for_output<-puts_info[union(x = grep(pattern = "mrca",x = puts_info$put_level),y = grep(pattern = "con",x = puts_info$put_level)),]


file.create(puts_filename)
writeLines(text = paste(gsub(pattern = " ",replacement = "_",x = puts_info_for_output[,1] )," ",puts_info_for_output$put,sep = ""),
           con = puts_filename)  


#write current version of tree (with node labels)

write.tree(phy = phylogeny,file = phylogeny_filename)


}#end function



#############################################################################################

#############################################################################################


#Make replicated phylogenies

sunplin_phylo_replicates<-function(put_file,phylogeny_file,output_directory=NULL,output_base_filename=NULL,nrep=1,method=2,directory=NULL,taxa_to_keep=NULL){
  wd<-getwd()
  if(!is.null(directory)){setwd(directory)}
  
  #r limits character size, so have to use iterate the hard way
  
  for(i in 1:nrep){    
    
    trees <- sunplin.expd(phylogeny_file,put_file,numTree = 1 ,method = method)
    t1<-strsplit(x = trees,split = ";")[[1]][2]
    t2<-read.tree(text = paste(t1,";",sep = " "))
    
    
    #summary(t2)
    
    if(!is.null(taxa_to_keep)){
    

    t2<-drop.tip(phy = t2,
                 tip = t2$tip.label[which(!t2$tip.label %in% gsub(pattern = " ",replacement = "_",x = taxa_to_keep))],
                 trim.internal = T,
                 collapse.singles = T  ) 
    #summary(t2)  
      
    }
    
    
    write.tree(phy = t2,file = paste(output_directory,output_base_filename,i,".tre",sep = ""))  
    print(paste(i/nrep*100, "percent done"))
    
    
  }
  
  
  setwd(wd)
  
  
}#phylo reps fx





