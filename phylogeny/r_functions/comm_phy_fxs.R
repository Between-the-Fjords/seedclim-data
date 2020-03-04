
#replicated mpd
#inputs: 
  #community data matrix
  #directory containing phylogenies
  #n replicates (default = all)

library(picante)

replicated_mpd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){

phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
if(is.null(n_reps)){n_reps<-length(phylogenies)}  
if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)


for(i in 1:n_reps){

  
phy_i<-read.tree(phylogenies[i])  
phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
dist_i<-cophenetic(phy_i)
comm_matrix<-as.matrix(comm_matrix)
comm_matrix[which(is.na(comm_matrix))]<-0
mpd_out[,i]<-mpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)

print(paste(i/n_reps*100," percent done",sep = ""))  


}#i loop  
return(mpd_out)
  
}

###################

replicated_mntd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-mntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mntd_out)
  
}

###################


replicated_pd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-PhyloMeasures::pd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}

###################


replicated_pd_std<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-PhyloMeasures::pd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}

###################


replicated_mpd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mpd_out[,i]<-PhyloMeasures::mpd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mpd_out)
  
}

###################


replicated_mpd_std<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mpd_out[,i]<-PhyloMeasures::mpd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mpd_out)
  
}

###################


replicated_mntd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-PhyloMeasures::mntd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mntd_out)
  
}

###################


replicated_mntd_std<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-PhyloMeasures::mntd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mntd_out)
  
}

############################

library(lefse)

replicated_pd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-weighted.faith(my.phylo = phy_i,my.sample = comm_matrix)
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}

###############################

replicated_vpd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vpd_out[,i]<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}

################################################

replicated_vpd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vpd_out[,i]<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}

###############################################
replicated_vntd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vntd_out[,i]<-vntd(samp = comm_matrix,dis = dist_i,abundance.weighted = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vntd_out)
  
}
###############################################
replicated_vntd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vntd_out[,i]<-vntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vntd_out)
  
}

################



vpd<-function (samp, dis, abundance.weighted = FALSE) 
{
  N <- dim(samp)[1]
  vpd <- numeric(N)
  for (i in 1:N) {
    sppInSample <- names(samp[i, samp[i, ] > 0])
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample, sppInSample]
      if (abundance.weighted) {
        sample.weights <- t(as.matrix(samp[i, sppInSample, 
                                           drop = FALSE])) %*% as.matrix(samp[i, sppInSample, 
                                                                              drop = FALSE])
        vpd[i] <- weighted.var(sample.dis, sample.weights)
      }
      else {
        vpd[i] <- var(sample.dis[lower.tri(sample.dis)])
      }
    }
    else {
      vpd[i] <- NA
    }
  }
  vpd
}

################



vntd<-function (samp, dis, abundance.weighted = FALSE) 
{
  N <- dim(samp)[1]
  vntd <- numeric(N)
  for (i in 1:N) {
    sppInSample <- names(samp[i, samp[i, ] > 0])
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample, sppInSample]
      diag(sample.dis) <- NA
      if (abundance.weighted) {
        vntds <- apply(sample.dis, 2, min, na.rm = TRUE)
        sample.weights <- samp[i, sppInSample]
        vntd[i] <- weighted.var(vntds, sample.weights)
      }
      else {
        vntd[i] <- mean(apply(sample.dis, 2, min, na.rm = TRUE))
      }
    }
    else {
      vntd[i] <- NA
    }
  }
  vntd
}
#######################################

weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
                                       na.rm)
}

###########################################
library(vegan)
replicated_mpd_abd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
  
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
        #
    
    #calc null expectations
    expected_mpds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_mpds)){
      expected_mpds[,j]<-mpd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = T)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_mpd_i<-mpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    all_mpds<-cbind(obs_mpd_i,expected_mpds)
    
    
    mpd_out[,i]<-apply(X = all_mpds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(mpd_out)
  
}
#############################################################

replicated_mntd_abd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)

  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)

  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    #
    
    #calc null expectations
    expected_mntds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_mntds)){
      expected_mntds[,j]<-mntd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = T)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_mntd_i<-mntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    all_mntds<-cbind(obs_mntd_i,expected_mntds)
    
    
    mntd_out[,i]<-apply(X = all_mntds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(mntd_out)
  
}

##################################################

replicated_vpd_abd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
  
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
        #
    
    #calc null expectations
    expected_vpds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_vpds)){
      expected_vpds[,j]<-vpd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = T)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_vpd_i<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    all_vpds<-cbind(obs_vpd_i,expected_vpds)
    
    
    vpd_out[,i]<-apply(X = all_vpds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}

###########################################

replicated_vntd_abd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
    comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
    #
    
    #calc null expectations
    expected_vntds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_vntds)){
      expected_vntds[,j]<-vntd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = T)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_vntd_i<-vntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    all_vntds<-cbind(obs_vntd_i,expected_vntds)
    
    
    vntd_out[,i]<-apply(X = all_vntds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vntd_out)
  
}

############################################

replicated_pd_abd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  
  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
        #
    
    #calc null expectations
    expected_pds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_pds)){
      #expected_pds[,j]<-pd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = T)  
      expected_pds[,j]<-weighted.faith(my.phylo = phy_i,my.sample = comm_perm$perm[[j]])
      
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    #obs_vntd_i<-vntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    obs_pd_i<-weighted.faith(my.phylo = phy_i,my.sample = comm_matrix)
    
    
    all_pds<-cbind(obs_pd_i,expected_pds)
    
    
    pd_out[,i]<-apply(X = all_pds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(pd_out)
  
}

###############################################################

replicated_vpd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  #comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "prab",times = nreps_null)
  
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
          
    #calc null expectations
    expected_vpds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_vpds)){
      expected_vpds[,j]<-vpd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted = F)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_vpd_i<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = F)
    
    all_vpds<-cbind(obs_vpd_i,expected_vpds)
    
    
    vpd_out[,i]<-apply(X = all_vpds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}


###########################################################
replicated_vntd_std<-function(comm_matrix,phylogeny_directory,n_reps_phylo=NULL,nreps_null=100){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps_phylo)){n_reps_phylo<-length(phylogenies)}  
  if(n_reps_phylo>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps_phylo)
  
  comm_matrix<-as.matrix(comm_matrix)
  comm_matrix[which(is.na(comm_matrix))]<-0
  comm_matrix<-apply(X = comm_matrix,MARGIN = 2,FUN = as.integer)
  #comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "count",times = nreps_null)
  comm_perm<-permatfull(m = comm_matrix,fixedmar = "both",shuffle = "ind",mtype = "prab",times = nreps_null)
  
  for(i in 1:n_reps_phylo){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
        
    #
    
    #calc null expectations
    expected_vntds<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = nreps_null)
    for(j in 1:ncol(expected_vntds)){
      expected_vntds[,j]<-vntd(samp = comm_perm$perm[[j]],dis = dist_i,abundance.weighted =F)  
    }#j loop
    
    #calc SES (obs-randmean)/randsd
    
    obs_vntd_i<-vntd(samp = comm_matrix,dis = dist_i,abundance.weighted = F)
    
    all_vntds<-cbind(obs_vntd_i,expected_vntds)
    
    
    vntd_out[,i]<-apply(X = all_vntds,MARGIN = 1,FUN = function(x){(x[1]-mean(x[2:length(x)]))/sd(x[2:length(x)])})
    
    print(paste(i/n_reps_phylo*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vntd_out)
  
}
