library(ape)
library(phytools)
library(apTreeshape)
#devtools::install_version("CollessLike")
library(CollessLike)
#two simulations:
  #1) increasing min distance
  #2) removing outliers

#mean dist
#var dist


out_limiting <- NULL
nreps<-1000
  for(i in 1:nreps){
    
    tree <- pbtree(n = 100)
    colless <- colless.like.index(tree,norm = T)
    traits <- rTraitCont(phy = tree,model = "BM")
    trait_dist <- as.matrix(dist(traits,diag = T,upper = T))
    tree_dist <- as.matrix(cophenetic.phylo(tree))
    
    phy_var <- var(tree_dist[upper.tri(tree_dist,diag = F)])
    phy_var_min <-var(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
            
    phy_mean <- mean(tree_dist[upper.tri(tree_dist,diag = F)])
    phy_mean_min <- mean(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    
    trait_var <-var(trait_dist[upper.tri(trait_dist,diag = F)])
    trait_var_min <- var(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    trait_mean <-mean(trait_dist[upper.tri(trait_dist,diag = F)])
    trait_mean_min <- mean(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    out_i <- cbind(i,100,colless,
                   phy_var,phy_var_min,
                   phy_mean,phy_mean_min,
                   trait_var,trait_var_min,
                   trait_mean,trait_mean_min)
    names(out_i) <- c("iteration","tips","Colless",
                                          "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                          "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                          "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                          "Mean Functional Distance","Mean Minimum Functional Distance")
    
    for(t in 1:95){
      
    #remove a tip  
      #which species is the closest to the others? (min min dist)
      
      tip_to_drop <- names(which.min(apply(X = trait_dist,MARGIN = 2,FUN = function(x){min(x[which(x!=0)])})))
      
      
      
      tree<- drop.tip(phy = tree,
               tip = tip_to_drop)
      colless <- colless.like.index(tree,norm = T)
      
      index <- which(colnames(trait_dist)==tip_to_drop)
        
      tree_dist <- tree_dist[,-index]
      tree_dist <- tree_dist[-index,]
      
      trait_dist <- trait_dist[,-index]
      trait_dist <- trait_dist[-index,]
      
      if(tip_to_drop%in%colnames(tree_dist)){stop("tree_dist")}
      if(tip_to_drop%in%colnames(trait_dist)){stop("trait_dist")}
      
    #recalculate metrics
      phy_var <- var(tree_dist[upper.tri(tree_dist,diag = F)])
      phy_var_min <-var(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
      
      phy_mean <- mean(tree_dist[upper.tri(tree_dist,diag = F)])
      phy_mean_min <- mean(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
      
      
      trait_var <-var(trait_dist[upper.tri(trait_dist,diag = F)])
      trait_var_min <- var(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
      
      trait_mean <-mean(trait_dist[upper.tri(trait_dist,diag = F)])
      trait_mean_min <- mean(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
      

      out_t <- cbind(i,100-t,colless,
                     phy_var,phy_var_min,
                     phy_mean,phy_mean_min,
                     trait_var,trait_var_min,
                     trait_mean,trait_mean_min)
      
      names(out_i) <- c("iteration","tips","Colless",
                        "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                        "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                        "Variance in Functional Distance","Variance in Minimum Functional Distance",
                        "Mean Functional Distance","Mean Minimum Functional Distance")
      
      
      
      #out_t <- cbind(i,100-t,phy_var,phy_mean,trait_var,trait_mean)  
      out_i<- rbind(out_i,out_t)
      
    }
    
    out_limiting<-rbind(out_limiting,out_i)
    
    
    
  }



pairs(out_limiting[,3:6])

cor.test(out_limiting[,3],out_limiting[,5]) #expect positive but weak correlations with trait var
cor.test(out_limiting[,3],out_limiting[,6]) #expect positive but weak corr with trait mean

cor.test(out_limiting[,4],out_limiting[,5]) #expect positive but weak correlations with trait var
cor.test(out_limiting[,4],out_limiting[,6]) #expect positive but weak corr with trait mean

out_limiting<-as.data.frame(out_limiting)
out_limiting$species_removed <- 100 - out_limiting$V2

names(out_limiting) <- c("iteration","tips","Colless",
                  "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                  "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                  "Variance in Functional Distance","Variance in Minimum Functional Distance",
                  "Mean Functional Distance","Mean Minimum Functional Distance", "Number removed")


pairs(out_limiting[,3:7])


##########################



out_filtering<-NULL
for(i in 1:nreps){
  
  tree <- pbtree(n = 100)
  colless <- colless.like.index(tree,norm = T)
  traits <- rTraitCont(phy = tree,model = "BM")
  trait_dist <- as.matrix(dist(traits,diag = T,upper = T))
  tree_dist <- as.matrix(cophenetic.phylo(tree))
  
  phy_var <- var(tree_dist[upper.tri(tree_dist,diag = F)])
  phy_var_min <-var(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
  
  phy_mean <- mean(tree_dist[upper.tri(tree_dist,diag = F)])
  phy_mean_min <- mean(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
  
  
  trait_var <-var(trait_dist[upper.tri(trait_dist,diag = F)])
  trait_var_min <- var(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
  
  trait_mean <-mean(trait_dist[upper.tri(trait_dist,diag = F)])
  trait_mean_min <- mean(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
  
  out_i <- cbind(i,100,colless,
                 phy_var,phy_var_min,
                 phy_mean,phy_mean_min,
                 trait_var,trait_var_min,
                 trait_mean,trait_mean_min)
  names(out_i) <- c("iteration","tips","Colless",
                    "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                    "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                    "Variance in Functional Distance","Variance in Minimum Functional Distance",
                    "Mean Functional Distance","Mean Minimum Functional Distance")
  
  
  
  for(t in 1:95){
    
    #remove a tip  
    #which species is the furthest from the mean
    
    tip_to_drop <- names(traits)[which.max(abs(traits-mean(traits)))]
    
    traits <- traits[-which(names(traits)==tip_to_drop)]
    
    
    tree<- drop.tip(phy = tree,
                    tip = tip_to_drop)
    colless <- colless.like.index(tree,norm = T)
    
    index <- which(colnames(trait_dist)==tip_to_drop)
    
    tree_dist <- tree_dist[,-index]
    tree_dist <- tree_dist[-index,]
    
    trait_dist <- trait_dist[,-index]
    trait_dist <- trait_dist[-index,]
    
    if(tip_to_drop%in%colnames(tree_dist)){stop("tree_dist")}
    if(tip_to_drop%in%colnames(trait_dist)){stop("trait_dist")}
    
    #recalculate metrics
    phy_var <- var(tree_dist[upper.tri(tree_dist,diag = F)])
    phy_var_min <-var(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    phy_mean <- mean(tree_dist[upper.tri(tree_dist,diag = F)])
    phy_mean_min <- mean(apply(X = tree_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    
    trait_var <-var(trait_dist[upper.tri(trait_dist,diag = F)])
    trait_var_min <- var(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    trait_mean <-mean(trait_dist[upper.tri(trait_dist,diag = F)])
    trait_mean_min <- mean(apply(X = trait_dist,MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    
    out_t <- cbind(i,100-t,colless,
                   phy_var,phy_var_min,
                   phy_mean,phy_mean_min,
                   trait_var,trait_var_min,
                   trait_mean,trait_mean_min)
    
    names(out_i) <- c("iteration","tips","Colless",
                      "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                      "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                      "Variance in Functional Distance","Variance in Minimum Functional Distance",
                      "Mean Functional Distance","Mean Minimum Functional Distance")
    
    
    
    
    #out_t <- cbind(i,100-t,phy_var,phy_mean,trait_var,trait_mean)  
    out_i<- rbind(out_i,out_t)
    
  }
  
  out_filtering<-rbind(out_filtering,out_i)
  
  
  
}


colnames(out_filtering)
pairs(out_filtering[,3:6])

cor.test(out_filtering[,3],out_filtering[,5]) #expect positive but weak correlations with trait var
cor.test(out_filtering[,3],out_filtering[,6]) #expect positive but weak corr with trait mean

cor.test(out_filtering[,4],out_filtering[,5]) #expect positive but weak correlations with trait var
cor.test(out_filtering[,4],out_filtering[,6]) #expect positive but weak corr with trait mean

out_filtering<-as.data.frame(out_filtering)
out_filtering$species_removed <- 100 - out_filtering$V2
pairs(out_filtering[,3:7])

names(out_filtering) <- c("iteration","tips","Colless",
                         "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                         "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                         "Variance in Functional Distance","Variance in Minimum Functional Distance",
                         "Mean Functional Distance","Mean Minimum Functional Distance", "Number removed")


###################################


#Limiting
limiting_summary<-list()
limiting_out_mat_r<-array(dim = c(10,10,nreps))
limiting_out_mat_p<-array(dim = c(10,10,nreps))

for(i in 1:nreps){

data_i <- out_limiting[which(out_limiting$i==i),]
corr_i <- Hmisc::rcorr(as.matrix(data_i[,3:12]))
limiting_out_mat_r[,,i]<-corr_i$r
limiting_out_mat_p[,,i]<-corr_i$P

  
}


colnames(limiting_out_mat_r)<-colnames(corr_i$r)
rownames(limiting_out_mat_r)<-colnames(corr_i$r)

colnames(limiting_out_mat_p)<-colnames(corr_i$r)
rownames(limiting_out_mat_p)<-colnames(corr_i$r)

limiting_summary[[1]] <- apply(X = limiting_out_mat_r,MARGIN = c(1,2),FUN = mean)
names(limiting_summary)[1] <- "mean_r"

limiting_summary[[2]] <- apply(X = limiting_out_mat_p,MARGIN = c(1,2),FUN = mean)
names(limiting_summary)[2] <- "mean_p"

limiting_summary[[3]] <- apply(X = limiting_out_mat_r,MARGIN = c(1,2),
                                FUN = function(x){  
                                x <-sort(x)  
                                x <- x[-1:-(nreps*.025)]  
                                x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                min(x)
                                })
names(limiting_summary)[3] <- "CI_min_r"

limiting_summary[[4]] <- apply(X = limiting_out_mat_r,MARGIN = c(1,2),
                                FUN = function(x){  
                                x <-sort(x)  
                                x <- x[-1:-(nreps*.025)]  
                                x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                max(x)
                                })
names(limiting_summary)[4] <- "CI_max_r"


limiting_summary[[5]] <- apply(X = limiting_out_mat_p,MARGIN = c(1,2),
                                FUN = function(x){  
                                x <-sort(x)  
                                x <- x[-1:-(nreps*.025)]  
                                x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                min(x)
                                })
names(limiting_summary)[5] <- "CI_min_p"

limiting_summary[[6]] <- apply(X = limiting_out_mat_p,MARGIN = c(1,2),
                                FUN = function(x){  
                                x <-sort(x)  
                                x <- x[-1:-(nreps*.025)]  
                                x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                max(x)
                                })
names(limiting_summary)[6] <- "CI_max_p"

limiting_summary[[7]] <- limiting_summary$CI_max_r>0 & limiting_summary$CI_min_r<0
names(limiting_summary)[7] <- "CI_r_includes_zero"

#filtering
filtering_summary<-list()
filtering_out_mat_r<-array(dim = c(10,10,nreps))
filtering_out_mat_p<-array(dim = c(10,10,nreps))

for(i in 1:nreps){
  
  data_i <- out_filtering[which(out_filtering$i==i),]
  corr_i <- Hmisc::rcorr(as.matrix(data_i[,3:12]))
  filtering_out_mat_r[,,i]<-corr_i$r
  filtering_out_mat_p[,,i]<-corr_i$P
  
  
}


colnames(filtering_out_mat_r)<-colnames(corr_i$r)
rownames(filtering_out_mat_r)<-colnames(corr_i$r)

colnames(filtering_out_mat_p)<-colnames(corr_i$r)
rownames(filtering_out_mat_p)<-colnames(corr_i$r)

filtering_summary[[1]] <- apply(X = filtering_out_mat_r,MARGIN = c(1,2),FUN = mean)
names(filtering_summary)[1] <- "mean_r"

filtering_summary[[2]] <- apply(X = filtering_out_mat_p,MARGIN = c(1,2),FUN = mean)
names(filtering_summary)[2] <- "mean_p"

filtering_summary[[3]] <- apply(X = filtering_out_mat_r,MARGIN = c(1,2),
                                FUN = function(x){  
                                  x <-sort(x)  
                                  x <- x[-1:-(nreps*.025)]  
                                  x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                  min(x)
                                })
names(filtering_summary)[3] <- "CI_min_r"

filtering_summary[[4]] <- apply(X = filtering_out_mat_r,MARGIN = c(1,2),
                                FUN = function(x){  
                                  x <-sort(x)  
                                  x <- x[-1:-(nreps*.025)]  
                                  x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                  max(x)
                                })
names(filtering_summary)[4] <- "CI_max_r"


filtering_summary[[5]] <- apply(X = filtering_out_mat_p,MARGIN = c(1,2),
                                FUN = function(x){  
                                  x <-sort(x)  
                                  x <- x[-1:-(nreps*.025)]  
                                  x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                  min(x)
                                })
names(filtering_summary)[5] <- "CI_min_p"

filtering_summary[[6]] <- apply(X = filtering_out_mat_p,MARGIN = c(1,2),
                                FUN = function(x){  
                                  x <-sort(x)  
                                  x <- x[-1:-(nreps*.025)]  
                                  x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                  max(x)
                                })
names(filtering_summary)[6] <- "CI_max_p"

filtering_summary[[7]] <- filtering_summary$CI_max_r>0 & filtering_summary$CI_min_r<0
names(filtering_summary)[7] <- "CI_r_includes_zero"

########################################
#Plotting correlations
library(corrplot)



corrplot(filtering_summary$mean_r, method = "ellipse",
         type="full", title = "Filtering",
         p.mat = filtering_summary$CI_r_includes_zero,
         diag = F,tl.col = "black")

corrplot(limiting_summary$mean_r, method = "ellipse",
         type="full",title = "Limiting Similarity",
         p.mat = limiting_summary$CI_r_includes_zero,
         diag = F,tl.col = "black")







