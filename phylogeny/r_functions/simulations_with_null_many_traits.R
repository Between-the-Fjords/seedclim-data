#Simulation with null expectations

source("phylogeny/r_functions/simulation_mean_sd_calc.R")
library(ape)
library(phytools)
library(foreach)
library(doParallel)
###################################
nreps <- 1000

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

#for(i in 1:nreps){
out_limiting_combined <- foreach(i = 1:nreps,
                                 .combine = rbind,
                                 .packages = c("ape","phytools"))%dopar%{
                                   print(i)
                                   tree <- pbtree(n = 100)
                                   out_limiting <- NULL
                                   out_limiting_ses <- NULL
                                   
                                   #colless <- colless.like.index(tree,norm = T) #slows down code, so commenting out
                                   colless <- NA 
                                   #traits <- rTraitCont(phy = tree,model = "BM")
                                   traits <- replicate(n = 20,expr = rTraitCont(phy = tree,model = "BM"))
                                   trait_dist <- as.matrix(dist(traits,diag = T,upper = T))
                                   tree_dist <- as.matrix(cophenetic.phylo(tree))
                                   exp_i <- expected_values(tree = tree,traits = traits,n_reps = 100)
                                   rm(tree)
                                   
                                   #cor(as.numeric(tree_dist),as.numeric(trait_dist)) r=.2
                                   
                                   
                                   colnames(trait_dist)
                                   colnames(tree_dist)
                                   
                                   #cor(exp_i[which(exp_i$trait=="trait_mean"),2:3])
                                   #cor(exp_i[which(exp_i$trait=="phy_mean_min"),2:3])
                                   #cor(exp_i[which(exp_i$trait=="trait_mean_min"),2:3])
                                   
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
                                   
                                   
                                   #SES version
                                   phy_var_ses <- (phy_var-exp_i$mean[which(exp_i$trait=="phy_var" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_var" & exp_i$richness==100)]
                                   
                                   
                                   phy_var_min_ses <- (phy_var_min-exp_i$mean[which(exp_i$trait=="phy_var_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_var_min" & exp_i$richness==100)]
                                   
                                   phy_mean_ses <- (phy_mean-exp_i$mean[which(exp_i$trait=="phy_mean" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_mean" & exp_i$richness==100)]
                                   phy_mean_min_ses <- (phy_mean_min-exp_i$mean[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100)]
                                   
                                   
                                   trait_var_ses <-(trait_var-exp_i$mean[which(exp_i$trait=="trait_var" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_var" & exp_i$richness==100)]
                                   trait_var_min_ses <-(trait_var_min-exp_i$mean[which(exp_i$trait=="trait_var_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_var_min" & exp_i$richness==100)]
                                   
                                   trait_mean_ses <-(trait_mean-exp_i$mean[which(exp_i$trait=="trait_mean" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_mean" & exp_i$richness==100)]
                                   trait_mean_min_ses <-(trait_mean_min-exp_i$mean[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100)]
                                   
                                   out_i_ses <- cbind(i,100,colless,
                                                      phy_var_ses,phy_var_min_ses,
                                                      phy_mean_ses,phy_mean_min_ses,
                                                      trait_var_ses,trait_var_min_ses,
                                                      trait_mean_ses,trait_mean_min_ses)
                                   
                                   names(out_i_ses) <- c("iteration","tips","Colless",
                                                         "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                         "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                         "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                         "Mean Functional Distance","Mean Minimum Functional Distance")
                                   
                                   
                                   
                                   #plot(traits)
                                   
                                   
                                   
                                   for(t in 1:95){
                                     
                                     #remove a tip  
                                     #which species is the closest to the others? (min min dist)
                                     
                                     tip_to_drop <- names(which.min(apply(X = trait_dist,MARGIN = 2,FUN = function(x){min(x[which(x!=0)])})))
                                     
                                     #abline(h = traits[which(names(traits)==tip_to_drop)])
                                     #Sys.sleep(1)
                                     
                                     
                                     #tree<- drop.tip(phy = tree,
                                     #               tip = tip_to_drop)
                                     #colless <- colless.like.index(tree,norm = T)
                                     colless <- NA
                                     
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
                                     
                                     names(out_t) <- c("iteration","tips","Colless",
                                                       "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                       "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                       "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                       "Mean Functional Distance","Mean Minimum Functional Distance")
                                     
                                     
                                     
                                     #out_t <- cbind(i,100-t,phy_var,phy_mean,trait_var,trait_mean)  
                                     out_i<- rbind(out_i,out_t)
                                     
                                     
                                     #SES version
                                     phy_var_ses <- (phy_var-exp_i$mean[which(exp_i$trait=="phy_var" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_var" & exp_i$richness==100-t)]
                                     
                                     
                                     phy_var_min_ses <- (phy_var_min-exp_i$mean[which(exp_i$trait=="phy_var_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_var_min" & exp_i$richness==100-t)]
                                     
                                     phy_mean_ses <- (phy_mean-exp_i$mean[which(exp_i$trait=="phy_mean" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_mean" & exp_i$richness==100-t)]
                                     phy_mean_min_ses <- (phy_mean_min-exp_i$mean[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100-t)]
                                     
                                     
                                     trait_var_ses <-(trait_var-exp_i$mean[which(exp_i$trait=="trait_var" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_var" & exp_i$richness==100-t)]
                                     trait_var_min_ses <-(trait_var_min-exp_i$mean[which(exp_i$trait=="trait_var_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_var_min" & exp_i$richness==100-t)]
                                     
                                     trait_mean_ses <-(trait_mean-exp_i$mean[which(exp_i$trait=="trait_mean" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_mean" & exp_i$richness==100-t)]
                                     trait_mean_min_ses <-(trait_mean_min-exp_i$mean[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100-t)]
                                     
                                     out_t_ses <- cbind(i,100-t,colless,
                                                        phy_var_ses,phy_var_min_ses,
                                                        phy_mean_ses,phy_mean_min_ses,
                                                        trait_var_ses,trait_var_min_ses,
                                                        trait_mean_ses,trait_mean_min_ses)
                                     
                                     names(out_t_ses) <- c("iteration","tips","Colless",
                                                           "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                           "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                           "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                           "Mean Functional Distance","Mean Minimum Functional Distance")
                                     
                                     
                                     
                                     
                                     out_i_ses<- rbind(out_i_ses,out_t_ses)
                                     
                                     
                                   }
                                   
                                   out_limiting<-rbind(out_limiting,out_i)
                                   out_limiting_ses<-rbind(out_limiting_ses,out_i_ses)
                                   
                                   colnames(out_limiting) <- c("iteration","tips","Colless",
                                                               "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                               "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                               "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                               "Mean Functional Distance","Mean Minimum Functional Distance")
                                   
                                   colnames(out_limiting_ses) <- c("iteration","tips","Colless",
                                                                   "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                                   "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                                   "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                                   "Mean Functional Distance","Mean Minimum Functional Distance")
                                   
                                   out_limiting <- as.data.frame(out_limiting)
                                   out_limiting_ses <- as.data.frame(out_limiting_ses)
                                   out_limiting$std <- "no"
                                   out_limiting_ses$std <- "yes"
                                   
                                   out_combined <- rbind(out_limiting,out_limiting_ses)
                                   return(out_combined)
                                   
                                 }

#cleanup
rm(colless,trait_dist,tree_dist,i,traits,exp_i,out_i,out_i_ses,out_t,out_t_ses,index,
   phy_mean,phy_mean_min,phy_mean_min_ses,phy_mean_ses,phy_var,phy_var_min,phy_var_min_ses,phy_var_ses,
   tip_to_drop,trait_mean,trait_mean_min,trait_mean_min_ses,trait_mean_ses,trait_var,trait_var_min,trait_var_min_ses,trait_var_ses,t)

stop("make sure to write the file out_limiting_combined.csv using the line below")
#Split code into separate dataframe

#write.csv(x = out_limiting_combined,file = "phylogeny/simulations_limiting_20_traits.csv",row.names = F)
#out_limiting_combined <- read.csv("phylogeny/simulations_limiting_20_traits.csv",stringsAsFactors = F)
#out_limiting_combined <- out_limiting_combined[which(colnames(out_limiting_combined)!="X")]
#colnames(out_limiting_combined) <- gsub(pattern = ".",replacement = " ", x = colnames(out_limiting_combined),fixed = T)

out_limiting_ses <- out_limiting_combined[which(out_limiting_combined$std=="yes"),]
out_limiting <- out_limiting_combined[which(out_limiting_combined$std=="no"),]

out_limiting<- out_limiting[which(colnames(out_limiting)!="std")]
out_limiting_ses<- out_limiting_ses[which(colnames(out_limiting_ses)!="std")]

out_limiting$species_removed <- 100 - out_limiting$tips
out_limiting_ses$species_removed <- 100 - out_limiting_ses$tips

colnames(out_limiting_ses)[which(colnames(out_limiting_ses)=="species_removed")] <- "Species removed"
colnames(out_limiting)[which(colnames(out_limiting)=="species_removed")] <- "Species removed"



##############################################################################


#Filtering
out_filtering <- NULL
out_filtering_ses <- NULL

#for(i in 1:nreps){
out_filtering_combined <- foreach(i = 1:nreps,
                                  .combine = rbind,
                                  .packages = c("ape","phytools"))%dopar%{
                                    
                                    print(i)
                                    tree <- pbtree(n = 100)
                                    out_filtering <- NULL
                                    out_filtering_ses <- NULL
                                    
                                    #colless <- colless.like.index(tree,norm = T) #slows down code, so commenting out
                                    colless <- NA 
                                    
                                    #traits <- rTraitCont(phy = tree,model = "BM")
                                    traits <- replicate(n = 20,expr = rTraitCont(phy = tree,model = "BM"))
                                    trait_dist <- as.matrix(dist(traits,diag = T,upper = T))
                                    tree_dist <- as.matrix(cophenetic.phylo(tree))
                                    
                                    exp_i <- expected_values(tree = tree,traits = traits,n_reps = 100)
                                    rm(tree)
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
                                    
                                    
                                    #SES version
                                    phy_var_ses <- (phy_var-exp_i$mean[which(exp_i$trait=="phy_var" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_var" & exp_i$richness==100)]
                                    
                                    
                                    phy_var_min_ses <- (phy_var_min-exp_i$mean[which(exp_i$trait=="phy_var_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_var_min" & exp_i$richness==100)]
                                    
                                    phy_mean_ses <- (phy_mean-exp_i$mean[which(exp_i$trait=="phy_mean" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_mean" & exp_i$richness==100)]
                                    phy_mean_min_ses <- (phy_mean_min-exp_i$mean[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100)]
                                    
                                    
                                    trait_var_ses <-(trait_var-exp_i$mean[which(exp_i$trait=="trait_var" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_var" & exp_i$richness==100)]
                                    trait_var_min_ses <-(trait_var_min-exp_i$mean[which(exp_i$trait=="trait_var_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_var_min" & exp_i$richness==100)]
                                    
                                    trait_mean_ses <-(trait_mean-exp_i$mean[which(exp_i$trait=="trait_mean" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_mean" & exp_i$richness==100)]
                                    trait_mean_min_ses <-(trait_mean_min-exp_i$mean[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100)])/exp_i$sd[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100)]
                                    
                                    out_i_ses <- cbind(i,100,colless,
                                                       phy_var_ses,phy_var_min_ses,
                                                       phy_mean_ses,phy_mean_min_ses,
                                                       trait_var_ses,trait_var_min_ses,
                                                       trait_mean_ses,trait_mean_min_ses)
                                    names(out_i_ses) <- c("iteration","tips","Colless",
                                                          "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                          "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                          "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                          "Mean Functional Distance","Mean Minimum Functional Distance")
                                    
                                    
                                    
                                    trait_centroid <- colMeans(traits)
                                    traits_and_centroid <- rbind(traits,trait_centroid)
                                    dist_from_cent <- dist(traits_and_centroid,upper = T,diag = T)
                                    dist_from_cent <- as.matrix(dist_from_cent)
                                    dist_from_cent <- as.data.frame(dist_from_cent)
                                    dist_from_cent <- dist_from_cent['trait_centroid']
                                    dist_from_cent <- dist_from_cent[-which(rownames(dist_from_cent)=='trait_centroid'),,drop=FALSE] 
                                    
                                    
                                    #plot(traits)
                                    #abline(h=trait_centroid,col="red")
                                    
                                    for(t in 1:95){
                                      
                                      #remove a tip  
                                      #which species is the furthest from the mean
                                      
                                      tip_to_drop <- row.names(dist_from_cent)[which.max(abs(dist_from_cent$trait_centroid))]
                                      dist_from_cent <- dist_from_cent[-which(rownames(dist_from_cent)==tip_to_drop),,drop=FALSE]
                                      
                                      
                                      #abline(h = traits[which(names(traits)==tip_to_drop)])
                                      #Sys.sleep(1)
                                      
                                      
                                      #traits <- traits[-which(names(traits)==tip_to_drop)]
                                      
                                      #tree <- drop.tip(phy = tree,
                                      #                tip = tip_to_drop)
                                      #colless <- colless.like.index(tree,norm = T)
                                      colless <- NA
                                      
                                      index <- which(rownames(trait_dist)==tip_to_drop)
                                      
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
                                      
                                      names(out_t) <- c("iteration","tips","Colless",
                                                        "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                        "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                        "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                        "Mean Functional Distance","Mean Minimum Functional Distance")
                                      
                                      
                                      
                                      #out_t <- cbind(i,100-t,phy_var,phy_mean,trait_var,trait_mean)  
                                      out_i<- rbind(out_i,out_t)
                                      
                                      
                                      #SES version
                                      phy_var_ses <- (phy_var-exp_i$mean[which(exp_i$trait=="phy_var" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_var" & exp_i$richness==100-t)]
                                      
                                      
                                      phy_var_min_ses <- (phy_var_min-exp_i$mean[which(exp_i$trait=="phy_var_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_var_min" & exp_i$richness==100-t)]
                                      
                                      phy_mean_ses <- (phy_mean-exp_i$mean[which(exp_i$trait=="phy_mean" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_mean" & exp_i$richness==100-t)]
                                      phy_mean_min_ses <- (phy_mean_min-exp_i$mean[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="phy_mean_min" & exp_i$richness==100-t)]
                                      
                                      
                                      trait_var_ses <-(trait_var-exp_i$mean[which(exp_i$trait=="trait_var" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_var" & exp_i$richness==100-t)]
                                      trait_var_min_ses <-(trait_var_min-exp_i$mean[which(exp_i$trait=="trait_var_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_var_min" & exp_i$richness==100-t)]
                                      
                                      trait_mean_ses <-(trait_mean-exp_i$mean[which(exp_i$trait=="trait_mean" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_mean" & exp_i$richness==100-t)]
                                      trait_mean_min_ses <-(trait_mean_min-exp_i$mean[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100-t)])/exp_i$sd[which(exp_i$trait=="trait_mean_min" & exp_i$richness==100-t)]
                                      
                                      out_t_ses <- cbind(i,100-t,colless,
                                                         phy_var_ses,phy_var_min_ses,
                                                         phy_mean_ses,phy_mean_min_ses,
                                                         trait_var_ses,trait_var_min_ses,
                                                         trait_mean_ses,trait_mean_min_ses)
                                      
                                      names(out_t_ses) <- c("iteration","tips","Colless",
                                                            "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                            "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                            "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                            "Mean Functional Distance","Mean Minimum Functional Distance")
                                      
                                      
                                      
                                      
                                      out_i_ses<- rbind(out_i_ses,out_t_ses)
                                      
                                      
                                    }
                                    
                                    out_filtering<-rbind(out_filtering,out_i)
                                    out_filtering_ses<-rbind(out_filtering_ses,out_i_ses)
                                    
                                    colnames(out_filtering) <- c("iteration","tips","Colless",
                                                                 "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                                 "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                                 "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                                 "Mean Functional Distance","Mean Minimum Functional Distance")
                                    
                                    colnames(out_filtering_ses) <- c("iteration","tips","Colless",
                                                                     "Variance in Phylogenetic Distance","Variance in Minimum Phylogenetic Distance",
                                                                     "Mean Phylogenetic Distance","Mean Minimum Phylogenetic Distance",
                                                                     "Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                                     "Mean Functional Distance","Mean Minimum Functional Distance")
                                    
                                    out_filtering <- as.data.frame(out_filtering)
                                    out_filtering_ses <- as.data.frame(out_filtering_ses)
                                    out_filtering$std <- "no"
                                    out_filtering_ses$std <- "yes"
                                    
                                    out_combined <- rbind(out_filtering,out_filtering_ses)
                                    return(out_combined)
                                    
                                    
                                  }


#cleanup
rm(tree,colless,trait_dist,tree_dist,i,traits,exp_i,out_i,out_i_ses,out_t,out_t_ses,index,
   phy_mean,phy_mean_min,phy_mean_min_ses,phy_mean_ses,phy_var,phy_var_min,phy_var_min_ses,phy_var_ses,
   tip_to_drop,trait_mean,trait_mean_min,trait_mean_min_ses,trait_mean_ses,trait_var,trait_var_min,trait_var_min_ses,trait_var_ses,t)

#Split code into separate dataframe

#write.csv(x = out_filtering_combined,file = "phylogeny/simulations_filtering_20_traits.csv", row.names = F)
#out_filtering_combined <- read.csv(file = "phylogeny/simulations_filtering_20_traits.csv",stringsAsFactors = F)
#out_filtering_combined <- out_filtering_combined[which(colnames(out_filtering_combined)!="X")]
#colnames(out_filtering_combined) <- gsub(pattern = ".",replacement = " ", x = colnames(out_filtering_combined),fixed = T)


out_filtering_ses <- out_filtering_combined[which(out_filtering_combined$std=="yes"),]
out_filtering <- out_filtering_combined[which(out_filtering_combined$std=="no"),]

out_filtering <- out_filtering[which(colnames(out_filtering)!="std")]
out_filtering_ses<- out_filtering_ses[which(colnames(out_filtering_ses)!="std")]

out_filtering$species_removed <- 100 - out_filtering$tips
out_filtering_ses$species_removed <- 100 - out_filtering_ses$tips

colnames(out_filtering_ses)[which(colnames(out_filtering_ses)=="species_removed")] <- "Species removed"
colnames(out_filtering)[which(colnames(out_filtering)=="species_removed")] <- "Species removed"

###################################


#Limiting
limiting_summary<-list()
limiting_out_mat_r<-array(dim = c(9,9,nreps))
limiting_out_mat_p<-array(dim = c(9,9,nreps))

for(i in 1:nreps){
  
  data_i <- out_limiting[which(out_limiting$i==i),]
  corr_i <- Hmisc::rcorr(as.matrix(data_i[,4:12]))
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

##########################################################

#Limiting SES
limiting_ses_summary<-list()
limiting_ses_out_mat_r<-array(dim = c(9,9,nreps))
limiting_ses_out_mat_p<-array(dim = c(9,9,nreps))

for(i in 1:nreps){
  
  data_i <- out_limiting_ses[which(out_limiting_ses$i==i),]
  data_i$Colless<-0
  
  data_i <- na.omit(data_i)
  corr_i <- Hmisc::rcorr(as.matrix(data_i[,4:12]))
  limiting_ses_out_mat_r[,,i]<-corr_i$r
  limiting_ses_out_mat_p[,,i]<-corr_i$P
  
  
}


colnames(limiting_ses_out_mat_r)<-colnames(corr_i$r)
rownames(limiting_ses_out_mat_r)<-colnames(corr_i$r)

colnames(limiting_ses_out_mat_p)<-colnames(corr_i$r)
rownames(limiting_ses_out_mat_p)<-colnames(corr_i$r)

limiting_ses_summary[[1]] <- apply(X = limiting_ses_out_mat_r,MARGIN = c(1,2),FUN = mean)
names(limiting_ses_summary)[1] <- "mean_r"

limiting_ses_summary[[2]] <- apply(X = limiting_ses_out_mat_p,MARGIN = c(1,2),FUN = mean)
names(limiting_ses_summary)[2] <- "mean_p"

limiting_ses_summary[[3]] <- apply(X = limiting_ses_out_mat_r,MARGIN = c(1,2),
                                   FUN = function(x){  
                                     x <-sort(x)  
                                     x <- x[-1:-(nreps*.025)]  
                                     x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                     min(x)
                                   })
names(limiting_ses_summary)[3] <- "CI_min_r"

limiting_ses_summary[[4]] <- apply(X = limiting_ses_out_mat_r,MARGIN = c(1,2),
                                   FUN = function(x){  
                                     x <-sort(x)  
                                     x <- x[-1:-(nreps*.025)]  
                                     x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                     max(x)
                                   })
names(limiting_ses_summary)[4] <- "CI_max_r"


limiting_ses_summary[[5]] <- apply(X = limiting_ses_out_mat_p,MARGIN = c(1,2),
                                   FUN = function(x){  
                                     x <-sort(x)  
                                     x <- x[-1:-(nreps*.025)]  
                                     x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                     min(x)
                                   })
names(limiting_ses_summary)[5] <- "CI_min_p"

limiting_ses_summary[[6]] <- apply(X = limiting_ses_out_mat_p,MARGIN = c(1,2),
                                   FUN = function(x){  
                                     x <-sort(x)  
                                     x <- x[-1:-(nreps*.025)]  
                                     x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                     max(x)
                                   })
names(limiting_ses_summary)[6] <- "CI_max_p"

limiting_ses_summary[[7]] <- limiting_ses_summary$CI_max_r>0 & limiting_ses_summary$CI_min_r<0
names(limiting_ses_summary)[7] <- "CI_r_includes_zero"


##########################################################


#filtering
filtering_summary<-list()
filtering_out_mat_r<-array(dim = c(9,9,nreps))
filtering_out_mat_p<-array(dim = c(9,9,nreps))

for(i in 1:nreps){
  
  data_i <- out_filtering[which(out_filtering$i==i),]
  corr_i <- Hmisc::rcorr(as.matrix(data_i[,4:12]))
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


#filtering ses
filtering_ses_summary<-list()
filtering_ses_out_mat_r<-array(dim = c(9,9,nreps))
filtering_ses_out_mat_p<-array(dim = c(9,9,nreps))

for(i in 1:nreps){
  
  data_i <- out_filtering_ses[which(out_filtering_ses$i==i),]
  data_i$Colless<-0
  
  data_i <- na.omit(data_i)
  
  corr_i <- Hmisc::rcorr(as.matrix(data_i[,4:12]))
  
  filtering_ses_out_mat_r[,,i]<-corr_i$r
  filtering_ses_out_mat_p[,,i]<-corr_i$P
  
  
}


colnames(filtering_ses_out_mat_r)<-colnames(corr_i$r)
rownames(filtering_ses_out_mat_r)<-colnames(corr_i$r)

colnames(filtering_ses_out_mat_p)<-colnames(corr_i$r)
rownames(filtering_ses_out_mat_p)<-colnames(corr_i$r)

filtering_ses_summary[[1]] <- apply(X = filtering_ses_out_mat_r,MARGIN = c(1,2),FUN = mean)
names(filtering_ses_summary)[1] <- "mean_r"

filtering_ses_summary[[2]] <- apply(X = filtering_ses_out_mat_p,MARGIN = c(1,2),FUN = mean)
names(filtering_ses_summary)[2] <- "mean_p"

filtering_ses_summary[[3]] <- apply(X = filtering_ses_out_mat_r,MARGIN = c(1,2),
                                    FUN = function(x){  
                                      x <-sort(x)  
                                      x <- x[-1:-(nreps*.025)]  
                                      x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                      min(x)
                                    })
names(filtering_ses_summary)[3] <- "CI_min_r"

filtering_ses_summary[[4]] <- apply(X = filtering_ses_out_mat_r,MARGIN = c(1,2),
                                    FUN = function(x){  
                                      x <-sort(x)  
                                      x <- x[-1:-(nreps*.025)]  
                                      x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                      max(x)
                                    })
names(filtering_ses_summary)[4] <- "CI_max_r"


filtering_ses_summary[[5]] <- apply(X = filtering_ses_out_mat_p,MARGIN = c(1,2),
                                    FUN = function(x){  
                                      x <-sort(x)  
                                      x <- x[-1:-(nreps*.025)]  
                                      x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                      min(x)
                                    })
names(filtering_ses_summary)[5] <- "CI_min_p"

filtering_ses_summary[[6]] <- apply(X = filtering_ses_out_mat_p,MARGIN = c(1,2),
                                    FUN = function(x){  
                                      x <-sort(x)  
                                      x <- x[-1:-(nreps*.025)]  
                                      x <- x[-(length(x)-(nreps*.025)+1):-length(x)]  
                                      max(x)
                                    })
names(filtering_ses_summary)[6] <- "CI_max_p"

filtering_ses_summary[[7]] <- filtering_ses_summary$CI_max_r>0 & filtering_ses_summary$CI_min_r<0
names(filtering_ses_summary)[7] <- "CI_r_includes_zero"

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

corrplot(filtering_ses_summary$mean_r, method = "ellipse",
         type="full",title = "Filtering SES",
         p.mat = filtering_ses_summary$CI_r_includes_zero,
         diag = F,tl.col = "black")

corrplot(limiting_ses_summary$mean_r, method = "ellipse",
         type="full",title = "Limiting Similarity SES",
         p.mat = limiting_ses_summary$CI_r_includes_zero,
         diag = F,tl.col = "black")



