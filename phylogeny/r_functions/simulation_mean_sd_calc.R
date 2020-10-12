#null values for simulations

#columns:

  #richness, metric, mean, sd


expected_values <- function(tree,traits,n_reps){
  
  
  trait_dist <- as.matrix(dist(traits,diag = T,upper = T))
  tree_dist <- as.matrix(cophenetic.phylo(tree))
  
  
  purrr::map_df(100:5,
                function(x){get_exp_means_sds(n_tips = x,n_reps = nreps,
                                              trait_dist = trait_dist,
                                              tree_dist = tree_dist)})
  

}



  get_exp_means_sds <- function(n_tips,n_reps,trait_dist,tree_dist){
    

    reps <- replicate(n = n_reps,expr = get_stuff(tree = tree,n_tips = n_tips,trait_dist = trait_dist,tree_dist = tree_dist),simplify = T)

    means <- rowMeans(reps)  
    
    sds <- apply(X = reps,1,FUN = sd)
    
    
    names <- c("phy_var","phy_var_min",
               "phy_mean","phy_mean_min", 
               "trait_var","trait_var_min", 
               "trait_mean","trait_mean_min")
    
    out <- data.frame(trait=names,richness=n_tips,mean=means,sd=sds)
    
    return(out)
  }
  
  
  
  
  
  
get_stuff <- function(tree,n_tips,trait_dist,tree_dist){
    
  
  
    samp <- sample(x = length(tree$tip.label),size = n_tips,replace = F)
    tree$tip.label[samp]
    
    phy_var <- var(tree_dist[samp,samp][upper.tri(tree_dist[samp,samp],diag = F)])
    phy_var_min <- var(apply(X = tree_dist[samp,samp],MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    phy_mean <- mean(tree_dist[samp,samp][upper.tri(tree_dist[samp,samp],diag = F)])
    phy_mean_min <- mean(apply(X = tree_dist[samp,samp],MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    trait_var <-var(trait_dist[samp,samp][upper.tri(trait_dist[samp,samp],diag = F)])
    trait_var_min <- var(apply(X = trait_dist[samp,samp],MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))
    
    trait_mean <-mean(trait_dist[samp,samp][upper.tri(trait_dist[samp,samp],diag = F)])
    trait_mean_min <- mean(apply(X = trait_dist[samp,samp],MARGIN = 1,FUN = function(x){min(x[which(x!=0)])}))  
    
    return(cbind(phy_var,phy_var_min,
          phy_mean,phy_mean_min, 
          trait_var,trait_var_min, 
          trait_mean,trait_mean_min)  )
  
  
}  
  