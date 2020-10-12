

replicate_phylo_glmm <- function(tree_list, formula, data,family ="binomial"){
        out_list<-list()
       
        for(i in 1:length(tree_list)){
          tree <- read.tree(tree_list[i])

          out_list[[i]] <- 
          
          pglmm(formula = formula ,
                data = data,
                family = family,
                cov_ranef = list(species_i = tree))    
          
        }
        
        
    return(out_list)
  
}


summarize_replicates <- function( phylo_reps_output){
  
  out <- array(dim = c(length(phylo_reps_output[[1]]$B),
                        3,
                        length(phylo_reps_output)))

  colnames(out)<- c("estimate","se","p_val")
  rownames(out)<- rownames(phylo_reps_output[[1]]$B)
  aic_list <- NULL
  
  
  for(i in 1:length(phylo_reps_output)){
    
    out[,,i] <- cbind(phylo_reps_output[[i]]$B, #estimate
                      phylo_reps_output[[i]]$B.se, #standard error 
                      phylo_reps_output[[i]]$B.pvalue #pvale
                      )
    aic_list <- c(aic_list,phylo_reps_output[[i]]$AIC)
    
    
  } #i loop 
  

  
  out_mean <- apply(X = out,MARGIN = c(1,2),FUN = mean)
  aic_mean <- mean(aic_list)
  
  out_list<-list(means=out_mean,aic_mean=aic_mean)    
  return(out_list)  
}
