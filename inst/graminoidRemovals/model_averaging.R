#### Functions ####

# thanks to (find blog name) for vifmer function...
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


# function to select and average mixed effects models when given a full model. Prints dredge table if print = TRUE. Outputs model estimates, standard errors, and confidence intervals.

model.average <- function(mod, percent.thresh, print) {
  
  mod.select <- dredge(mod, rank = "AICc")
  if(print == TRUE){
    print(mod.select)
    print(importance(mod.select))
  }
  avg.mod <- model.avg(mod.select, cumsum(weight) <= 0.95, fit = TRUE)
  summary.mod.avg <- data.frame(summary(avg.mod)$coefmat.full)
  summary.mod.avg <- summary.mod.avg %>%
    rownames_to_column(var = "explan.var") %>%
    select(explan.var, Estimate, St.error = Std..Error) %>%
    mutate(CI.upper = Estimate + (St.error * 1.96), CI.lower = Estimate - (St.error * 1.96))
  summary.mod.avg
  #write.csv(summary.mod.avg, file = paste0(mod, ".csv", sep = ","))
  
}

model.importance <- function(mod) {
  
  mod.select <- dredge(mod, extra = "adjR^2", rank = "AICc")
  
  i <- as.data.frame(importance(mod.select)) %>%
    rownames_to_column(var = "Explanatory_variables")
  return(i)
}

# function to plot the estimates of all variables returned by model averaging
plot.estimates <- function(output, title) {
  p <- ggplot(output, aes_string(x = "explan.var", y = "Estimate"))
  print(p + geom_boxplot() +
          geom_errorbar(aes_string(ymin = "CI.lower", ymax = "CI.upper")) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme(axis.text = element_text(angle = 90)) +
          ggtitle(title)
  )
}
