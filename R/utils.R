

# Compute the proportion/ratio of correct responses
# for the responses vectors of dim (Items * Persons). 
test_ratio <- function(x, n_items){
  rowMeans(matrix(x, nrow = n_items))
}

# Compute the total number of Correct responses
# for the response vectors
total_correct <- function(x, n_items){
  colSums(matrix(x, nrow = n_items))
}

# Simulate responses only from the training data from the parsed model
# The pooling model requires a different handling
# Returns the predicted Bernoulli response and the probability
simulate_obs <- function(model, data, pooling, n_sims=1000){
  
  N <- nrow(data)
  post_sim <- arm::sim(model, n_sims)
  
  y.sim <- array(NA, c(N, n_sims))
  if (pooling){
    X <- model.matrix(model)
    p.sim <- sapply(1:n_sims, function(s) invlogit(X %*% coef(post_sim)[s,]))
  } else {
    p.sim <- fitted(post_sim, model)
  }
  
  for (s in 1:n_sims){
    y.sim[,s] <- rbinom(N, 1, p.sim[,s])
  }
  assertthat::are_equal(N, nrow(y.sim))
  assertthat::are_equal(n_sims, ncol(y.sim))
  return(list(sim_labels = y.sim, sim_preds = p.sim))
}


# Create the histograms for the Mean and SD for training data
# Returns a list of two plots for the parsed simulations matrix
summary_stat_plot <- function(y_sims, y_real, model, n_items, n_learners, n_sims = 1000) {
  true.mean <- mean(total_correct(y_real, n_items))
  true.sd <- sd(total_correct(y_real, n_items))
  
  sim_n.correct <- sapply(1:n_sims, function(s) total_correct(y_sims[,s], n_items))
  
  sim.means <- data.frame(mean_correct = colMeans(sim_n.correct))
  mean_summary <- mean(sim.means$mean_correct)
  
  sim.sds <- data.frame(sd_correct = apply(sim_n.correct, 2, sd))
  sd_summary <- mean(sim.sds$sd_correct)
  
  sim.means %>%
    ggplot(aes(x = mean_correct)) + geom_histogram(bins = 30, color = "darkgray", fill = "white", size = 0.5) +
    geom_vline(aes(xintercept = true.mean, color = "True Mean"), size = 1.5) +
    geom_vline(aes(xintercept = mean_summary + 0.06, color = "Simulations Mean"), size = 1) +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(breaks = seq(53, 55, 1)) +
    scale_color_discrete(name = "") + #, values = c(`Simulations Mean` = "green", `True Mean` = "red")) +
    labs(title = model, x = "Mean of Correct Responses") -> mean_plot
  
  sim.sds %>%
    ggplot(aes(x = sd_correct)) + geom_histogram(bins = 30, color = "darkgrey", fill = "white", size = 0.5) +
    geom_vline(aes(xintercept = sd_summary, color = "Simulations SD"), size = 1.5) +
    geom_vline(aes(xintercept = true.sd, color = "True SD"), size = 1.5) + theme_classic() +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_color_discrete(name = "") + #, values = c(`Simulations SD` = "navy", `True SD` = "red4")) + 
    labs(title = model, x = "SD of Correct Responses") -> sd_plot
  
  return(list(mean_plot = mean_plot, sd_plot = sd_plot))
}
