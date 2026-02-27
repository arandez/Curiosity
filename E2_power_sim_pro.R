simulate_one <- function(
    n_subj = 30,
    beta0,
    beta_envir,
    beta_block,
    beta_int
) {
  n_trials_per_subj <- 180
  total_trials <- n_subj * n_trials_per_subj
  
  subj <- factor(rep(1:n_subj, each = n_trials_per_subj))
  
  # Trial-level predictors
  block_raw <- sample(1:6, total_trials, replace = TRUE)
  block_0   <- block_raw - 1
  
  envir_raw <- sample(1:2, total_trials, replace = TRUE)
  envir_0   <- envir_raw - 3
  
  # Interaction
  interaction <- block_0 * envir_0
  
  # Linear predictor
  eta <- beta0 +
    beta_envir * envir_0 +
    beta_block  * block_0 +
    beta_int  * interaction
  
  # Convert to probability
  p <- 1 / (1 + exp(-eta))
  
  # Simulate binary outcome
  y <- rbinom(total_trials, size = 1, prob = p)
  
  data.frame(
    subj = subj,
    block_0 = block_0,
    envir_0 = envir_0,
    interaction = interaction,
    y = y
  )
}

beta0 =0.12599
beta_envir =-0.90844
beta_block = -0.6458
beta_int = 0.42928

run_power <- function(
    n_sims = 1000,
    alpha = 0.05,
    beta0,
    beta_envir,
    beta_block,
    beta_int
) {
  sig_count <- 0
  
  for (i in 1:n_sims) {
    dat <- simulate_one(
      n_subj = 30,
      beta0 = beta0,
      beta_envir = beta_envir,
      beta_block = beta_block,
      beta_int = beta_int
    )
    
    model <- glm(y ~ block_0 * envir_0,
                 data = dat, family = binomial)
    
    # Extract p-value for the interaction
    p_val <- summary(model)$coefficients["block_0:envir_0", "Pr(>|z|)"]
    
    if (p_val < alpha) {
      sig_count <- sig_count + 1
    }
  }
  
  power <- sig_count / n_sims
  return(power)
}
set.seed(123)

run_power(
  n_sims = 1000,
  alpha = 0.05,
  beta0 = beta0,
  beta_envir = beta_envir,
  beta_block = beta_block,
  beta_int = beta_int
)
