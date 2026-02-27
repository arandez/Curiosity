simulate_one <- function(
    n_subj = 20,
    beta0,
    beta_conf,
    beta_conf2,
    beta_cur
) {
  n_trials_per_subj <- 180
  total_trials <- n_subj * n_trials_per_subj
  
  subj <- factor(rep(1:n_subj, each = n_trials_per_subj))
  
  # Trial-level predictors
  conf_raw <- sample(1:5, total_trials, replace = TRUE)
  conf_c   <- conf_raw - 3 #Centered
  conf_c2  <- conf_c^2
  
  cur_raw <- sample(1:5, total_trials, replace = TRUE)
  cur_c   <- cur_raw - 3 #Centered
  
  # Linear predictor
  eta <- beta0 +
    beta_conf  * conf_c +
    beta_conf2 * conf_c2 +
    beta_cur   * cur_c
  
  # Convert to probability
  p <- 1 / (1 + exp(-eta))
  
  # Simulate binary outcome
  y <- rbinom(total_trials, size = 1, prob = p)
  
  data.frame(
    subj = subj,
    conf_c = conf_c,
    conf_c2 = conf_c2,
    cur_c = cur_c,
    y = y
  )
}
set.seed(123)
run_power <- function(
    n_sims = 1000,
    alpha = 0.05,
    beta0,
    beta_conf,
    beta_conf2,
    beta_cur
) {
  sig_count <- 0
  
  for (i in 1:n_sims) {
    dat <- simulate_one(
      n_subj = 20,
      beta0 = beta0,
      beta_conf = beta_conf,
      beta_conf2 = beta_conf2,
      beta_cur = beta_cur
    )
    
    model <- glm(y ~ conf_c + conf_c2 + cur_c,
                 data = dat, family = binomial)
    
    # Extract p-value
    p_val <- summary(model)$coefficients["conf_c2", "Pr(>|z|)"]
    
    if (p_val < alpha) {
      sig_count <- sig_count + 1
    }
  }
  
  power <- sig_count / n_sims
  return(power)
}

beta0 =-.19
beta_conf =.24
beta_cur = .08

beta_conf2_grid <- c(-.025,- .04, -.06, -.08)

results <- sapply(beta_conf2_grid, function(b2) {
  run_power(
    n_sims    = 1000,
    alpha     = 0.05,
    beta0     = beta0,
    beta_conf = beta_conf,
    beta_conf2 = b2,
    beta_cur  = beta_cur
  )
})

data.frame(
  beta_conf2 = beta_conf2_grid,
  power      = results
)
