# ==============================================================================
# HW 1 - Bayesian Dynamic Modeling
# Nome : Bruno Drezza Reis de Souza
# ==============================================================================

# ==========================================
# PROBLEM 3: Beta-Binomial Update
# ==========================================

# Target mode
prior_mode <- 0.15

# Function to calculate beta_0 given alpha_0 and the mode
get_beta <- function(a, mode_val) {
  (a - 1) / mode_val + 2 - a
}

# Objective function to minimize the distance to the [0.10, 0.25] bounds (95% CI)
objective_fn <- function(a) {
  b <- get_beta(a, prior_mode)
  # Calculate squared errors of the 2.5% and 97.5% quantiles
  error_low <- (qbeta(0.025, a, b) - 0.10)^2
  error_high <- (qbeta(0.975, a, b) - 0.25)^2
  return(error_low + error_high)
}

# Optimize to find the best alpha
opt_result <- optimize(objective_fn, interval = c(1.1, 50))
alpha_0 <- opt_result$minimum
beta_0  <- get_beta(alpha_0, prior_mode)

# Print parameters
prior_params <- c(alpha_0, beta_0)
names(prior_params) <- c("Alpha_0", "Beta_0")
round(prior_params, 4)

# Plotting the Prior
par(mfrow=c(1,1))
curve(dbeta(x, alpha_0, beta_0), from = 0, to = 0.5, 
      xlab = expression(pi), ylab = "Density", 
      main = "Pamela's Prior Belief: Beta Distribution", 
      col = "blue", lwd = 2)
abline(v = prior_mode, col = "black", lty = 2)
abline(v = qbeta(c(0.025, 0.975), alpha_0, beta_0), col = "gray", lty = 3)
legend("topright", legend=c("Prior Density", "Mode (0.15)", "95% Interval") ,
       col=c("blue", "black", "gray"), lty=c(1,2,3), lwd=c(2,1,1), bty="n")


       
# Data
n <- 90
y <- 30

# Posterior Update
alpha_1 <- alpha_0 + y
beta_1  <- beta_0 + n - y

post_params <- c(alpha_1, beta_1)
names(post_params) <- c("Alpha_1", "Beta_1")
round(post_params, 4)


# Calculations
post_mean <- alpha_1 / (alpha_1 + beta_1)
post_mode <- (alpha_1 - 1) / (alpha_1 + beta_1 - 2)
post_var  <- (alpha_1 * beta_1) / (((alpha_1 + beta_1)^2) * (alpha_1 + beta_1 + 1))
post_sd   <- sqrt(post_var)

# Displaying in a neat table
summaries <- matrix(c(post_mean, post_mode, post_sd), ncol=1)
rownames(summaries) <- c("Mean", "Mode", "St. Dev")
colnames(summaries) <- c("Posterior Value")
round(summaries, 4)


mle_pi <- y / n
n_0 <- alpha_0 + beta_0

cat("Prior Effective Sample Size (n_0):", round(n_0, 2), "\n")
cat("Data Sample Size (n):", n, "\n")
cat("MLE (Data Proportion):", round(mle_pi, 4), "\n")
cat("Posterior Mode:", round(post_mode, 4), "\n")

# Graphical Comparison
par(mfrow=c(1,1))
pis = seq(0, 0.6, length=1000)
plot(pis, dbeta(pis, alpha_1, beta_1), type="l", col="red", lwd=2,
     xlab=expression(pi), ylab="Density", 
     main="Comparison: Prior vs Likelihood (MLE) vs Posterior",
     ylim=c(0, max(dbeta(pis, alpha_1, beta_1)) * 1.2))

# Add prior
lines(pis, dbeta(pis, alpha_0, beta_0), col="blue", lwd=2)

# Add MLE line
abline(v=mle_pi, col="green3", lwd=2)

legend("topright", legend=c("Prior", "Posterior", "MLE (Data)"),
       col=c("blue", "red", "green3"), lty=1, lwd=2, bty="n")

