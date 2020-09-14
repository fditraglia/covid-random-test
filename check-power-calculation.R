# Check power calculation
get_sample <- function(n, C, N = 20000) {
  popn <- c(rep(0, N - C), rep(1, C))
  out <- sample(popn, size = n, replace = FALSE)
  return(out)
}

get_test_stat <- function(x, p0, N = 20000) {
  est <- mean(x)
  n <- length(x)
  K <- (N - n) / (N - 1)
  SE <- sqrt(K * p0 * (1 - p0) / n)
  out <- (est - p0) / SE
  return(out)
}

set.seed(1234)
nreps <- 100000

# Simulated test statistics under the null of 50 cases
sims0 <- replicate(nreps, get_test_stat(get_sample(1400, 50), 50 / 20000))

# Simulated test statistics under the alternative of 100 cases versus the null of 50
sims1 <- replicate(nreps, get_test_stat(get_sample(1400, 100), 50 / 20000))

# Simulated test statistics under the alternative of 200 cases versus the null of 50
sims2 <- replicate(nreps, get_test_stat(get_sample(1400, 200), 50 / 20000))

# Check size and power
mean(sims0 > qnorm(0.9)) # slightly over-sized: discreteness problem
mean(sims1 > qnorm(0.9)) # and consequently over-powered...
mean(sims2 > qnorm(0.9))

