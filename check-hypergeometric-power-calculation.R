# Null hypothesis of 35 positives in a population of 3000
# Sample size of 300
# Reject if the sample contains more than 5 positives
# Two alternatives: 70 positives and 105 positives

sim_test_stat <- function(C, n = 300, N = 3000) {
  popn <- c(rep(0, N - C), rep(1, C))
  sum(sample(popn, n, replace = FALSE))
}

nreps <- 1e5

set.seed(1234)
T0 <- replicate(nreps, sim_test_stat(35)) # Null
mean(T0 > 5)

set.seed(1234)
T1 <- replicate(nreps, sim_test_stat(70)) # First alternative
mean(T1 > 5)

set.seed(1234)
T2 <- replicate(nreps, sim_test_stat(105)) # Second alternative
mean(T2 > 5)
