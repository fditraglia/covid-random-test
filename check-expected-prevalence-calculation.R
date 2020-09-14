f <- function(N, k, S) {
  N * (1 - dhyper(0, S, k * N - S, k))
}

g <- function(N, k, S) {
  N * (1 - exp(lchoose(k * N - S, k) - lchoose(k * N, k)))
}

all.equal(f(3000, 6, 45), g(3000, 6, 45))
all.equal(f(3000, 6, 531), g(3000, 6, 531))


sim_draw <- function(N, k, S) {
  popn <- c(rep(0, k * N - S), rep(1, S))
  sum(rowSums(matrix(sample(popn), nrow = N, ncol = k)) > 0) 
}
nreps <- 50000
set.seed(1234)
foo <- replicate(nreps, sim_draw(3000, 6, 45))
mean(foo)
f(3000, 6, 45)
g(3000, 6, 45)


