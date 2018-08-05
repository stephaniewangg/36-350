generate_data <- function(n,p) {
  covariates <-(matrix(runif(n*p),n,p))
  responses <- (as.vector(runif(n)))
  list(covariates,responses)
}

model_select <- function(covariates, responses, cutoff) {
  reg=lm(responses~covariates)
  result=(summary(reg)$coefficients[which(summary(reg)$coefficients[c("covariates","responses"),4]) <= cutoff])
  result
}

run_simulation <- function(n_trials, n, p, cutoff) {
  for (i in 1:3) {
    v=vector()
    n = n[i]
    p = p[i]
    for (j in 1:n_trials) {
      data = generate_data(n,p)
      vals=(model_select(data[[1]],data[[2]],cutoff))
      v=c(v, vals)
    }
    hist(v)
  }
}