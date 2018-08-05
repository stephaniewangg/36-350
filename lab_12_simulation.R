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
