library(rjags)

df <- pga2004  # Make sure 'pga2004' has the right data
df$log_avg_winnings <- log(df$V12)

modelString <- "
model {
  for (i in 1:N) {
    log_avg_winnings[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + inprod(beta[], x[i,])
  }
  
  for (j in 1:p) {
    beta[j] ~ dnorm(0, tau_beta[j])
    inclusion[j] ~ dbern(pi[j])
    tau_beta[j] <- pow(sigma[j], -2)
    sigma[j] <- inprod(inclusion[j], sigma1) + inprod(1 - inclusion[j], sigma2)
  }
  beta0 ~ dnorm(0, 1.0E-4)
  tau ~ dgamma(1.0E-3, 1.0E-3)
  sigma1 ~ dunif(0, 100)
  sigma2 ~ dunif(0, 100)
  for (j in 1:p) {
    pi[j] ~ dbeta(1, 1)
  }
}
"

dataList <- list(
  N = nrow(df),
  p = 7, # Number of predictors
  x = as.matrix(df[, c("V3", "V4", "V5", "V6", "V7", "V8", "V10")]),
  log_avg_winnings = df$log_avg_winnings
)

params <- c("beta0", "beta", "tau", "sigma1", "sigma2", "pi", "inclusion")

n.iter <- 5000

# Improve initial values
inits <- function(chain) {
  list(
    beta0 = rnorm(1, 0, 10),
    beta = rnorm(7, 0, 10),
    tau = rgamma(1, 0.01, 0.01),
    sigma1 = runif(1, 0, 100),
    sigma2 = runif(1, 0, 100),
    pi = rbeta(7, 1, 1),
    inclusion = rbinom(7, 1, 0.5)
  )
}

model <- jags.model(textConnection(modelString), data = dataList, inits = inits, n.chains = n.chains)
samples <- coda.samples(model, variable.names = params, n.iter = n.iter / 2)


gelman.diag(samples)
autocorr.diag(samples)
effectiveSize(samples)

summary(samples)
