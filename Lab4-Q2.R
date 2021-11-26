load("chemical.RData")
chemical_df <- data.frame(X, Y)
plot(X, Y)
mc_gibbs <- function(step, Y, sig) {
  mu0 <- rep(0, 50)
  Y_est <- rep(0, 50)
  mu_est <- rep(0, 50)
  gibbs_result <- rep(0, 50)
  for (tstep in 1:(step-1)) {
    for (i in 1:50) {
      if (i==1) {
        mu_est <- mean(mu0[i+1],Y[i])
        Y_est[i] <- rnorm(1, mean = mu_est, sd = sig/sqrt(2))
      } else if (i == 50) {
        mu_est <- mean(Y_est[i-1],Y[i])
        Y_est[i] <- rnorm(1, mean = mu_est, sd = sig/sqrt(2))
      } else {
        mu_est <- mean(Y[i],Y_est[i-1], mu0[i+1])
        Y_est[i]<- rnorm(1, mean = mu_est, sd = sig/sqrt(3))
      }
    }
  }
  return(Y_est)
}

gibbs_output <- mc_gibbs(1000, chemical_df$Y, sqrt(0.2))

plot(X, Y)
lines(gibbs_output)