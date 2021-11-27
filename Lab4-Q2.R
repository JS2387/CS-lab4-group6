load("chemical.RData")
chemical_df <- data.frame(X, Y)
plot(X, Y)
mc_gibbs <- function(step, Y, sig) {
  mu_i <- 0
  result_matrix <- matrix(0, nrow = 50, ncol = step)
  for (tstep in 1:(step-1)) {
    for (i in 1:50) {
      if (i==1) {
        mu_i <- mean(result_matrix[i+1,tstep],Y[i])
        result_matrix[i,tstep+1] <- rnorm(1, mean = mu_i, sd = sig/sqrt(2))
      } else if (i == 50) {
        mu_i <- mean(result_matrix[i-1,tstep+1],Y[i])
        result_matrix[i,tstep+1] <- rnorm(1, mean = mu_i, sd = sig/sqrt(2))
      } else {
        mu_i <- mean(Y[i],result_matrix[i-1,tstep+1], result_matrix[i+1,tstep])
        result_matrix[i,tstep+1] <- rnorm(1, mean = mu_i, sd = sig/sqrt(3))
      }
    }
  }
  return(result_matrix)
}

gibbs_output <- mc_gibbs(1000, chemical_df$Y, sqrt(0.2))
gibbs_result <- cbind(chemical_df, rowMeans(gibbs_output))
names(gibbs_result) <- c("X", "Y", "expected_mu")
plot(X, Y)
lines(gibbs_result$expected_mu, col = "red")

vN <- 1:ncol(gibbs_output)
vX <- gibbs_output

plot(vN,vX[50,],pch=19,cex=0.3,col="black",xlab="X",ylab="Y", ylim=c(0,3.5), type = 'l')
abline(h=1)
abline(h=2)
abline(h=1.5)