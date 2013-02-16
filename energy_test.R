energy.test <- function (x, R=99) {
  if (is.vector(x)) {
    n <- length(x)
    d <- 1
    bootobj <- boot(x, statistic = normal.e, R = R, sim = "parametric", 
                    ran.gen = function(x, y) {
                      return(rnorm(n))
                    })
  }
  else {
    n <- nrow(x)
    d <- ncol(x)
    bootobj <- boot(x, statistic = mvnorm.e, R = R, sim = "parametric", 
                    ran.gen = function(x, y) {
                      return(matrix(rnorm(n * d), nrow = n, ncol = d))
                    })
  }
  p <- 1 - mean(bootobj$t < bootobj$t0)
  names(bootobj$t0) <- "E-statistic"
  e <- list(statistic = bootobj$t0, p.value = p, method = "Energy test of 
  		  multivariate normality: estimated parameters", 
            data.name = paste("x, sample size ", n, ", dimension ", 
                              d, ", replicates ", R, sep = ""))
  class(e) <- "htest"
  e
}