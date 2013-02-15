<<<<<<< HEAD
simulation <- function (sample.size, rng, ...) {
  require(moments)
  require(nortest)
  require(energy)
  source("energy_test.R")  # set replicate to 50 times for efficiency
  test <- c("shapiro.test", "energy.test", "ad.test", "cvm.test",
            "lillie.test", "pearson.test", "sf.test", "jarque.test")
  
  # generate random number
  kTimes <- 5
  set.seed(12)
  x <- replicate(n=kTimes, expr=),
                        simplify=F)
  
  require(parallel)
  source('johnson_transform.R')
  
  res <- mclapply(x, johnsonTransform, test, mc.cores=4)
  
  rownames(res) <- NULL
  distribution <- unlist(lapply(c("Log-normal", "Gamma", 
                               "Uniform", "t"), rep, times=length(test)))
  
  res <- cbind(sample.size, distribution, res)
  return(res)
}
=======
simulation <- function (sample.size) {
  require(moments)
  require(nortest)
  require(energy)
  source("energy_test.R")  # set replicate to 200 times for efficiency
  test <- c("shapiro.test", "energy.test", "ad.test", "cvm.test",
            "lillie.test", "pearson.test", "sf.test", "jarque.test")
  kTimes <- 31
  kTestNum <- 8
  
  # generate random number
  set.seed(12)
  x <- unlist(replicate(n=kTimes, 
                        expr=list(rlnorm(sample.size), 
                                  rgamma(sample.size, 3, 1),
                                  runif(sample.size), 
                                  rt(sample.size, 3)),
                        simplify=F), 
              recursive=F)
  
  require(parallel)
  source('Johnson_Transform.R')
  res <- do.call(rbind, mclapply(x, johnsonTransform, test, mc.cores=4))
  
  rownames(res) <- NULL
  distribution=unlist(lapply(c("Log-normal", "Gamma", 
                               "Uniform", "t"), rep, times=kTestNum))
  
  res <- cbind(sample.size, distribution, res)
  return(res)
  

}

# pb <- txtProgressBar(min = 0, max = kRep, style=3)
# for (i in 1:kRep) {
#   setTxtProgressBar(pb, i)
# }

# plot empirical density
# if (require(ggplot2)) {
#   p1 <- qplot(test, p.value, data=res, 
#               colour=test, geom="violin")
#   p2 <- qplot(p.value, data=res, group=test, 
#               colour=test, geom="density")
# }
>>>>>>> 12706464ccfda271df4a827ff492a2bb2dc5b0b8
