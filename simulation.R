rng <- function(size) {
	# generate random number
	kTimes <- 100
	set.seed(0712)
	x <- c(replicate(n=kTimes, expr=rlnorm(size), simplify=F), 
		   replicate(n=kTimes, expr=rgamma(size, 3, 1), simplify=F),
		   replicate(n=kTimes, expr=runif(size), simplify=F),
		   replicate(n=kTimes, expr=rt(size, 3), simplify=F))
	
	a <- do.call(rbind, x)
	row.names(a) <- c(sapply(list("Log-normal", "Gamma", "Uniform", "t"), 
							 rep, times=kTimes))
	write.csv(a, paste("data/data", size, ".csv", sep=""))
	return(x)
}

simulation <- function (size) {
	require(moments)
	require(nortest)
	require(energy)
	source("energy_test.R")  # set replicate to 99 times for efficiency
	test <- c("shapiro.test", "ad.test", "energy.test", "cvm.test",
			  "lillie.test", "pearson.test", "sf.test", "jarque.test")
	
	x <- rng(size)
	
	# load johnson source functions
	require(parallel)
	source('johnson_transform.R')
	res <- do.call(rbind, mclapply(x, johnsonTransform, test))
	return(res)
	dist <- rep(c("Log-normal", "Gamma", "Uniform", "t"), times=length(test))
	
	res <- cbind(size, dist, res)
	return(res)
}



size <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 
		  500, 1000, 1500, 2000, 5000)
res <- data.frame()
pb <- txtProgressBar(min = 0, max = length(size), style=3)
for (i in 1:length(size)) {
	d <- cbind(size=size[i], simulation(size[i]))
	res <- rbind(res, d)
	setTxtProgressBar(pb, i)
}

