simulation <- function (sample.size) {
	require(moments)
	require(nortest)
# 	require(energy)
# 	source("energy_test.R")  # set replicate to 50 times for efficiency
 	test <- c("shapiro.test", "ad.test", "cvm.test",
			  "lillie.test", "pearson.test", "sf.test", "jarque.test")
	
	
	# generate random number
	kTimes <- 100
	set.seed(kTimes)
	x <- unlist(replicate(n=kTimes, 
						  expr=list(rlnorm(sample.size), 
						  		  rgamma(sample.size, 3, 1),
						  		  runif(sample.size), 
						  		  rt(sample.size, 3)),
						  simplify=F), 
				recursive=F)
	# use simplify and recursive to make an organized list
	
	# load johnson source functions
	source('johnson_transform.R')
	res <- do.call(rbind, lapply(x, johnsonTransform, test))
	
	dist <- c(mapply(rep, c("Log-normal", "Gamma", "Uniform", "t"), 
					 times=length(test)))
	
	res <- cbind(sample.size, dist, res)
	return(res)
}





