
Normal_Check <- function(data){
	require(ggplot2)
	require(Matching)
	
	 num_vars <- dim(data)[2]
	
	 output <- vector(,num_vars)
	 for (i in c(1:num_vars)){
		name <- colnames(data)[i]
		m <- mean(data[,i]) 
		s <- sd(data[,i]) 

		result <- ks.boot(data[,i], rnorm(100,m,s)) 

		
	 }

}