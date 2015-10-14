
Marginals <- function(data){
	require(ggplot2)
	
	 num_vars <- dim(data)[2]
	
	 for (i in c(1:num_vars)){
		name <- colnames(data)[i]
		dev.new()
		p <- ggplot(data, aes_q(x = as.name(name)))
		print(p + geom_density())
		
	 }

}