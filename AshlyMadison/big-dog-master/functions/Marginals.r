
Marginals <- function(data){
	require(ggplot2)
		
	p <- ggplot(data, aes_q(x = as.name(name))) + geom_density()		

}