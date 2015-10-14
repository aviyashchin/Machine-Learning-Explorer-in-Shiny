Scree_Plot <- function(data){
	result <- prcomp(data, center = TRUE, scale = TRUE)
	retained_variance <- cumsum(unlist(result[1])^2) /  max(cumsum(unlist(result[1])^2))
	
	df <- data.frame(x = c(1:dim(data)[2]), y = retained_variance)
	
	p <- ggplot(df, aes(x = x,y = y))
	p + geom_point()
}