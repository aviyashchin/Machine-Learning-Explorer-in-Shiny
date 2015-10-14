Scree_Plot <- function(data){
	require(fastcluster)
	require(shiny)

	clust <- hclust(dist(data), method = "complete")

	memb <- cutree(clust, k = 10)
	
	fit <- prcomp(data, center=TRUE, scale = TRUE)
	
	df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = memb)
	
	
	p <- ggplot(df,aes(x = x,y = y))
	
	p + geom_point(aes(colour = z)) 
	
	
	
	+ geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))
	
	
	
	# cent <- NULL
	# for(k in 1:10){
		# cent <- rbind(cent, colMeans(data[memb == k, , drop = FALSE]))
	# }
	
	
	
}