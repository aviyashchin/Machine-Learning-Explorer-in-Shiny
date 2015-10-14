Outliers <- function(data){
	require(ggplot2)
	require(robustbase)
	
	num_cols <- dim(data)[1]

	mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data))
	
	cutoff <- qchisq(0.95, 4, ncp = 0, lower.tail = TRUE, log.p = FALSE)
	
	outlier <- mahalanobis_dist > cutoff
	
	df_outliers <- data.frame(x = c(1:dim(data)[1]), y = log(sqrt(mahalanobis_dist)), z = outlier)
	
	p <- ggplot(df_outliers,aes(x = x,y = y))
	
	p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))
	
	
	
	
	#result <- covOGK(data, sigmamu = scaleTau2)

	#df_outliers <- data.frame(x = mahalanobis_dist, y = result$distances)

	#p <- ggplot(df_outliers,aes(x = x,y = y)) 
	#p + geom_point() + geom_abline(intercept = 0, slope = 1,linetype="dashed") + labs(x = "Mahalanobis Distances",y = "Robust Distances", title = paste("Robust Distances versus Mahalanobis Distance"))
		
	

	 
	
}