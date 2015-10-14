
Mean_Vectors <- function(data){
	require(ggplot2)
	
	 num_vars <- dim(data)[2]
	
	 output_mean <- vector(,num_vars)
	 output_se <- vector(,num_vars)
	 for (i in c(1:num_vars)){
		name <- colnames(data)[i]
		
		output_mean[i] <- mean(data[,i],na.rm = TRUE)	
		output_se[i] <- sd(data[,i],na.rm = TRUE) / sqrt(length(data[,3][!is.na(data[,3])]))
	 }

	 df <- data.frame(names = colnames(data), means = output_mean)
	 
	 limits <- aes(ymax = output_mean + output_se, ymin=output_mean - output_se)
	 p <- ggplot(df, aes(x = names, y = means))
	 p + geom_point() + geom_errorbar(limits, width=0.3) + ylab("Mean") + xlab("")

	 
	
}