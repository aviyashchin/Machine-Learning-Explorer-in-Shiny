Correlation <- function(data){
	require(ggplot2)
	require(reshape)
	
	result <- cor(data)

	temp <- result^2
	temp <- melt(temp)
	plot_results <- ggplot(temp, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	base_size <- 14
	
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(),legend.text = element_text(size = base_size)) + ggtitle("Coefficient of Determination Heatmap"))
	#axis.text.x = element_blank(),axis.text.y = element_blank(), 
	
	#num_vars <- dim(data)[2]
	
	# combinations <- combn(c(1:num_vars), 2)
	
	# r_2 <- vector(,dim(combinations)[2])
	# corr_matrix <- as.data.frame(matrix(nrow = dim(combinations)[2], ncol = dim(combinations)[2]))
	# for (i in c(1:dim(combinations)[2])){
		# result <- lm(as.formula(paste(colnames(data)[combinations[1,i]],"~", colnames(data)[combinations[2,i]])), data)
		
		# r_2[i] <- summary(result)$r.squared 
		# corr_matrix[combinations[1,i],combinations[2,i]] <- r_2[i]
	# }
	

	

}