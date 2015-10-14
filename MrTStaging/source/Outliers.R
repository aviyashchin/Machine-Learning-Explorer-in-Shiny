Outliers <- function(data,cutoff_in){
  #numeric only
  #data <- idata
  #replace this with the function that CMakris showed us
  #data <- cars
  #data <- idata
  plot.new()

  #select only numeric columns
  #data <- data[sapply(data, class)=="numeric"]

  mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data), ,tol=1e-20)
  
  cutoff <- qchisq(1 - cutoff_in / 100, dim(data)[2], ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  outlier <- mahalanobis_dist > cutoff
  
  df_outliers <<- data.frame(x = c(1:dim(data)[1]), y = log(sqrt(mahalanobis_dist)), z = outlier)

  outlier_list <- df_outliers[df_outliers$z==TRUE,]

  #show_outliers$Names <<- row_names[df_outliers[,3]]
  #show_outliers$Distances <<- mahalanobis_dist[df_outliers[,3]]

  p <- ggplot(df_outliers,aes(x = x,y = y))
  
  p <- p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))  
  
  p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20))

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","PlotOutliers.jpg",sep="")
  ggsave(filename = fileloc, plot = p)

  Email_file_to_Slack(paste("Based on your cutoff, you have ",nrow(df_outliers[df_outliers$z==TRUE,])," outliers. ",sep=""),fileloc)

  print(p)
}