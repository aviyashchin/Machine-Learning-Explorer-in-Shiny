Scree_Plot <- function(data){

  plot.new()
  #select only numeric columns
  #data <- data[sapply(data, class)=="numeric"]

  result <- prcomp(data, center = TRUE, scale = TRUE)
  retained_variance <- cumsum(unlist(result[1])^2) /  max(cumsum(unlist(result[1])^2))
  
  df <- data.frame(x = c(1:dim(data)[2]), y = retained_variance)
  
  p <- ggplot(df, aes(x = x,y = y)) + xlab('Retained Dimensions') + ylab('Explained Variance') + ggtitle('Scree Plot')
  p <- p + geom_point() + geom_line() + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=45)) 

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","PlotScreePlot.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Scree Plots Incoming! ",sep=""),fileloc)

  plot(p)
  print(p)
}