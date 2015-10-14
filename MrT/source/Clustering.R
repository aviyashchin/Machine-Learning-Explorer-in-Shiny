Clustering <- function(data,num){
  plot.new()

  #select only numeric columns
  #data <- data[sapply(data, class)=="numeric"]

  clust <- hclust(dist(data), method = "complete")
  memb <- cutree(clust, k = num)

  fit <- prcomp(data, center=TRUE, scale = TRUE)
  df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = memb)  

  #Graph
  p <- ggplot(df,aes(x = x,y = y, colour = factor(z)))
  p <- p + geom_point(size = 5) + xlab('First Principal Component') + ylab('Second Principle Component') + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x = element_text(vjust = 2)) + scale_colour_discrete(name = "Clusters")  

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","Clustering.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Check out these clusters. ",sep=""),fileloc)

  print(p)
}