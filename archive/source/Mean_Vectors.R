Mean_Vectors <- function(data, type){

  plot.new()
  output_mean = c() #need to replace this with reactive shiny
  output_se = c() #need to replace this with reactive shiny
  #ranges <- reactiveValues(y = NULL)
  ranges = c()
  ranges$y <- c(brush$ymin, brush$ymax)
  ranges$y <- c(0, 10)
  ranges$x = 1
  brush <- ranges

  #data <- sleep
   num_vars <- dim(data)[2]
  
   for (i in c(1:num_vars)){

    name <- colnames(data)[i]
    output_mean[i] <- mean(data[,i],na.rm = TRUE) 
    output_se[i] <- sd(data[,i],na.rm = TRUE) / sqrt(length(data[,3][!is.na(data[,3])]))
   }

   index <- output_mean < 100
   names_to_use <- colnames(data)
   
   df <- data.frame(names = names_to_use[index], means = output_mean[index])
   
   keep_data <- data[,index]
   keep_data <- melt(keep_data)
   
   if (type == "Scatter"){
    p <- ggplot(df, aes(x = names, y = means))
     p <- p + geom_point() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else if(type == "Scatter with error bars"){
     limits <- aes(ymax = output_mean[index] + output_se[index], ymin=output_mean[index] - output_se[index])
     p <- ggplot(df, aes(x = names, y = means))
     p <- p + geom_point() + geom_errorbar(limits, width=0.3) + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else if(type == "Violin Plot"){
    p <- ggplot(keep_data,aes(x = variable, y = value)) + geom_violin() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else{
    p <- ggplot(keep_data,aes(x = variable, y = value)) + geom_boxplot() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   }

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","MeanVectors.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Know what your data looks like. MeanVectors Plot: ",sep=""),fileloc)

  print(p)
}