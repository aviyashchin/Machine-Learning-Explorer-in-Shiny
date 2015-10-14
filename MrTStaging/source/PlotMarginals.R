PlotMarginals <- function(data,name,type){
  # list("Histogram" = "hist", 
  #  "Kernel Density" = "kd", 
  #  "Combined" = "comb")) 
  #data <- iris
  #name <- "Sepal.Length"

  plot.new()
  print(name)

  if (type == "hist"){
      p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(fill = "deepskyblue2", alpha = 0.2, color = "white") + title("Marginal Distribution") + ylab('Counts')
  } else if (type == "kd"){
      p <- ggplot(data, aes_q(x = as.name(name))) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
  } else {
       p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(aes(y = ..density..), fill = "deepskyblue2", color = "white", alpha = 0.2) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
  }
  p <- p + theme(text = element_text(size=20))
  
  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","PlotMarginals.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Let's take a look at how your independent variable (",name,") is distributed ",sep=""),fileloc)

  print(p)
}