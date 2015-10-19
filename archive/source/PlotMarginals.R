PlotMarginals <- function(data,name,type){
  # list("Histogram" = "hist", 
  #  "Kernel Density" = "kd", 
  #  "Combined" = "comb")) 
  #data <- iris
  #name <- dependentVariable
  type <- "comb"
  #data <- idata

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


#Source: http://www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/
# #placeholder plot - prints nothing at all
# empty <- ggplot()+geom_point(aes(1,1), colour="white") +
#      theme(                              
#        plot.background = element_blank(), 
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), 
#        panel.border = element_blank(), 
#        panel.background = element_blank(),
#        axis.title.x = element_blank(),
#        axis.title.y = element_blank(),
#        axis.text.x = element_blank(),
#        axis.text.y = element_blank(),
#        axis.ticks = element_blank()
#      )

# #scatterplot of x and y variables
# scatter <- ggplot(xy,aes(xvar, yvar)) + 
#   geom_point(aes(color=zvar)) + 
#   scale_color_manual(values = c("orange", "purple")) + 
#   theme(legend.position=c(1,1),legend.justification=c(1,1)) 

# #marginal density of x - plot on top
# plot_top <- ggplot(xy, aes(xvar, fill=zvar)) + 
#   geom_density(alpha=.5) + 
#   scale_fill_manual(values = c("orange", "purple")) + 
#   theme(legend.position = "none")

# #marginal density of y - plot on the right
# plot_right <- ggplot(xy, aes(yvar, fill=zvar)) + 
#   geom_density(alpha=.5) + 
#   coord_flip() + 
#   scale_fill_manual(values = c("orange", "purple")) + 
#   theme(legend.position = "none") 

# #arrange the plots together, with appropriate height and width for each row and column
# grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))