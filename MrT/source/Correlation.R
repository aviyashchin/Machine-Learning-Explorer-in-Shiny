Correlation <- function(data){
  plot.new()

  #select only numeric columns
  #data <- data[sapply(data, class)=="numeric"]

  #  data_t <- data[,order(colnames(data))]
  temp <- cor(data)
  temp[lower.tri(temp)] <- NA
  temp <- melt(temp)
  temp <- na.omit(temp)

  p <- ggplot(temp, aes(x=Var1, y=Var2, fill = value)) + geom_tile(alpha = 0.75, colour = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Pearson\ncorrelation\n")

  p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Correlation Heatmap")

  p <- p + geom_text(aes(Var1, Var2, label = round(value,2)), color = "black", size = 4)+  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(.5, 0),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
    title.position = "right", title.hjust = 0.5))

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","CorrelationPlots.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Look at your correlations fool! ",sep=""),fileloc)

  print(p)
}