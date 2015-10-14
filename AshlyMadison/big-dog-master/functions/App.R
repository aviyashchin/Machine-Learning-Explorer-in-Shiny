library(shiny)
library(ggplot2)
library(robustbase)
library(reshape)
library(xlsx)

DataScrubbing <- function(file_name) 
{
  # This function takes an excel spreadsheet with the first row as labels. It will cut off all rows beyond 100  
  # for the columns with both continuous/Discrete and categorical data and returns 
  # a data set with the first column as the header 
  
  file_name <- paste(file_name,".xlsx",sep="") # add excel extenstion 
  library(xlsx) # import excel library for reading 
  df <- read.xlsx(file_name,1,header=FALSE,stringsAsFactors = FALSE)
  columns <- df[1,]; # store the column names for future reference 
  df <- df[2:dim(df)[1],] # remove the first row 
  row_names <- df[,1]; 
  names(df) <- columns 
  
  # cut rows beyond 100 
  if (dim(df)[2]>100)
  {
  df <- df[,1:100] # chop dataframe down 
  }
  
  
  dataTypes <- vector(mode="character", length=dim(df)[2])  # define a vector to hold each columns data type 
  # we loop through each column and determine its type 
  for (i in 1:dim(df)[2])
  {
    # first task is to scrub the data 
    df[,i] <- gsub(" ", "", df[,i]) # remove spaces 
    df[,i] <- tolower(df[,i])
    # check to make sure there are no na n/a and we missed this as continuous data 
    na_indi <- which(df[,i] =="na" | df[,i]=="n/a")
    if (length(na_indi) > 0 ) # we found some Nas 
    {
      df[na_indi,i] <- NA
    }
    
    na_indi <- sum(is.na(df[,i])) # get initial count of na indices 
    
    # check if it is numeric by converting to it 
    test <- df[,i] # holder variable 
    test <- as.numeric(test) 
    na_indi2 <- sum(is.na(test))
    
    if (na_indi2>na_indi) #must be characters 
    {
      dataTypes[i] <- "character"
      
    } else 
    { 
        dataTypes[i] <- "double"
        df[,i] <- test
        
    }
  }
  
  # we now look to convert to factors 

  for (i in 1:(dim(df)[2]))
  {
    if (dataTypes[i] == "character")
    {
      dataTypes[i] = "factor"
      df[,i] <- as.factor(df[,i])
      if (nlevels(df[,i]) > 6) # bad column and we delete 
      {
        # df[,i] <- NULL # remove column 
        dataTypes[i] <- 0 # mark to remove data type
      }
      
    }
  }
  r_indi <- which(dataTypes == 0)
  df[,r_indi] <- NULL 
  dataTypes <- dataTypes[-r_indi] 
  df <- cbind(row_names,df)
  return(list(dataTypes,df))
}

input <- DataScrubbing("home/ec2-user/big-dog/public/data/bbqpizza")

input_data <- input[[2]]

row_names <- input_data[,1]

input_data[,1] <- NULL

data <- input_data

# Define UI for application that plots random distributions 
shinyUI(navbarPage("Big Dog Analytics", id = "tabs",
  tabPanel("Marginal Distributions", value = "MD",
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
	selectInput(inputId = "col_names",
				label = "Select",
				colnames(data)), 
				
	selectInput(inputId = "show_type",
				label = "Select",
				list("Histogram" = "hist", 
				 "Kernel Density" = "kd", 
				 "Combined" = "comb")) 
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("MarginalPlot")
  )  
  ),
  tabPanel("Outlier Analysis", value = "OA",
	sidebarPanel(
		sliderInput(inputId = "pval", label = "Rejection P-Value", min=0, max=10, value=5, step = 1)
	),
  mainPanel(
    plotOutput("Outliers")
  )
  ),
  tabPanel("Correlation Analysis", value = "CA",
	sidebarPanel(),
  mainPanel(
    plotOutput("Corr", hover = "plot_hover"
        ),
		verbatimTextOutput("hover$info")
	)
   ),
  tabPanel("Mean Vector", value = "MV",
	sidebarPanel(),
  mainPanel(
    plotOutput("Mean_o")
  )
  )
))

shinyServer(function(input, output) {
  
  Marginals <- function(data,name,type){	
	if (type == "hist"){
		p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(fill = "deepskyblue2", alpha = 0.2, color = "white") + title("Marginal Distribution") + ylab('Counts')
	} else if (type == "kd"){
		p <- ggplot(data, aes_q(x = as.name(name))) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
	}
	else{
		 p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(aes(y = ..density..), fill = "deepskyblue2", color = "white", alpha = 0.2) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
	}
	
	p <- p + theme(text = element_text(size=20))
			 
  }
  
  Outliers <- function(data,cutoff_in){
  
	num_cols <- dim(data)[1]

	mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data), ,tol=1e-20)
	
	cutoff <- qchisq(1 - cutoff_in / 100, dim(data)[2], ncp = 0, lower.tail = TRUE, log.p = FALSE)
	
	outlier <- mahalanobis_dist > cutoff
	
	df_outliers <- data.frame(x = c(1:dim(data)[1]), y = log(sqrt(mahalanobis_dist)), z = outlier)
	
	p <- ggplot(df_outliers,aes(x = x,y = y))
	
	p <- p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))	
	
	p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20))
  }
  
  Correlation <- function(data){
	data_t <- data[,order(colnames(data))]
	result <- cor(data_t)

	temp <- result
	temp[lower.tri(temp)] <- NA
	temp <- melt(temp)
	temp <- na.omit(temp)
	
	p <- ggplot(temp, aes(X2, X1, fill = value)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_gradient2(low = "steelblue", high = "red", mid = "violet", midpoint = 0, limit = c(-1,1), name = "Pearson\ncorrelation\n")
	base_size <- 14
	
	p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Correlation Heatmap")
	
	p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_blank(), axis.text.y = element_blank(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
	
	#+ geom_text(aes(X2, X1, label = round(value,2)), color = "black", size = 10)

  }
  
  Mean_Vectors <- function(data){
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
	 p <- p + geom_point() + geom_errorbar(limits, width=0.3) + ylab("Mean") + xlab("")	
  }
  
  output$MarginalPlot <- renderPlot({
    p <- Marginals(data,input$col_names,input$show_type)
    print(p)
  })
  
  output$Outliers <- renderPlot({
	p <- Outliers(data,input$pval)
	print(p)
  })
  
  output$Corr <- renderPlot({
	p <- Correlation(data)
	print(p)
  })
  
  output$Mean_o <- renderPlot({
	p <- Mean_Vectors(data)
	print(p)
  })
  
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str('hi')
  })
})

