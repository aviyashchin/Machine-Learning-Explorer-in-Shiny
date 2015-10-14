library(shiny)
library(ggplot2)
library(robustbase)
library(xlsx)

# Define UI for application that plots random distributions 
shinyUI(navbarPage("Big Dog Analytics", id = "tabs",
  tabPanel("Data", value = "D",
	mainPanel(
		dataTableOutput(outputId="table")
	)  
  ),
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
		sliderInput(inputId = "pval", label = "Rejection P-Value", min=0, max=10, value=5, step = 1),
		dataTableOutput(outputId="outlier_info")
	),
  mainPanel(
    plotOutput("Outliers")
		
  )
  ),
  tabPanel("Correlation Analysis", value = "CA",
  mainPanel(
    plotOutput("Corr", width = "150%",height = "1200px")
	)
   ),
  tabPanel("Mean Vector", value = "MV",
	sidebarPanel(
	selectInput(inputId = "mean_type",
				label = "Select Type of Plot",
				list("Scatter", "Scatter with error bars", "Box Plot","Violin Plot")
				) 
	 ),
  mainPanel(
    plotOutput("Mean_o", height = "800px", dblclick = "plot1_dblclick",
        brush = brushOpts(
          id = "plot1_brush",
          resetOnNew = TRUE))
  )
  ),  
  tabPanel("Clustering", value = "C",
	sidebarPanel(
		plotOutput("Scree")
	),
  mainPanel(
  	sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step = 1),
    plotOutput("Clust")	
	)
   )
))
