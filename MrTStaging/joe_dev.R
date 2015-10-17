
# /** 
#  * [I PITY THE FOOL Project]
#  * @type {[R Script]}
#  *
#  * To run me:
#  * source("/Users/avi/Dropbox/programming/boxer/AshlyMadison/AviYashchin_Project_1.1.R")
#  *
#  */

#Load Dependency Libraries
library(dplyr)
library(slackr)
library(reshape2)
library(ggplot2)
library(lattice)
library(DBI)
library(RMySQL)
library(plyr)
library(PASWR)
library(mice)
library(VIM)
library(mailR)
library(kknn)
library(PASWR)
library(Hmisc)  
library(shiny)
library("ggthemes")


.load_Constants <- function(){
  #These should be monitored from Shiny
  DEBUG <<- FALSE   #Clear workspace with rm(list=objects()) 
  #  PATH <<- "~/boxer/MrT"
  #  IMGPATH <<- "~/boxer/MrT/plots"
  
  PATH <<- "~/Documents/boxer/MrTStaging"
  IMGPATH <<- "~/Documents/boxer/MrTStaging/plots"
  BASE_SIZE <<- 100
  #  source("/Users/avi/Dropbox/programming/boxer/MrT/source/")
  
  #If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out. We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function
  MISSING_COLS_FOR_REMOVAL <<- 15
  MISSING_ROWS_FOR_REMOVAL <<- 15
  OUTLIER_CUTOFF_P_VALUE <<- 5
  NUM_CLUSTERS <<- 4
  TYPE <<- "violin"  #Options: c("kd","hist","Violin Plot")
  NUM_FACTORS_WE_CAN_HANDLE <<- 6
}


loadSourceRFiles <- function(trace = TRUE, ...) {
  .load_Constants()
  for (nm in list.files(paste(PATH,"/source/",sep=""), pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(paste(PATH,"/source/",sep=""), nm), ...)
    if(trace) cat("\n")
  }
}


AMData = loadAllAMMysqlData() # AShley MAdison Data
missing = sapply(AMData, function(x) sum(is.na(x)))
colnames(AMData)
nrow(AMData)
ncol(AMData)

