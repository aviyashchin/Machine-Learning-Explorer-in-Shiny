
# /** 
#  * [I PITY THE FOOL Project]
#  * @type {[R Script]}
#  *
#  * To run me:
#  * source("/Users/avi/Dropbox/programming/boxer/AshlyMadison/AviYashchin_Project_1.1.R")
#  *
#  */

#Load Dependency Libraries
  library(caret)
  library(gbm)
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
  library(C50)  #chrun dataset
  library(VIM)
  library(mailR)
  library(kknn)
  library(PASWR)
  library(Hmisc)  
  library(shiny)
  library("ggthemes")
  #rm(list = ls())    <- DELETES ALL OBJECTS

#Checking for more than 5% missing values in cols
pMiss <- function(x){sum(is.na(x))/length(x)*100}


.load_Constants <- function(){
  #These should be monitored from Shiny
  DEBUG <<- TRUE   #Clear workspace with rm(list=objects()) 
  PATH <<- "/Users/avi/boxer/MrTStaging/"
  IMGPATH <<- "/Users/avi/boxer/MrTStaging/plots/"
  BASE_SIZE <<- 100
  Independent <<- "numsbs"
#  source("/Users/avi/Dropbox/programming/boxer/MrT/source/")

  #If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out. We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function
  MISSING_COLS_FOR_REMOVAL <<- 15
  MISSING_ROWS_FOR_REMOVAL <<- 15
  OUTLIER_CUTOFF_P_VALUE <<- 5
  NUM_CLUSTERS <<- 4
  TYPE <<- "violin"  #Options: c("kd","hist","Violin Plot")
  NUM_FACTORS_WE_CAN_HANDLE <<- 6
  TRAIN_TEST_SPLIT <<- .75

  #GBM parameters
  NUMBER_OF_TREES <<- 1000
  SPLITS_PER_TREE <<- 5
  LEARNING_RATE_TREE <<- 0.01

  #Max Factors Allowed
  MAX_FACTORS_ALLOWED <<- 10

}

loadSourceRFiles <- function(trace = TRUE, ...) {
  .load_Constants()
  for (nm in list.files(paste(PATH,"/source/",sep=""), pattern = "[.][RrSsQq]$")) {
     if(trace) cat(nm,":")
     source(file.path(paste(PATH,"/source/",sep=""), nm), ...)
     if(trace) cat("\n")
  }
}


removeTextAndWhitespace <- function(df){
  #REmove factors with more than NUM_FACTORS_WE_CAN_HANDLE factors
  #  df <- titanic3
  
  #  columns <- df[1,]; # store the column names for future reference 
  #  row_names <- df[,1]; 
  #  names(df) <- colnames(df) 
  
  # cut rows beyond 100 
  #  if (dim(df)[2]>100)
  #  {
  #  df <- df[,1:100] # chop dataframe down 
  #  }
  
  slackr("Here's the data we're starting with:",str(df))
  
  dataTypes <- vector(mode="character", length=dim(df)[2])  # define a vector to hold each columns data type 
  # we loop through each column and determine its type 
  
  for (i in 1:dim(df)[2]){
    # first task is to scrub the data 
    df[,i] <- gsub(" ", "", df[,i]) # remove spaces 
    df[,i] <- tolower(df[,i])
    # check to make sure there are no na n/a and we missed this as continuous data 
    na_indi <- which(df[,i] =="na" | df[,i]=="n/a")
    if (length(na_indi) > 0 ){ # we found some Nas
      df[na_indi,i] <- NA
    }
    
    na_indi <- sum(is.na(df[,i])) # get initial count of na indices 
    
    # check if it is numeric by converting to it 
    test <- df[,i] # holder variable 
    test <- as.numeric(test) 
    na_indi2 <- sum(is.na(test))
    
    if (na_indi2>na_indi){ #must be characters 
      dataTypes[i] <- "character"  
    } else { 
      dataTypes[i] <- "double"
      df[,i] <- test
      #add function so if data is only 0 or 1 is cast to a factor.  
      if(class(df[1,i])=="integer"){
        if(max(df[,i])==1 && min(df[,i])==0)
          df[,i] <- (df[,i]==1) #cast the DF column into a logical factor
      }
    }
  }
  
  # we now look to convert to factors 
  for (i in 1:(dim(df)[2])){
    if (dataTypes[i] == "character"){
      dataTypes[i] = "factor"
      df[,i] <- as.factor(df[,i])
      if (nlevels(df[,i]) > NUM_FACTORS_WE_CAN_HANDLE){ # bad column and we delete 
        # df[,i] <- NULL # remove column 
        dataTypes[i] <- 0 # mark to remove data type
      }      
    }
  }
  
  #Columns to remove
  r_indi <- which(dataTypes == 0)
  
  #Rejected columns go to "removedColumns"
  removedColumns <- df[,c(r_indi)]
  
  if(length(r_indi)>0){
    SUCKAtxt = paste(sep="",collapse="\n")
    slackr(eval(SUCKAtxt))
    SUCKAtxt = paste("I'm removing columns '",list(colnames(df)[dataTypes == 0]),"'. Find someone smarter than me to analyze columns with more than ",NUM_FACTORS_WE_CAN_HANDLE," factors. Call Owen Zhang @ datarobot.com, or try H2o.ai for model building, wit.ai for speech, or cortical.io for basic text comparison NLP, or torch.ch for RNN/CNN.  Here's what was taken out:",sep="")
    slackr(eval(SUCKAtxt))
    slackr(str(removedColumns))
  }
  
  #Only keep Integers, Logical, and factors <NUM_FACTORS_WE_CAN_HANDLE columns
  dfreturn <- df[,-c(r_indi)]
  
  return(dfreturn)
}

#Delete me? 
LinearModelAnalysis <- function(){

  # Create categories of Rurual codes and Urban Influence Codes
  # category_list=c("Rural_urban_continuum_code_2013","Urban_influence_code_2013")
  FIPS$Rural_urban_continuum_code_2013 <- as.factor(FIPS$Rural_urban_continuum_code_2013)
  FIPS$Urban_influence_code_2013 <- as.factor(FIPS$Urban_influence_code_2013)

  # Metropolitan Counties*  Rural_urban_continuum_code_2013
  # Code  Description
  # 1 Counties in metro areas of 1 million population or more
  # 2 Counties in metro areas of 250,000 to 1 million population
  # 3 Counties in metro areas of fewer than 250,000 population
    
  # Nonmetropolitan Counties  
  # 4 Urban population of 20,000 or more, adjacent to a metro area
  # 5 Urban population of 20,000 or more, not adjacent to a metro area
  # 6 Urban population of 2,500 to 19,999, adjacent to a metro area
  # 7 Urban population of 2,500 to 19,999, not adjacent to a metro area
  # 8 Completely rural or less than 2,500 urban population, adjacent to a metro area
  # 9 Completely rural or less than 2,500 urban population, not adjacent to a metro area


  # Metropolitan Counties*  - URBAN-INFLUENCE-CODE
  # Code  
  # 1 In large metro area of 1+ million residents
  # 2 In small metro area of less than 1 million residents
    
  # Nonmetropolitan Counties  
  # 3 Micropolitan area adjacent to large metro area
  # 4 Noncore adjacent to large metro area
  # 5 Micropolitan area adjacent to small metro area
  # 6 Noncore adjacent to small metro area and contains a town of at least 2,500 residents
  # 7 Noncore adjacent to small metro area and does not contain a town of at least 2,500 residents
  # 8 Micropolitan area not adjacent to a metro area
  # 9 Noncore adjacent to micro area and contains a town of at least 2,500 residents
  # 10  Noncore adjacent to micro area and does not contain a town of at least 2,500 residents
  # 11  Noncore not adjacent to metro or micro area and contains a town of at least 2,500 residents
  # 12  Noncore not adjacent to metro or micro area and does not contain a town of at least 2,500 residents
    #go through and add numeric items into correlation table


  ########################################################################################################
  #    NEED TO BUIULD A FUNCTION FOR CREATING FACTORS OUT OF REGULAR COLUMNS
  ########################################################################################################
  #mtcars$am <- as.factor(mtcars$am)

  ########################################################################################################
  #    NEED TO BUILD FUNCTIONS FOR DICING COLUMNS
  ########################################################################################################

  #    SEPARATES CONTINUOUS AND CATEGORICAL VARIABLES
    	j=2  #start at 2 because NewGuys first row is already initialized
      k=2
    	continuousVariables<-as.data.frame(FIPS$FIPS)
    	categoricalVariables<-as.data.frame(FIPS$FIPS)
    	#find non-numeric names
       	for (i in 1:length(colnames(FIPS))){
    	   print(paste("coltype ",i," ",colnames(FIPS)[i]," ",class(FIPS[,i])))
       	  if (class(FIPS[,i])=="integer"){ 
       	    continuousVariables[j]<-FIPS[i]
       	    j<-j+1
       	  }else{
         	    if (class(FIPS[,i])=="numeric"){ 
         	      continuousVariables[j]<-FIPS[i]
         	      j<-j+1
         	    }else{
         	      categoricalVariables[k]<-FIPS[i]
                k<-k+1
              }
       	  }
       	}
    
    	#Which columns to use
    	MakeMePerCapita <- c("All FilingsTotal",
    	                     "Business Filings Total",
    	                     "Nonbusiness Filings Total", 
    	                     "Number of establishments",
    	                     #"Paid employees for pay period including March 12 (number)", 
    	                     #"first_q_payroll",
    	                     #"annual_payroll", 
    	                     "N_POP_CHG_2014",
    	                     "Births_2014", 
    	                     "Deaths_2014", 
    	                     "NATURAL_INC_2014",
    	                     "INTERNATIONAL_MIG_2014",
    	                     "DOMESTIC_MIG_2014", 
    	                     "NET_MIG_2014",
    	                     "RESIDUAL_2014", 
    	                     "GQ_ESTIMATES_2014", 
    	                     "POVALL_2013", 
    	                     "POV017_2013",
    	                     "POV517_2013", 
    	                     #"POV05_2013",
    	                     "Civilian_labor_force_2014",
    	                     "Employed_2014",
    	                     "Unemployed_2014",
    	                     "SBs")
    	percapita<-FIPS[2]
  i=1
    	for (i in 1:length(MakeMePerCapita)){
    	    print(paste("MakeMePerCapita ",i," ",colnames(continuousVariables[eval(MakeMePerCapita[i])])))
      	  ((continuousVariables[,eval(MakeMePerCapita[i])]=continuousVariables[,eval(MakeMePerCapita[i])]/continuousVariables[,"POP_ESTIMATE_2014"]))
    	  #rename columns that have been adjusted to Per Capita Columns
      	  names(continuousVariables)[names(continuousVariables)==eval(MakeMePerCapita[i])] <- paste(eval(MakeMePerCapita[i]),"PerCapita",sep="")
    	  }

    	  	#Remove NAs- REDUCES ZIP CODES FROM 3,100 TO 1,100.  YIKES
    	final = filter(continuousVariables, !is.na(continuousVariables$SBsPerCapita))    
    	#sum(final$SBsPerCapita)
    	
    	#filter for less than 100 SB's per county
    	#final = filter(final, final$SBstest>100)    
    	#sum(final$SBstest)
     #Find correlation matrix of SB's

  #  SBMatrix<-cor(FIPS, method = "spearman", use = "pairwise")
    SBMatrix<-cor(final, method = "spearman", use = "pairwise")
  #  sort(SBMatrix[nrow(SBMatrix),])
}

#Delete me? 
CheckCorrs <- function(){
  #------- CHECK FOR THESE FOR EACH OF THE INPUT VARIABLES  ----------
  #➢ Linearity
  #➢ Constant Variance
  #➢ Normality  
  #➢ Independent Errors

  Sig=final[1]
  NoSig=final[1]
  #i=3
  i=1
  #find non-numeric names
  x <- final$SBsPerCapita
  for (i in 1:nrow(SBMatrix)){
    y <- final[eval(colnames(SBMatrix)[i])][,1]
    qplot(x, y, geom = "smooth")
    qplot(x, y)
    #aovr <- summary(aov(x ~ y),na.rm="TRUE") #Conducting the One-Way ANOVA on the weight
    aovr <- summary(aov(x ~ y,na.rm="TRUE")) #Conducting the One-Way ANOVA on the weight
    if (aovr[[1]][5][1,]<0.025){ 
      print(paste("SIGNIFICANT Name:",colnames(SBMatrix)[i]," Class:",class(SBMatrix[,i])))
      Sig[eval(colnames(SBMatrix)[i])] <- final[i]
    }else{
      print(paste("INSIGNIFICANT Name:",colnames(SBMatrix)[i]," Class:",class(SBMatrix[,i])))
      NoSig[eval(colnames(SBMatrix)[i])] <- final[i]
    }
  }

  # #remove missing values
  # impute.na <- function(x){
  #   return (sapply(x, function(f){is.na(f)<-which(f == '');f}))
  # }
  # ft <- sapply(final,function(f){is.na(f)<-which(f == '');f})

  # count number of NAs per row  and remove rows with more than 3 NAs
  numNAs_by_row <- apply(ft, 1, function(z) sum(is.na(z)))
  ft <- ft[!(numNAs_by_row >= 3),]
}

#Rewrite me. 
AICCANalysis <- function(){
  ########################################################################
  #boxcox transofrms
  ft$SBsPerCapita 
  ft$SBsPerCapita

  SBmodel.empty = lm(SBsPerCapita ~ 1, data = as.data.frame(ft)) #The model with an intercept ONLY.
  SBmodel.empty.bc = boxCox(SBmodel.empty)
  cb1_lambda <- SBmodel.empty.bc$x[SBmodel.empty.bc$y ==max(SBmodel.empty.bc$y)]
  print(paste("ML Box-Cox estimate for forward model:",cb1_lambda))
  ft$SBsPerCapitaBoxCoxAdd=bcPower(ft$SBsPerCapita, cb1_lambda)

  SBmodel.full = lm(SBsPerCapita ~ ., data = as.data.frame(ft)) #The model with ALL variables.
  SBmodel.full.bc = boxCox(SBmodel.full)
  cb2_lambda <- SBmodel.full.bc$x[SBmodel.full.bc$y ==max(SBmodel.full.bc$y)]
  print(paste("ML Box-Cox estimate for backward model:",cb2_lambda))
  bcPower(ft$SBsPerCapita, cb2_lambda)

  summary(SBmodel.full.bc)

  ########################################################################

  scope = list(lower = formula(SBmodel.empty), upper = formula(SBmodel.full))

  library(MASS) #The Modern Applied Statistics library.

  #Stepwise regression using AIC as the criteria (the penalty k = 2).
  forwardAIC = step(SBmodel.empty, scope, direction = "forward", k = 2)
  #best model - SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita + pol09 + Rural_urban_continuum_code_2013 + `VarCoe1990: Coef of var for household income, 1990` + respn10 + GQ_ESTIMATES_2014PerCapita + golf09 + POV517_2013PerCapita + PCTPOVALL_2013

  backwardAIC = step(SBmodel.full, scope, direction = "backward", k = 2)
  bothAIC.empty = step(SBmodel.empty, scope, direction = "both", k = 2)
  bothAIC.full = step(SBmodel.full, scope, direction = "both", k = 2)

  #Stepwise regression using BIC as the criteria (the penalty k = log(n)).
  forwardBIC = step(SBmodel.empty, scope, direction = "forward", k = log(50))
  #simple model - SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita
  backwardBIC = step(SBmodel.full, scope, direction = "backward", k = log(50))
  bothBIC.empty = step(SBmodel.empty, scope, direction = "both", k = log(50))
  bothBIC.full = step(SBmodel.full, scope, direction = "both", k = log(50))

  SBmodel.forw <- lm(SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita, data = as.data.frame(ft))


  SBmodel.back <- lm(SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita + pol09 + Rural_urban_continuum_code_2013 + `VarCoe1990: Coef of var for household income, 1990` + respn10 + GQ_ESTIMATES_2014PerCapita + golf09 + POV517_2013PerCapita + PCTPOVALL_2013, data = as.data.frame(ft))

  summary(SBmodel.forw)
  summary(SBmodel.back)
  plot(SBmodel)

  summary(lm(Sig$SBsPerCapita ~ CI90LBALLP_2013 + PCTPOVALL_2013 + no_exercise_percent, data = Sig))
  plot(lm(Sig$SBsPerCapita ~ CI90LBALLP_2013 + PCTPOVALL_2013 + no_exercise_percent +`Theil1990: Theil index of income disparity, 1990`, data = Sig))
}

#Delete me.
Prettyqplot<-function(xs,ys){
  qplot(eval(xs), ys, geom = "smooth")
  qplot(eval(xs), ys)
  
  # Deciding on logical breaks for temperature factor
  labels <- c("low", "med", "high")
  minW <- min(eval(xs),na.rm=TRUE)
  maxW <- max(eval(xs),na.rm=TRUE)
  RangeW <- (maxW-minW)
  breaksW <- c(minW, minW + RangeW/3, minW + 2 * RangeW/3, maxW)
  # Adding columns with temperature groups
  Wtemp <- cut(xs, breaks = breaksW, include.lowest = TRUE)
  datas$xaxis=xs
  ggplot(datas, aes(xs, ys)) +
    geom_point(size = 5) +
    geom_point(aes(col = Wtemp), size = 4) +
    scale_color_manual(values = c("skyblue3", "lightgoldenrod1","indianred1"),
                       name="", 
                       labels = c("< 24ºF", "24-46ºF", ">46ºF")) +
    theme_hc() + 
    ylab("Homeless Rate Change (percentage points)") + 
    xlab("Mental Health Budget Change (percentage points)")+
    ggtitle("State Homeless Rate Change by MH Budget Change, 2009-2013") 
}

# Linear_Regression <- function(data,num){
#   plot.new()


#   #Saves the file to the drive, and emails the file out & to slack. 
#   fileloc <- paste(IMGPATH,"/","Linear_Regression.jpg",sep="")
#   ggsave(filename = fileloc, plot = p)
#   Email_file_to_Slack(paste("Linear Regression Model goes here ",sep=""),fileloc)

#   print(p)
# }

MultiVariate_Regression2 <- function(data,dependentVar){
# data = idata.notText
# depend = dependentVar
data = read.table("04NYCRestaurants.txt")
data = removeTextAndWhitespace(data)
dependentVar = "Price"

  plot.new()

model.empty.text <- paste(dependentVar," ~ ")
mycolnames <- colnames(data)[colnames(data) != "Temp"]
model.saturated.text <- paste(model.empty.text,paste(mycolnames,collapse=" + "),sep="")

model.saturated = lm(model.saturated.text, data = data)
model.empty = lm(paste(model.empty.text," 1",sep=""), data = data)

summary(model.saturated)
#3
par(mfrow=c(2,2))
plot(model.saturated)

#No overt deviations from any of the assumptions.

#4
library(car)
influencePlot(model.saturated)

#There are a few points that surface with either high residuals or high hat-values,
#but there are none that have a severe negative influence on the regression surface.

#5
vif(model.saturated)

#The VIF of Service is about 3.6, which is decently high. As we saw in the
#scatterplot matrix above, we might be cautious about multicollinearity in our
#data because of correlations among our predictor variables.

#6
avPlots(model.saturated)

#The food, decor, and location variables all seem to add some type of information
#to our model; however, the service variable doesn't seem to add any information
#when among the other predictors in this model.

#7
model.service = lm(Price ~ Service, data = restaurants)
summary(model.service)

#In the simple linear regression model, service is a highly significant variable
#that helps to predict the price of dinner; however, because of multicollinearity
#with other variables such as the food rating, in a multiple regression setting
#this variable tends to fall out of significance because it doesn't add any
#additional information.



#####################
#####Question #2#####
#####################

#1abcde
model2 = lm(Price ~ Food + Decor + Location, data = restaurants)
summary(model2)
plot(model2)
influencePlot(model2)
vif(model2)
avPlots(model2)

#All coefficients are significant, and the overall regression is significant. The
#adjusted R^2 value increased a bit while the RSE decreased slightly. There are
#a couple observations that are outliers but there are no extreme deviations from
#any of the regression assumptions. The VIF and added variable plots show that
#we don't have any cause for concern of multicollinearity in our model.

#2
anova(model2, model.saturated)

#The p-value for this partial F-test is 0.995, indicating that we should retain
#the null hypothesis. Thus, we have no reason to believe that the coefficient
#of the service variable is non-zero, and we should use the model that does not
#contain this variable.

#3
model.reduced = lm(Price ~ Food + Decor, data = restaurants)
summary(model.reduced)
plot(model.reduced)
influencePlot(model.reduced)
vif(model.reduced)
avPlots(model.reduced)

#The reduced model doesn't violate any assumptions.

#4
AIC(model.saturated, model2, model.reduced)

#The AIC for the saturated model without the service variable is lowest.

#5
BIC(model.saturated, model2, model.reduced)

#The BIC for the reduced model is the lowest.

#6

#We would expect to see these results since BIC tends to prefer a more
#parsimonious model; however, we note that the BIC for the saturated model
#missing the service variable and the BIC for the reduced model are very close.

  forwardAIC = step(SBmodel.empty, scope, direction = "forward", k = 2)
  #best model - SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita + pol09 + Rural_urban_continuum_code_2013 + `VarCoe1990: Coef of var for household income, 1990` + respn10 + GQ_ESTIMATES_2014PerCapita + golf09 + POV517_2013PerCapita + PCTPOVALL_2013

  backwardAIC = step(SBmodel.full, scope, direction = "backward", k = 2)
  bothAIC.empty = step(SBmodel.empty, scope, direction = "both", k = 2)
  bothAIC.full = step(SBmodel.full, scope, direction = "both", k = 2)

  #Stepwise regression using BIC as the criteria (the penalty k = log(n)).
  forwardBIC = step(SBmodel.empty, scope, direction = "forward", k = log(50))
  #simple model - SBsPerCapita ~ PCTPOV017_2013 + R_death_2014 + INTERNATIONAL_MIG_2014PerCapita
  backwardBIC = step(SBmodel.full, scope, direction = "backward", k = log(50))
  bothBIC.empty = step(SBmodel.empty, scope, direction = "both", k = log(50))
  bothBIC.full = step(SBmodel.full, scope, direction = "both", k = log(50))










  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","MultiVariate_Regression.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("MultiVariate Regression Goes here",sep=""),fileloc)

  print(p)
}


MultiVariate_Regression <- function(data,dependentVar){
  # data = idata.notText
  # depend = dependentVar
  data = removeTextAndWhitespace(data)
  
  plot.new()
  
  model.empty.text <- paste(dependentVar," ~ ")
  mycolnames <- colnames(data)[colnames(data) != "Temp"]
  model.saturated.text <- paste(model.empty.text,paste(mycolnames,collapse=" + "),sep="")
  
  model.saturated = lm(model.saturated.text, data = data)
  model.empty = lm(paste(model.empty.text," 1",sep=""), data = data)
}


Logistic_Regression <- function(data,num){
  plot.new()

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","MultiVariate_Regression.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("MultiVariate Regression Goes here",sep=""),fileloc)

  print(p)
}

#my spotify playlist https://open.spotify.com/user/shanonlev/playlist/3mAMTVobhui7WNAnGowQQrFruits_Vegetables[,1]=as.numeric(Fruits_Vegetables[,1])

# ShinyServer <- function(){
#     show_outliers <<- reactiveValues(Names = NULL, Distances = NULL)
#     show_outliers = c();
#   }

main <- function(){
  setwd(PATH)
  loadSourceRFiles()
  slackrSetup(config_file = paste(PATH,'/gitignore/slackr.dcf',sep=""))  #to send txt to slack.
  #save.image("MrT.RData")
  #load("MrT.RData")

  #idata <- titanic3  #missing data 
  dependentVariable <- "survived"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- airquality   #missing data 
  dependentVariable <- "Solar.R"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- iris
  dependentVariable <- "Species"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- state.x77
  dependentVariable <- "Life.Exp"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- sleep
  dependentVariable <- "Sleep"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- loadAllAMMysqlData()
  idata <- AMData
  dependentVariable <- "SBsPerCapita"  #SHOULD BE A FUNCTION IN SHINY

  #idata <- churnTrain
  dependentVariable <- "churn"  #SHOULD BE A FUNCTION IN SHINY
  #idata[,dependentVariable] <- ifelse(idata[,dependentVariable] == "yes", 1, 0)

  #replace all variable name whitespaces with a dot"
  colnames(idata) <- gsub(" ", ".", colnames(idata), fixed = TRUE)

  #for GBMClassification
  if(length(levels(idata[,dependentVariable])) == 2){
    GBMdistribution <<- "bernoulli" # For classification
  }else if (is.null(levels(idata[,dependentVariable]))){
    GBMdistribution <<- "gaussian" # For classification
  } else {
    GBMdistribution <<- "multinomial" # For classification
  }

#DATAMUNGING  PRE-PROCESSING

  #Check for missingness (NA's, remove columns if there's too much missing data)
  idata <- Missingness_Analysis(idata)

  #select only numeric columns
  idata.num <- idata[sapply(idata, class)=="numeric"]
  idata.int <- idata[sapply(idata, class)=="integer"]
  idata.factor <- idata[sapply(idata, class)=="factor"]
  idata.float <- idata[sapply(idata, class)=="complex"]
  idata.logic <- idata[sapply(idata, class)=="logical"]
  idata.notText <- cbind(idata.num,idata.int,idata.float,idata.logic,idata.factor)

  #CLEANS OUT ANY VARIABLE WITH TOO MANY OF DIFFERENT LEVELS
  for(i in 1:ncol(idata.notText)){
    tmpname=colnames(idata)[i]
    if(length(levels(idata[,eval(tmpname)])) > MAX_FACTORS_ALLOWED){
      idata.notText[,eval(tmpname)] <- NULL
    }
  }
  idata <- idata.notText

  #run K_means or average on the remaining NA's in the data
  idata <- CleanBlanksAndNAs(idata,Method="avg")  #check this with AM dataset

  #Remove any columns that have a correlation of 1 with the value were trying to investigate
  #SBMatrix<-cor(idata, method = "spearman", use = "pairwise")

  #dummies <- dummyVars(survived ~ ., data = etitanic)
  #head(predict(dummies, newdata = etitanic))

  #SPLIT TRAINING AND TESTING DATA
  inTrainingSet <- createDataPartition(idata[,dependentVariable],p = TRAIN_TEST_SPLIT, list = FALSE)
  iTrain    <- idata[inTrainingSet,]
  iTest     <- idata[-inTrainingSet,]

  #preProcess calculates values that can be used to apply to any data set (e.g. training, set, unknowns).
  numerics <- c(colnames(idata)[apply(idata,2,class)=="numeric"])

  #idata <- AMTrain 
  #idata <- TitanTrain
  #idata <- TitanTrain
  #idata <- ozoneTrain

#  as.matrix()?

  procValues <- preProcess(iTrain[,numerics],method = c("center", "scale", "YeoJohnson"))
  ## Use the predict methods to do the adjustments
  trainScaled <- predict(procValues, iTrain[,numerics])
  testScaled <- predict(procValues, iTest[,numerics])
  slackr(eval(procValues))

#LINEAR REGRESSION ANALYZE DATA/MODEL BUILDING
  forGBM <- as.data.frame(iTrain)
  predict_formula <- as.formula(paste(dependentVariable," ~ .",sep=""))

  gbmFit <- gbm(formula = predict_formula, # Use all predictors
   distribution = GBMdistribution,
   data = forGBM,
   n.trees = NUMBER_OF_TREES, # 2000 boosting iterations
   interaction.depth = SPLITS_PER_TREE, # How many splits in each tree
   shrinkage = LEARNING_RATE_TREE, # learning rate
   verbose = FALSE) # Do not print the details

  #Split predictors and dependent variables
  predictors <- names(idata)[names(idata) != dependentVariable]

  summary(gbmFit)

  gbmTune <- train(predict_formula, data = iTrain,method = "gbm")

   # or, using the formula interface
   gbmTune <- train(predict_formula, data = churnTrain, method = "gbm")

  #Linear_Regression(idata,idata.notText)  #broken
  
  MultiVariate_Regression(idata.notText,"Price")  #broken

  Generalized_Linear_Models(idata,idata.notText)  #broken

  #Check for time series
  ARIMATests(idata)  #broken


#DATAVISUALIZATION  #should come back to this once the regressions are complete. 
  PlotMarginals(idata,dependentVariable,"comb")

  Outliers(idata.notText,OUTLIER_CUTOFF_P_VALUE)

  Scree_Plot(idata.notText)

  Correlation(idata.notText)

  Mean_Vectors(idata.notText,"Violin Plot")

  Clustering(idata.notText,NUM_CLUSTERS)

#CHECKING SIGNIFICANT, CHECKING FOR OVERFITTING FUNCTIONS

  #MODEL VALIDATION    

  #➢ Linearity
  # Not linear? Does Tranform help?

  #➢ Constant Variance
  # Not linear? Does Tranform help?

  #➢ Normality
  #Checking for normality of Dependent Variable
  #Run Box-Cox Transformation on dependent Variable

  #➢ Independent Error
}

