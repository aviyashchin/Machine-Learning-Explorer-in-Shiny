CleanBlanksAndNAs <- function(FIPS,Method){

#K_means_Clustering <- function(FIPS){

  ################################################################################  
  # Impute leftovers with K Means clustering
  #
  ################################################################################  

  #odata <- titanic3
  #odata <- airquality
  #odata <- data
  #odata <- idata
  odata <- FIPS
  #odata <- FIPS.Missing     

  #only build correlation on complete columns 
  missingCols <- apply(odata,2,pMiss)
  compColNames <- names(odata[missingCols==0])  #no missing data
  missColNames <- names(odata[missingCols>0])  # any missing data

  if(length(missColNames)!=0){
    #colnames(odata)[sapply(odata, class)=="character"]
    for (i in 1:length(missColNames)){
      gdata <- odata[,c(compColNames)]
      bdata <- odata[,c(missColNames)]

      #move the current columns from "Bad columns" to "good columns"
      current_var <- missColNames[i]
      gdata[,current_var] <- bdata[,current_var]
      bdata[,current_var] <- NULL
      if(class(gdata[1,current_var][[1]][1][[1]])=="character"){
        print("Trying to find missingness in char field!")        
      }
      #only train and test using complete columns
      train <- gdata[!is.na(gdata[,current_var]),]
      test  <- gdata[!is.na(gdata[,current_var]),]

      if(Method=="knn"){
        regression_formula <- as.formula(paste(current_var," ~ .",sep=""))
        kknn1 <- kknn(regression_formula, train, test, k = 1, distance = 1)
        kknn2 <- kknn(regression_formula, train, test, k = 1, distance = 2)
        kknn5 <- kknn(regression_formula, train, test, k = 1, distance = 3)

        a1 <- data.frame(test = kknn1$fitted.values, TYPE = "kmeans k=1 dist=1")
        a2 <- data.frame(test = kknn2$fitted.values, TYPE = "kmeans k=1 dist=2")
        a3 <- data.frame(test = kknn5$fitted.values, TYPE = "kmeans k=1 dist=3")
        a4 <- data.frame(test = c(train[current_var]),TYPE="original")

        colnames(a1)[1] <- current_var
        colnames(a2)[1] <- current_var
        colnames(a3)[1] <- current_var
        colnames(a4)[1] <- current_var

        newdata <- list(a1,a2,a3,a4)
        q3.4 <- do.call(rbind, newdata)
        odata[current_var][is.na(odata[current_var])] <- a3[,current_var]

        fileloc <- print_and_save_graph("density",q3.4,paste(Method,"_",current_var,".jpg",sep=''))
        Email_file_to_Slack(paste("The jibba jabba stops here!  I've replaced NA's in variable '",current_var,"''  using K means Clustering.  ",sep=""),fileloc)

      } else {  #if(Method=="avg")
        MeanValue <- lapply(odata[current_var][!is.na(odata[current_var]),],mean)
        odata[current_var][is.na(odata[current_var])] <- MeanValue
#        Email_file_to_Slack("Replaced columsn with the average values for each column","")
        Message_to_slack <- paste("Replaced NAs in column '",current_var,"' with the value ",MeanValue,sep='')
        #slackr(str(paste("Replaced NAs in column '",current_var,"' with the value ",MeanValue,sep='')))
        print(Message_to_slack)
      }
    }
  } else {
    #no missing columns
#      Email_file_to_Slack("No missing data, so we don't need KNN.","")
      print("No missing data, so we don't need KNN.")
 
  }

  return(odata)
}
