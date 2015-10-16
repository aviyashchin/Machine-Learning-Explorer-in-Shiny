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
#data = read.table("04NYCRestaurants.txt")

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
#    SUCKAtxt = paste(sep="",collapse="\n")
#    slackr(eval(SUCKAtxt))
#    SUCKAtxt = paste("I'm removing columns '",list(colnames(df)[dataTypes == 0]),"'. Find someone smarter than me to analyze columns with more than ",NUM_FACTORS_WE_CAN_HANDLE," factors. Call Owen Zhang @ datarobot.com, or try H2o.ai for model building, wit.ai for speech, or cortical.io for basic text comparison NLP, or torch.ch for RNN/CNN.  Here's what was taken out:",sep="")
#    slackr(eval(SUCKAtxt))
    slackr(str(removedColumns))
  }

  #Only keep Integers, Logical, and factors <NUM_FACTORS_WE_CAN_HANDLE columns
  dfreturn <- df[,-c(r_indi)]

  return(dfreturn)
}