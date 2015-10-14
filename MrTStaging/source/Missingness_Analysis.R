Missingness_Analysis <- function(FIPS){
  #Missing Completely at Random (MCAR) - Very rare, can drop observations. 
  #Missing at Random (MAR) - if missing at random, can drop observations. 
  #Missing Not at Random (MNAR) NON-IGNORABLE!

  #data <- titanic3
  #data <- airquality
  #data <- cars
  data <- FIPS
  Message_if_you_have_clean_data=""

  #NA and Blanks Check
  tmp=data[is.na(data)==FALSE] #only the non_na cells
  numBlanks = count(tmp[tmp == ""])[1,2]  #checks for "" in cells
  if(!is.na(numBlanks)){
    data[data == ""] <- NA
    fileloc <- print_and_save_graph("aggr",data,"Before_Missingness_Adjustment.jpg") 
    Email_file_to_Slack(paste("Teachin' fools some basic rules - ",numBlanks," blanks were replaced with NA. I'm assuming your files are Missing not at random. Here's what your awful data looks like before I work my magic. You don't rehearse Mr. T, you just turn him loose.",sep=""),fileloc)
  } else {
   Message_if_you_have_clean_data=paste(Message_if_you_have_clean_data," There were no NA's or Blanks in this file. ") 
  }

  #Checking for more than 5% missing values in cols
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
  badcols = apply(data,2,pMiss)

  if(sum(badcols)>0){
    #columns to remove, if more than MISSING_COLS_FOR_REMOVAL % of columsn are NA
    removecols=names(badcols[badcols>=MISSING_COLS_FOR_REMOVAL])
    data <- data[,-which(names(data) %in% removecols)]

    fileloc <- print_and_save_graph("aggr",data,"After_Column_Removal.jpg")
    Email_file_to_Slack(paste("BAD NEWS SUCKA.  More than ",MISSING_COLS_FOR_REMOVAL,"% of variables data missing for column(s) '",do.call(paste, c(as.list(removecols),sep=", ")),"', so I deleted them. ",sep=""),fileloc)
  } else {
    Message_if_you_have_clean_data=paste(Message_if_you_have_clean_data," No columns have more than ",MISSING_COLS_FOR_REMOVAL,"% of their data missing. ")
  }

  #Checking for more than 5% missing values in rows
  badrows = apply(data,1,pMiss)
  if(sum(badrows[which(badrows>=MISSING_ROWS_FOR_REMOVAL)])){

    #only keep the good rows
    data <- data[which(badrows<=MISSING_ROWS_FOR_REMOVAL),]

    fileloc <- print_and_save_graph("aggr",data,"After_Row_Removal.jpg")
    Email_file_to_Slack(paste(" WHOHA.  More than ",MISSING_ROWS_FOR_REMOVAL,"% of variables were missing for row(s) ",do.call(paste, c(as.list(which(badrows>MISSING_ROWS_FOR_REMOVAL)), sep=", ")),", so I deleted them! CLEAN YO DATA. ",sep=""),fileloc)
  } else {
        Message_if_you_have_clean_data=paste(Message_if_you_have_clean_data," You're not missing more than ",MISSING_ROWS_FOR_REMOVAL,"% of data points in any your rows.")
  }

  if(Message_if_you_have_clean_data!=""){
    Email_file_to_Slack(Message_if_you_have_clean_data, paste(PATH,"/MrTImages/reuse these tools.jpg",sep=""))
  }

  # for (i in 1:39487) {
  #   #ERROR HANDLING
  #   possibleError <- tryCatch(
  #       thing(),
  #       error=function(e) e
  #   )

  #   if(!inherits(possibleError, "error")){
  #     #REAL WORK
  #     useful(i); fun(i); good(i);
  #   }

  # }  #end for
  return(data)
}
