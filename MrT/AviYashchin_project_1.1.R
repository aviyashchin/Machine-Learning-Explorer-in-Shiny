outlier# /** 
#  * [I PITY THE FOOL Project]
#  * @type {[R Script]}
#  *
#  * To run me:
#  * source("/Users/avi/Dropbox/programming/boxer/AshlyMadison/AviYashchin_Project_1.1.R")
#  *
#  */

#Load Dependency Libraries
  library(dplyr)
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
  library("ggthemes")
  source("/Users/avi/Dropbox/programming/boxer/Avi_r_tools.R")

load_Constants <- function(){
  PATH <<- "/Users/avi/boxer/MrT"
  IMGPATH <<- "/Users/avi/boxer/MrT/plots"

  #If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out. We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function
  MISSING_COLS_FOR_REMOVAL <<- 5
  MISSING_ROWS_FOR_REMOVAL <<- 5
  OUTLIER_CUTOFF_P_VALUE <<- 5
  NUM_CLUSTERS <<- 4
  TYPE <<- "violin"  #Options: c("kd","hist","Violin Plot")

}

load_DataSets <- function(){
  titanic <<- titanic3
  airquality <<- airquality
}

getConnectionGoogleMySql <- function() {
  ################################################################################################################################
  #
  # Get Connections to Google Account
  #
  ################################################################################################################################
  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(max.con = 1), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(max.con = 1), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")
  }
  return(.connection)
}

runSQL <- function(con,Query){
  ################################################################################################################################
  #
  # Call SQL Query.
  #
  # Useage:  con <- dbConnect(MySQL(max.con = 16), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")
  # No_Exercise <- runSQL(con,"select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
  ################################################################################################################################

  print(paste("Calling SQL Query:",Query))
  rs <- dbSendQuery(con, Query)
  tempsql <- fetch(rs,n=-1)
  dbClearResult(rs)
  return(tempsql);
}

loadAllAMMysqlData <- function(){
  ################################################################################################################################
  #
  # Make each SQL Call
  # Useage:   FIPS=loadAllAMMysqlData()
  #
  ################################################################################################################################

  con <- dbConnect(MySQL(max.con = 16), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")

  #load first set of MYSQL queries
  # rs <- dbSendQuery(con, "select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
  # No_Exercise <- fetch(rs,n=-1)
  No_Exercise <- runSQL(con,"select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
  
  Fruits_Vegetables <- runSQL(con,"select `Locale County FIPS Code` as FIPS,`Numeric Value` as `few_fruits_percent` from `121 - few fruitsvegetables adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")

  No_Exercise <- runSQL(con,"select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")

  Bankruptcy <- runSQL(con,"select `Circ/Dist and County`,`County Code` as `FIPS`,sum(`All FilingsTotal`) as `All FilingsTotal`,sum(`Business Filings Total`) as `Business Filings Total`,sum(`Nonbusiness Filings Total`) as `Nonbusiness Filings Total` from `bankruptcy` group by `County Code`;")

  NumCompanies <- runSQL(con,"select `FIPS`,`Number of establishments`, `Paid employees for pay period including March 12 (number)` as `num_paid_employees`, `First-quarter payroll ($1,000)` as `first_q_payroll`, `Annual payroll ($1,000)` as `annual_payroll` from `county business patterns from factfinder.census.gov1` where year=2013 and `Meaning of 2012 NAICS code`='Total for all sectors';")

  Education <- runSQL(con,"select `FIPS Code` as FIPS,`State` as `State`,`Area name` as `Area_name`,`2013 Rural-urban Continuum Code` as `2013_Rural-urban_Continuum_Code`,`2013 Urban Influence Code` as `2013_Urban_Influence_Code`,`Prc of adults with less than high school diploma, 2009-2013` as `Prc_of_adults_with_less_than_high_school_diploma_2009-2013`,`Prc of adults with a high school diploma only, 2009-2013` as `Prc_of_adults_with_a_high_school_diploma_only_2009-2013`,`Prc of adults compl some college or asc deg, 2009-2013` as `Prc_of_adults_compl_some_college_or_asc_deg_2009-2013`,`Prc of adults with a bachelor's deg or higher, 2009-2013` as `Prc_of_adults_with_a_bachelor's_deg_or_higher_2009-2013` from `education1`;")

  Disparity <- runSQL(con,"select * from `household income disparity1`;")

  PopulationEstimates <- runSQL(con,"select FIPStxt as FIPS, State, Area_Name, `Rural-urban_Continuum Code_2013` as `Rural_urban_Continuum_Code_2013`, Urban_Influence_Code_2013, CENSUS_2010_POP, ESTIMATES_BASE_2010, POP_ESTIMATE_2014, N_POP_CHG_2014, Births_2014, Deaths_2014, NATURAL_INC_2014, INTERNATIONAL_MIG_2014, DOMESTIC_MIG_2014, NET_MIG_2014, RESIDUAL_2014, GQ_ESTIMATES_2014, R_birth_2014, R_death_2014, R_NATURAL_INC_2014, R_INTERNATIONAL_MIG_2014, R_DOMESTIC_MIG_2014, R_NET_MIG_2014 from `populationestimates`;")

  PovertyEstimates <- runSQL(con,"select * from `povertyestimates`;")

  SocialCapital <- runSQL(con,"select * from `social_capital`;")

  Unemployment <- runSQL(con,"select FIPS_Code as FIPS, State, Area_name, Rural_urban_continuum_code_2013, Urban_influence_code_2013, Civilian_labor_force_2014, Employed_2014, Unemployed_2014, Unemployment_rate_2014, Median_Household_Income_2013 from `unemployment`;")

  ZiptoFips <- runSQL(con,"Select ZCTA5,FIPS from ziptofips;")

  AllScumbags <- runSQL(con,"select zip as ZCTA5, count(pnum) as SBs from `aminno_member` where gender=2 and approved=1 and country=1 and zip is not NULL group by zip;")

  ################################################################################################################################
  #
  # DATA MUNGING PART I - NAME COLUMNS PROPERLY, GET READY FOR THE MASS JOIN
  #
  ################################################################################################################################
  names(SocialCapital)[names(SocialCapital)=="fips"] <- "FIPS"
  names(PovertyEstimates)[names(PovertyEstimates)=="FIPStxt"] <- "FIPS"
  
  #cast things properly 
  Fruits_Vegetables[,1]=as.integer(Fruits_Vegetables[,1])
  NumCompanies[,"first_q_payroll"]=as.integer(NumCompanies[,"first_q_payroll"])
  NumCompanies[,"annual_payroll"]=as.integer(NumCompanies[,"annual_payroll"])
  NumCompanies[,"num_paid_employees"]=as.integer(NumCompanies[,"num_paid_employees"])
  PovertyEstimates[,"FIPS"]=as.integer(as.data.frame(PovertyEstimates)[,"FIPS"])
  Fruits_Vegetables[,"FIPS"]=as.integer(as.data.frame(Fruits_Vegetables)[,"FIPS"])
  SocialCapital[,"FIPS"]=as.integer(as.data.frame(SocialCapital)[,"FIPS"])

  AllScumbags$ZCTA5 <- iconv(AllScumbags$ZCTA5,"WINDOWS-1252","UTF-8")
  AllScumbags[,"ZCTA5"]=as.integer(as.data.frame(AllScumbags)[,"ZCTA5"])
  ZiptoFips[,"FIPS"]=as.integer(as.data.frame(ZiptoFips)[,"FIPS"])
  ZiptoFips[,"ZCTA5"]=as.integer(as.data.frame(ZiptoFips)[,"ZCTA5"])

  #View(AllScumbags)  
  #fix ZiptoFips Index
  Zeyu <- left_join(ZiptoFips,AllScumbags,by="ZCTA5")
  Zeyu[Zeyu == ""] <- NA
  Zeyu <- Zeyu[!is.na(Zeyu[, "SBs"]),]
  allscumbags <- dplyr::group_by(Zeyu, FIPS) %>% dplyr::summarize(SBs=sum(SBs))


    #   #TO DO - check out what's wrong with these variables:
    # aggr_plot <- aggr(t, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(t), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
          #      annual_payroll 1.000000000
          #          POV05_2013 1.000000000
          #       CI90LB05_2013 1.000000000
          #       CI90UB05_2013 1.000000000
          #       PCTPOV05_2013 1.000000000
          #      CI90LB05P_2013 1.000000000
          #      CI90UB05P_2013 1.000000000
          # no_exercise_percent 0.734161095
          #  few_fruits_percent 0.686723973}

  ################################################################################################################################
  #
  # DATA MUNGING PART II - THE MASS LEFT JOIN ON ZIP CODE
  #
  ################################################################################################################################

  #Goes through all the data and does a join on Zip code (FIPS code)
  # ZiptoFips
  allData=list(Bankruptcy, Fruits_Vegetables, SocialCapital, Unemployment, Disparity, Education, No_Exercise, NumCompanies, PovertyEstimates, PopulationEstimates,allscumbags)

    i=1
    FIPS=Bankruptcy
    print(paste("# of FIPS Codes:",nrow(FIPS)))
  #   FIPS=left_join(FIPS,allData[[i]],by="FIPS")
    for (i in 2:length(allData)){
       print(paste("Left Joining table ",i," ",nrow(allData[[i]])," Rows"))
       FIPS=left_join(FIPS,allData[[i]],by="FIPS")
    }

  dbDisconnect(con)

  return(FIPS)

  on.exit(dbDisconnect(con))
}

Email_file_to_Slack <- function(Body,File_Location){
  if (File_Location==""){
      send.mail(from = "mrtdatascientist@gmail.com",
      to = c("mrt.9msiu@zapiermail.com"),
      subject = "Hello sucka",
      body = Body,
      html = FALSE,
      smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "mrtdatascientist@gmail.com", passwd = "ipitythefool", ssl = TRUE),
      authenticate = TRUE,
      send = TRUE)
  }else{
      send.mail(from = "mrtdatascientist@gmail.com",
      to = c("mrt.9msiu@zapiermail.com"),
      subject = "Hello sucka",
      body = Body,
      html = FALSE,
      smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "mrtdatascientist@gmail.com", passwd = "ipitythefool", ssl = TRUE),
      attach.files = c(File_Location),
      authenticate = TRUE,
      send = TRUE)
    }
}

print_and_save_graph <- function(execute_function,new_data,file_name){
  data2=new_data
  print("test")

  if (execute_function == "aggr"){
    myPATH <- file.path(IMGPATH,file_name)
    print(myPATH)
    jpeg(file=myPATH)
    print("AGGR!")
    eval(call(execute_function,data2,col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data2)))
    dev.off()

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data2)+ 
        aes_string(x=colnames(data2)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())
  }


  return(paste(IMGPATH,"/",file_name,sep=""))
}

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

K_means_Clustering <- function(FIPS){

  ################################################################################  
  # Impute leftovers with K Means clustering
  #
  ################################################################################  

  #odata <- titanic3
  #odata <- airquality
  #odata <- data
  odata <- FIPS
  #odata <- FIPS.Missing     

  #only build correlation on complete columns 
  missingCols = apply(odata,2,pMiss)
  compColNames = names(odata[missingCols==0])  #no missing data
  missColNames = names(odata[missingCols>0])  # any missing data

  if(length(missColNames)!=0){
    #colnames(odata)[sapply(odata, class)=="character"]
    for (i in 1:length(missColNames)){
      gdata = odata[,c(compColNames)]
      bdata = odata[,c(missColNames)]

      #move the current columns from "Bad columns" to "good columns"
      current_var = missColNames[i]
      gdata[,current_var] <- bdata[,current_var]
      bdata[,current_var] <- NULL

      #only train and test using complete columns
      train = gdata[!is.na(gdata[,current_var]),]
      test = gdata[is.na(gdata[,current_var]),]

      regression_formula = as.formula(paste(current_var," ~ .",sep=""))
      kknn1  = kknn(regression_formula, train, test, k = 1, distance = 1)
      kknn2  = kknn(regression_formula, train, test, k = 1, distance = 2)
      kknn5  = kknn(regression_formula, train, test, k = 1, distance = 5)

      a1 = data.frame(test = kknn1$fitted.values, TYPE = "kmeans k=1 dist=1")
      a2 = data.frame(test = kknn2$fitted.values, TYPE = "kmeans k=1 dist=2")
      a3 = data.frame(test = kknn5$fitted.values, TYPE = "kmeans k=1 dist=5")
      a4 = data.frame(test = c(train[current_var]),TYPE="original")

      colnames(a1)[1]=current_var
      colnames(a2)[1]=current_var
      colnames(a3)[1]=current_var
      colnames(a4)[1]=current_var

      newdata = list(a1,a2,a3,a4)
      q3.4 = do.call(rbind, newdata)
      # ggplot(q3.4)+ 
      #  aes_string(x=current_var,group="type",color="type") + 
      #  geom_density(fill=NA)

      fileloc <- print_and_save_graph("density",q3.4,paste("k_means_",current_var,".jpg",sep=''))
      Email_file_to_Slack(paste("The jibba jabba stops here!  I've replaced NA's in variable '",current_var,"''  using K means Clustering.  ",sep=""),fileloc)

      # m <- ggplot(q3.4, aes(x=age, group=type,color=type))
      # m + geom_density(fill=NA)
      odata[current_var][is.na(odata[current_var])]=a3[,current_var]
    }
  } else {
    #no missing columns
      Email_file_to_Slack("No missing data, so we don't need KNN.","")
  }
  # Drink your milk.

  #Delete weird columns because I fell asleep at 2:00am at my desk
  #drops=c("CI90UBINC_2013","CI90LBINC_2013","R_NATURAL_INC_2014","R_NET_MIG_2014","R_DOMESTIC_MIG_2014","R_INTERNATIONAL_MIG_2014","CI90LBAll_2013","CI90UBALL_2013 ","CI90LB017_2013 ","CI90LB517_2013","CI90UB017_2013","CI90UB517_2013","CI90UB017P_2013","CI90UB517P_2013","CI90LB017P_2013","CI90UBALLP_2013","first_q_payrollPerCapita","num_paid_employees","Number of establishmentsPerCapita","annual_payrollPerCapita","POV05_2013PerCapita","CI90LB05_2013","CI90UB05_2013","PCTPOV05_2013","CI90LB05P_2013","CI90UB05P_2013")
  #drops=c("annual_payroll","POV05_2013","CI90LB05_2013","CI90UB05_2013","PCTPOV05_2013","CI90LB05P_2013","CI90UB05P_2013")
  #df=df[,!(names(df) %in% df)]

  #complete.cases(FIPS)
  return(odata)
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

#Rewrite me. 
Prettyqplot2<-function(xss,yss){

  print(colnames(Sig))

  x <- Sig$SBsPerCapita*100000

  #positive corrs
  titlevar="Median_Household_Income_2013"
  titlevar="N_POP_CHG_2014PerCapita"
  titlevar="Employed_2014PerCapita"
  titlevar="Civilian_labor_force_2014PerCapita"
  titlevar="Prc_of_adults_with_a_bachelor's_deg_or_higher"

  #negative corrs
  titlevar="Deaths_2014PerCapita"
  titlevar="gini2000"
  titlevar="few_fruits_percent"
  titlevar="Unemployment_rate_2014"

  #Significant as per LM Model
  titlevar="CI90LBALLP_2013"
  titlevar="PCTPOVALL_2013"
  titlevar="no_exercise_percent"

  y <-Sig[,titlevar]

  #for (i in 1:ncol(Sig)){
  qplot(x, y, geom = "smooth",na.rm=TRUE)
  qplot(x, y,na.rm=TRUE)
  cor(x,y, method = "spearman", use = "pairwise")
  aovr <- summary(aov(x ~ y,na.rm="TRUE")) #Conducting the One-Way ANOVA on the weight
  ggplot(final, aes(x, (y))) +
  geom_point(size = 5) +
  geom_point(aes(col = Sig[,"2013_Urban_Influence_Code"]), size = 4) +
  theme_hc() + 
  ylab(titlevar) + 
  xlab("Scumbags per 100,000")+
  ggtitle(paste("Scumbags vs ",titlevar,"by County"))+
  ggsave(file=paste(titlevar,"x~y.png",sep=""))
}

#Rewrite me as boxplot
makeplot <- function(){

  plot(lm(Sig,x ~ .)) #Conducting the One-Way ANOVA on the weight
  plot(lm(x ~ y)) #Conducting the One-Way ANOVA on the weight
  summary(aov(x ~ y)) #Conducting the One-Way ANOVA on the weight
  #loss by considering each category of diet.
  boxplot(x ~ y,
    col = c("red", "orange", "yellow", "green"),
    main = "Distribution of Weight Loss\nfor Various Diets")
}

#Delete me.
BuildPrettyPictures <- function(){
	ggplot(iris, aes(y = Sepal.Length, x = Sepal.Width, color=Species),  auto.key=TRUE) +
	  stat_density2d(aes(fill=Species, alpha=..level..), geom = "polygon", size=0) + 
	  xlab("Sepal Width") +  ylab("Sepal Length") +
	  geom_rug(position = "jitter") +
	  ggtitle("Sepal Length-Width")
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

Outliers <- function(data,cutoff_in){
#numeric only
  
#replace this with the function that CMakris showed us
#data <- cars

  plot.new()
  num_cols <- dim(data)[1]

  mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data), ,tol=1e-20)
  
  cutoff <- qchisq(1 - cutoff_in / 100, dim(data)[2], ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  outlier <- mahalanobis_dist > cutoff
  
  df_outliers <<- data.frame(x = c(1:dim(data)[1]), y = log(sqrt(mahalanobis_dist)), z = outlier)

  outlier_list <- df_outliers[df_outliers$z==TRUE,]

  #show_outliers$Names <<- row_names[df_outliers[,3]]
  #show_outliers$Distances <<- mahalanobis_dist[df_outliers[,3]]

  p <- ggplot(df_outliers,aes(x = x,y = y))
  
  p <- p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))  
  
  p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20))

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","PlotOutliers.jpg",sep="")
  ggsave(filename = fileloc, plot = p)

  Email_file_to_Slack(paste("Based on your cutoff, you have ",nrow(df_outliers[df_outliers$z==TRUE,])," outliers. ",sep=""),fileloc)

  print(p)
}
  
Scree_Plot <- function(data){

  plot.new()

  result <- prcomp(data, center = TRUE, scale = TRUE)
  retained_variance <- cumsum(unlist(result[1])^2) /  max(cumsum(unlist(result[1])^2))
  
  df <- data.frame(x = c(1:dim(data)[2]), y = retained_variance)
  
  p <- ggplot(df, aes(x = x,y = y)) + xlab('Retained Dimensions') + ylab('Explained Variance') + ggtitle('Scree Plot')
  p <- p + geom_point() + geom_line() + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=45)) 

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","PlotScreePlot.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Scree Plots Incoming! ",sep=""),fileloc)

  plot(p)
  print(p)
}
  
Correlation <- function(data){
  plot.new()

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
  
Mean_Vectors <- function(data, type){
  plot.new()
  output_mean = c() #need to replace this with reactive shiny
  output_se = c() #need to replace this with reactive shiny
  #ranges <- reactiveValues(y = NULL)
  ranges = c()
  ranges$y <- c(brush$ymin, brush$ymax)
  ranges$y <- c(0, 100)
  ranges$x = 1

  #data <- sleep
   num_vars <- dim(data)[2]
  
   for (i in c(1:num_vars)){

    name <- colnames(data)[i]
    output_mean[i] <- mean(data[,i],na.rm = TRUE) 
    output_se[i] <- sd(data[,i],na.rm = TRUE) / sqrt(length(data[,3][!is.na(data[,3])]))
   }

   index <- output_mean < 100
   names_to_use <- colnames(data)
   
   df <- data.frame(names = names_to_use[index], means = output_mean[index])
   
   keep_data <- data[,index]
   keep_data <- melt(keep_data)
   
   if (type == "Scatter"){
    p <- ggplot(df, aes(x = names, y = means))
     p <- p + geom_point() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else if(type == "Scatter with error bars"){
     limits <- aes(ymax = output_mean[index] + output_se[index], ymin=output_mean[index] - output_se[index])
     p <- ggplot(df, aes(x = names, y = means))
     p <- p + geom_point() + geom_errorbar(limits, width=0.3) + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else if(type == "Violin Plot"){
    p <- ggplot(keep_data,aes(x = variable, y = value)) + geom_violin() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   } else{
    p <- ggplot(keep_data,aes(x = variable, y = value)) + geom_boxplot() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Column Means') + coord_cartesian(ylim = ranges$y)
   }

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","MeanVectors.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Be Somebody or Be Somebody's Fool.  Know what your data looks like! ",sep=""),fileloc)

  print(p)
}

Clustering <- function(data,num){
  plot.new()

  clust <- hclust(dist(data), method = "complete")

  memb <- cutree(clust, k = num)
  
  fit <- prcomp(data, center=TRUE, scale = TRUE)
  
  df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = memb)
  
  p <- ggplot(df,aes(x = x,y = y, colour = factor(z)))
  
  p <- p + geom_point(size = 5) + xlab('First Principal Component') + ylab('Second Principle Component') + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x = element_text(vjust = 2)) + scale_colour_discrete(name = "Clusters")  

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","Clustering.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Life's tough, but I'm tougher!  Check out these clusters. ",sep=""),fileloc)

  print(p)
}

Linear_Regression <- function(data,num){
  plot.new()


  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","Linear_Regression.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Sylvester Stallone.  Check out these clusters. ",sep=""),fileloc)

  print(p)
}

MultiVariate_Regression <- function(data,num){
  plot.new()

  #Saves the file to the drive, and emails the file out & to slack. 
  fileloc <- paste(IMGPATH,"/","MultiVariate_Regression.jpg",sep="")
  ggsave(filename = fileloc, plot = p)
  Email_file_to_Slack(paste("Hannibal is on the jazz.  Check out these clusters. ",sep=""),fileloc)

  print(p)
}

# depth = quakes$depth
# KXCD comics

#my spotify playlist https://open.spotify.com/user/shanonlev/playlist/3mAMTVobhui7WNAnGowQQrFruits_Vegetables[,1]=as.numeric(Fruits_Vegetables[,1])

ShinyServer <- function(){
    show_outliers <<- reactiveValues(Names = NULL, Distances = NULL)
    show_outliers = c();
  }

main <- function(){
  load_Constants()
  load_DataSets()

  setwd(PATH)
  save.image("am.RData")
  #load("am.RData")

  #Define DataSet and Independent Variable
  AMData <- loadAllAMMysqlData()
  dependentVariable <- "SBsPerCapita"

  #idata <- titanic3  #missing data 
  #idata <- airquality   #missing data 
  #idata <- cars
  #idata <- iris
  #idata <- state.x77
  #idata <- sleep
  #idata <- chickwts

  dependentVariable <- colnames(idata)[1]
  row_names    <<- idata[,1]; 
  column_names <<- idata[1,]; # store the column names for future reference 

  #start analyzing the dataset
  #Email_file_to_Slack(paste("I pity the foo'... Who tries to analyze a dataset with ",nrow(idata)," rows and ",ncol(idata), " columns by hand. Sucka.",sep=""),"")

  #Check for missingness (blanks and NA's)
  idata.Missing <- Missingness_Analysis(idata)

  #run K_means on the remaining NA's in the data
  idata.NAs_replaced <- K_means_Clustering(idata.Missing)

  data <- idata.NAs_replaced
  #Show histograms of data
  PlotMarginals(idata.NAs_replaced,dependentVariable,"comb")

  Outliers(idata.NAs_replaced,OUTLIER_CUTOFF_P_VALUE)

  Scree_Plot(idata.NAs_replaced)

  Correlation(idata.NAs_replaced)

  Mean_Vectors(idata.NAs_replaced,"Violin Plot")

  Clustering(idata.NAs_replaced,NUM_CLUSTERS)

  Linear_Regression(idata.NAs_replaced)  #broken

  MultiVariate_Regression(idata.NAs_replaced)  #broken

  #➢ Linearity
  # Not linear? Does Tranform help?

  #➢ Constant Variance
  # Not linear? Does Tranform help?

  #➢ Normality
  #Checking for normality of Dependent Variable
  #Run Box-Cox Transformation on dependent Variable

  #➢ Independent Error
}

