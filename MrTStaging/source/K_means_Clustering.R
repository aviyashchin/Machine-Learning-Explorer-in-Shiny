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
