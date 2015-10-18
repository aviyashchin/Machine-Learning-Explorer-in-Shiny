loadAllAMMysqlData <- function(){
  ################################################################################################################################
  #
  # Make each SQL Call
  # Useage:   FIPS=loadAllAMMysqlData()
  #
  ################################################################################################################################

  con <- dbConnect(MySQL(max.con = 16), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")

  #load first set of MYSQL queries
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

  ################################################################################################################################
  #
  # DATA MUNGING PART II - THE MASS LEFT JOIN ON ZIP CODE
  #
  ################################################################################################################################

  #Goes through all the data and does a join on Zip code (FIPS code)
  # ZiptoFips
  allData=list(allscumbags,Bankruptcy, Fruits_Vegetables, SocialCapital, Unemployment, Disparity, Education, No_Exercise, NumCompanies, PovertyEstimates, PopulationEstimates)

    i=1
    FIPS=allscumbags
    print(paste("# of FIPS Codes:",nrow(FIPS)))
  #   FIPS=left_join(FIPS,allData[[i]],by="FIPS")
    for (i in 2:length(allData)){
       print(paste("Left Joining table ",i," ",nrow(allData[[i]])," Rows"))
       FIPS=left_join(FIPS,allData[[i]],by="FIPS")
    }

  #make scumbags per capita column
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
          print(paste("MakeMePerCapita ",i," ",colnames(FIPS[eval(MakeMePerCapita[i])])))
          ((FIPS[,eval(MakeMePerCapita[i])]=FIPS[,eval(MakeMePerCapita[i])]/FIPS[,"POP_ESTIMATE_2014"]))
        #rename columns that have been adjusted to Per Capita Columns
          names(FIPS)[names(FIPS)==eval(MakeMePerCapita[i])] <- paste(eval(MakeMePerCapita[i]),"PerCapita",sep="")
        }

      FIPS$State.y <- NULL
      FIPS$State.x <- NULL
      #Remove NAs- REDUCES ZIP CODES FROM 3,100 TO 1,100.  YIKES
      FIPS = filter(FIPS, !is.na(FIPS$SBsPerCapita))    

 dbDisconnect(con)

 return(FIPS)

  on.exit(dbDisconnect(con))
}