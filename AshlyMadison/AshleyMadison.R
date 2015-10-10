#database connection

#Database tables
# '120 - no exercise adults (percent) - national'
# '121 - few fruitsvegetables adults (percent) - national'
# 'aminno_member'
# 'bankruptcy'
# 'county business patterns from factfinder.census.gov1'
# 'education1'
# 'household income disparity1'
# 'member_details'
# 'populationestimates'
# 'povertyestimates'
# 'social_capital'
# 'unemployment'
# 'ziptofips'

#first WEIRD thing about using google's version of MySql is that you can't do "select top * from" - as "Top *" isn't standard Mysql.  Status: "Not a bug"
#https://bugs.mysql.com/bug.php?id=29855
LoadDepends <- function(){
#load dependency libraries
	library(dplyr)
	library(reshape2)
	library(ggplot2)
	library(lattice)
	library(DBI)
	library(RMySQL)
	library(plyr)

	setwd("/Users/avi/boxer/AshlyMadison/")
	save.image("am.RData")
	load("am.RData")
}

loadMysqlData <- function(){
#load first set of MYSQL queries
	con <- dbConnect(MySQL(),user="root", password="uLFZ2WoB",dbname="test", host="130.211.154.93")

	rs <- dbSendQuery(con, "select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
	No_Exercise <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select `Locale County FIPS Code`,`Numeric Value` as `few_fruits_percent` from `121 - few fruitsvegetables adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
	Fruits_Vegetables <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL;")
	No_Exercise <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select `Circ/Dist and County`,`County Code`,`All FilingsTotal`,`Business Filings Total`,`Nonbusiness Filings Total` from `bankruptcy`;")
	Bankruptcy <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select `FIPS`,`Number of establishments`, `Paid employees for pay period including March 12 (number)`, `First-quarter payroll ($1,000)` as `First_quarter_payroll`, `Annual payroll ($1,000)` as `Annual_payroll` from `county business patterns from factfinder.census.gov1` where year=2013 and `Meaning of 2012 NAICS code`='Total for all sectors';")
	NumCompanies <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select `FIPS Code` as FIPS,`State` as `State`,`Area name` as `Area_name`,`2013 Rural-urban Continuum Code` as `2013_Rural-urban_Continuum_Code`,`2013 Urban Influence Code` as `2013_Urban_Influence_Code`,`Prc of adults with less than high school diploma, 2009-2013` as `Prc_of_adults_with_less_than_high_school_diploma_2009-2013`,`Prc of adults with a high school diploma only, 2009-2013` as `Prc_of_adults_with_a_high_school_diploma_only_2009-2013`,`Prc of adults compl some college or asc deg, 2009-2013` as `Prc_of_adults_compl_some_college_or_asc_deg_2009-2013`,`Prc of adults with a bachelor's deg or higher, 2009-2013` as `Prc_of_adults_with_a_bachelor's_deg_or_higher_2009-2013` from `education1`;")
	Education <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select * from `household income disparity1`;")
	Disparity <- fetch(rs,n=-1)
	dbClearResult(rs)
	dbDisconnect(con)
	on.exit(dbDisconnect(con))
}

loadMysqlData2 <- function(){
#load second set of SQL Queries (need to do this smarter)
	con <- dbConnect(MySQL(),user="root", password="uLFZ2WoB",dbname="test", host="130.211.154.93")

	rs <- dbSendQuery(con, "select FIPStxt as FIPS, State, Area_Name, `Rural-urban_Continuum Code_2013` as `Rural_urban_Continuum_Code_2013`, Urban_Influence_Code_2013, CENSUS_2010_POP, ESTIMATES_BASE_2010, POP_ESTIMATE_2014, N_POP_CHG_2014, Births_2014, Deaths_2014, NATURAL_INC_2014, INTERNATIONAL_MIG_2014, DOMESTIC_MIG_2014, NET_MIG_2014, RESIDUAL_2014, GQ_ESTIMATES_2014, R_birth_2014, R_death_2014, R_NATURAL_INC_2014, R_INTERNATIONAL_MIG_2014, R_DOMESTIC_MIG_2014, R_NET_MIG_2014 from `populationestimates`;")
	PopulationEstimates <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select * from `povertyestimates`;")
	PovertyEstimates <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select * from `social_capital`;")
	SocialCapital <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select FIPS_Code, State, Area_name, Rural_urban_continuum_code_2013, Urban_influence_code_2013, Civilian_labor_force_2014, Employed_2014, Unemployed_2014, Unemployment_rate_2014, Median_Household_Income_2013 from `unemployment`;")
	Unemployment <- fetch(rs,n=-1)

	rs <- dbSendQuery(con, "select * from `ziptofips`;")
	ZiptoFips <- fetch(rs,n=-1)

	# rs <- dbSendQuery(con, "select zip,count(nickname),avg(dob) from `aminno_member` where zip in (select ZCTA5 from ziptofips) group by zip;")
	# Scumbags <- fetch(rs,n=-1)

	# rs <- dbSendQuery(con, "select * from `aminno_member` where gender=2 and approved=1 and zip is not NULL;")
	# AllScumbags <- fetch(rs,n=-1)

	dbClearResult(rs)
	dbDisconnect(con)
	on.exit(dbDisconnect(con))
}

MungMe <- function(){
#Crap I forgot to do in SQL
	#rename columns
	Bankruptcy <- rename(Bankruptcy, c("County Code"="FIPS"))
	SocialCapital <- rename(SocialCapital, c("FIPStxt"="FIPS"))
	Unemployment <- rename(Unemployment, c("FIPS_Code"="FIPS"))
	Fruits_Vegetables <- rename(Fruits_Vegetables, c("Locale County FIPS Code"="FIPS"))
	Scumbags <- rename(Scumbags, c("zip"="FIPS"))
	AllScumbags <- rename(AllScumbags, c("zip"="FIPS"))

	#cast things properly - NUMERIC VS INTEGERS?   REALLY R PEOPLE? 
	Fruits_Vegetables[,1]=as.integer(Fruits_Vegetables[,1])
	Scumbags[,1]=as.integer(Scumbags[,1])
	AllScumbags[,13]=as.integer(AllScumbags[,13])

	#remove NA's
	Scumbags=Scumbags[complete.cases(Scumbags[,1]),]
	AllScumbags=AllScumbags[complete.cases(AllScumbags[,13]),]
	Fruits_Vegetables=Fruits_Vegetables[complete.cases(Fruits_Vegetables[,1]),]

	allscumbags <- aggregate(AllScumbags$pnum, by=list(Category=AllScumbags$FIPS), FUN=length)
	allscumbags <- rename(allscumbags, c("x"="SBs","Category"="FIPS"))

  rm(AllScumbags)
}

BuildFinalTable <- function(){
#Goes through all the data and does a join on Zip code (FIPS code)
# ZiptoFips,
	allData=list(Bankruptcy, Disparity, Education, Fruits_Vegetables, No_Exercise, NumCompanies, PopulationEstimates,SocialCapital,Unemployment, allscumbags)

	# for (i in 1:length(allData)){
	#    print(paste("table ",i," ",nrow(allData[[i]])))
	#    print("hello")
	# }
i=1
	FIPS=Bankruptcy
	for (i in 2:length(allData)){
	   print(paste("table ",i," ",nrow(allData[[i]])))
		FIPS=left_join(FIPS,allData[[i]],by="FIPS")
		print(paste("rows:",nrow(FIPS)))
	}
	
#go through and add numeric items into correlation table
	j=2  #start at 2 because NewGuys first row is already initialized
	newGuy<-FIPS[2]
   #find non-numeric names
   	for (i in 1:length(colnames(FIPS))){
	   print(paste("coltype ",i," ",colnames(FIPS)[i]," ",class(FIPS[,i])))
  	   if (class(FIPS[,i])=="integer"){ 
  	      newGuy[j]<-FIPS[i]
  	           j<-j+1
        }else{
     	    0
     	  }
	   }

	#Which columns to use
	MakeMePerCapita <- c("All FilingsTotal",
	                     "Business Filings Total",
	                     "Nonbusiness Filings Total", 
	                     "Number of establishments",
	                     #"Paid employees for pay period including March 12 (number)", 
	                     "Annual_payroll", 
	                     "First_quarter_payroll",
	                     "POP_ESTIMATE_2014", 
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
	                     "POV05_2013",
	                     "Civilian_labor_force_2014",
	                     "Employed_2014",
	                     "Unemployed_2014",
	                     "SBs")
	
	for (i in 1:length(MakeMePerCapita)){
	  print(paste("MakeMePerCapita ",i," ",colnames(newGuy[eval(MakeMePerCapita[i])])))
	  newGuy[,eval(MakeMePerCapita[i])]=newGuy[,eval(MakeMePerCapita[i])]/newGuy["POP_ESTIMATE_2014"]
	  }
	
	
	SBMatrix<-cor(newGuy, method = "spearman", use = "pairwise")
  sort(SBMatrix[48,])


   #Creating population density variables.
	states[,9] = (states$Population*1000)/states$Area
	colnames(states)[9] = "Density"

	#build a correlation tabler

}

BuildPrettyPictures <- function(){
	ggplot(iris, aes(y = Sepal.Length, x = Sepal.Width, color=Species),  auto.key=TRUE) +
	  stat_density2d(aes(fill=Species, alpha=..level..), geom = "polygon", size=0) + 
	  xlab("Sepal Width") +  ylab("Sepal Length") +
	  geom_rug(position = "jitter") +
	  ggtitle("Sepal Length-Width")
}

# Bankruptcy, Disparity, Education, Fruits_Vegetables, No_Exercise, NumCompanies, PopulationEstimates,SocialCapital,Unemployment, ZiptoFips


#MySQL Community Server (GPL) Stinks

# First off, this is a topic near and dear to my heart.  When I was 21 and first moved to the city I found out my GF was dating someone in Baltimore for 6 months.  I had a "tom-cruise" style jump on the couch moment. 

#secondly, I'm using ZCTA as a proxy for Zip code.  of the 41k zip codes, 35k of them are identical to the ZCTA so, close enough. 

# KXCD comics

#my spotify playlist https://open.spotify.com/user/shanonlev/playlist/3mAMTVobhui7WNAnGowQQrFruits_Vegetables[,1]=as.numeric(Fruits_Vegetables[,1])