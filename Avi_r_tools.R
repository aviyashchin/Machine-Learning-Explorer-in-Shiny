
setwd("/Users/avi/boxer/data/week1data")
load(".RData")

#nice Plots
par(mfrow=c(1,1)) #1x1 plot
par(mfrow=c(2,2)) #2x2 plot


# load("Shiny.RData")
browseVignettes(package="dplyr")
save.image()
save.image("Shiny.RData")
# save specific objects to a file
# if you don't specify the path, the cwd is assumed 
save(object list,file="myfile.RData")

#loading data
	mydata = read.csv("NYC_Jobs.csv")

#getting stock data


library(Quandl)
library(ggplot2)
data_series <- Quandl("GOOG/NASDAQ_AAPL", start_date="2005-01-01")[,c(1,5)] 

my.plot <- ggplot(data=data_series, aes(x=Date, y=Close)) +
geom_line(color="#FAB521") + # Adding a colored line
theme(panel.background = element_rect(fill="#393939"), panel.grid.major.x = element_blank(),
panel.grid.major.y = element_line(colour="white", size=0.1),
panel.grid.minor = element_line(colour="white", size=0.1)) +
xlab("Date") + ylab("Closing Price") + ggtitle("AAPL")
my.plot	
#renaming a column
rename(d, c("beta"="two", "gamma"="three"))

#New columns
MyData=mutate(MyData,Salary.Annualized.From=ifelse(Salary.Frequency == "Daily" , Salary.Range.From*252,
		ifelse(Salary.Frequency == "Hourly", Salary.Range.From*2000, Salary.Range.From)))

#seclet columns
t3<- dplyr::select(titanic3, pclass, survived, sex, age, sibsp, parch)

#Filtering
FromSalaryAverage = MyData %>% group_by(Agency) %>% summarise(FromSalaryAverage=mean(Salary.Annualized.From))

#add a column

#nice little graph
qplot(reorder(class,hwy,median), hwy, data = mpg,geom = c("boxplot"))

#shiny work
setwd("/Users/avi/boxer/Shiny/")
load("Shiny.RData")
runApp()


#BASIC STATS FUNCTIONS TO Run
# ➢ Check for mepty Variabless
t<-titanic3
describeBy(t,group="survived")
hist(t$survived[t[survivied==0]])

t$survived[,t$survivied==0]

#comapring a dataset by anouther factor by basic visualization
plot(density(temp$Heart.Rate[temp$Gender == "Male"]), col = "blue")
lines(density(temp$Heart.Rate[temp$Gender == "Female"]), col = "pink")

#nice boxplots
boxplot(weight ~ group, data = PlantGrowth)

#Conducting the Bartlett test of homogeneity of variances.
bartlett.test(PlantGrowth$weight ~ PlantGrowth$group)

#3a One-Way ANOVA
summary(aov(weight ~ group, data = PlantGrowth))

#Mosaic plot
mosaicplot(HairEyeColor, shade = TRUE)



#Data cleaning tools!
	# ➢ What are the central tendencies?


	# ➢ What is the spread of the values?



	# ➢ How much do the values vary?




	# ➢ Are there any abnormalities that stand out?

#Three types of Missingness
md.pattern(t)

#remove blanks
df[df == ""] <- NA
#mean impute for a random column
t$age = ifelse(is.na(t$age), mean(t$age, na.rm=TRUE), t$age)


#Missing Completely at Random (MCAR)

#Missing at Random (MAR)
#Missing Not at Random (MNAR)


#KNN
#k = square root of n

#train a NN on how to do KNN properly. 


#CHECK ASSUMPTIONS

#➢ Linearity
# Not linear? Does Tranform help?

#➢ Constant Variance
# Not linear? Does Tranform help?

#➢ Normality
#➢ Normality

#➢ Independent Error




library(ggthemes)
plot(model)
#basic GGplot
ggplot(mpg, aes(hwy, cty)) +
  geom_point(aes(color = cyl)) +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  scale_color_gradient() +
  theme_hc()

ggpsadflot(dat,asdfadsfjh  aes(x=xvar, y=yvar)) +
  geom_point(shape=1)      # Use hollow circles

ggplot(dat, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hllow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)



#boxcox transofrm
#find shitty max
SBmodel.full.bc$x[SBmodel.full.bc$y ==max(SBmodel.full.bc$y)]


#------------------
#MULTIVARIATE assumptions

#------- CHECK FOR THESE FOR EACH OF THE INPUT VARIABLES  ----------
#➢ Linearity

#➢ Constant Variance

#➢ Normality	

#➢ Independent Errors



> Are the predictor variables useful to help predict Y?

> How well does our model fit? 

> How accurate are our guesses for y? 




#Check for AIC
#Check for BIC


Forward selection
backward selection
both selection
