install.packages("viridis")
install.packages("devtools")
devtools::install_github("ropensci/plotly")

Sys.setenv("plotly_username" = "aviyashchin")
Sys.setenv("plotly_api_key" = "iq2ftqjbsi")
library(plotly)


d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

install.packages("googleVis")
install.packages("reshape")
library(reshape)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lattice)
library(googleVis)

# 1. Read in 'Teams.csv' and 'Salaries.csv'.
setwd("/Users/avi/boxer/Moneyball_Lab")

Teams = read.csv("data/Teams.csv")
Salaries = read.csv("data/Salaries.csv")
salary = read.csv("data/Salaries.csv")

teams=Teams
salaries=Salaries
# teams = tbl_df(Teams)
# salaries = tbl_df(Salaries)

# 2. Show the total salaries for each team per year.
salaryGroup=group_by(salaries,teamID,yearID)
salaries_by_team_and_year = summarise(salaryGroup,sum(salary))

# 3. Merge the new salary dataframe you created with the teams
# dataframe. Hint*** Do both datasets range the same span of
# years?

#delete teamid from teams
teams$teamID
names(teams)[names(teams)=="teamID"] <- "teamID2"


m1 <- merge(salaries_by_team_and_year, teams, by.x = c("teamID","yearID"), by.y = c("franchID","yearID"))

names(m1)[names(m1)=="sum(salary)"] <- "sum_salary"

#make a "win percent" column to compare win rates over time
m2 <- mutate(m1,winrate=W/G)


# 4. Graph the total wins versus the total salaries for each year.
# Think about the best type of plot to use for this. Annotate the
# Oakland team on these graphs.

m2 <- mutate(m2,ashwin=sum_salary/W)

#dployr
qplot(yearID, ashwin, data = m2, geom = "smooth",color = teamID)

salary.team <- select(m2, yearID, teamID, sum_salary, W)
salary.oak <- filter(salary.team, teamID == "OAK")

salary.wins <- ggplot() +
    geom_point(data=salary.team, aes(x=W, y=sum_salary)) +
    geom_point(data=salary.oak, aes(x=W, y=sum_salary), color="red") +
    geom_text(data=salary.oak, aes(x=W + 5, y=sum_salary - 1, label="OAK"), 
        angle=45, color="red") +
    facet_wrap(~ yearID, nrow=5) +
    xlab("Wins") +
    ylab("Salary (in millions of dollars)") +
    theme_bw()

#googleViz
Motion=gvisMotionChart(m2, idvar="teamID", timevar="yearID")
plot(Motion)

#plotly
salary.team <- select(m2, yearID, teamID, sum_salary, W)
salary.team <- mutate(salary.team,ashwin=sum_salary/W)
saltoplotly <- select(m2, yearID, teamID, W)

test <- spread(saltoplotly, teamID, W)
drops <- c("yearID")
test <- test[,!(names(test) %in% drops)]

testasmatrix=data.matrix(test, rownames.force = NA)
plot_ly(z = testasmatrix, type = "surface")

plot_ly(z = test, type = "surface")


# 5. Looking at the graphs, in what years do you notice Oakland
# gaining a competitive advantage by using data science? Why
# did you pick those years?



# 6. What other insight can you provide with this data?