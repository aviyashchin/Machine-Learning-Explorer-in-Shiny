#####################################################
#####################################################
#####[01] Foundations of Statistics Lecture Code#####
#####################################################
#####################################################



###########################
#####One-Sample T-Test#####
###########################
set.seed(0)
heights = rnorm(n = 100, mean = 70, sd = 1) #Randomly generating 100 normally
                                            #distributed observations with a
                                            #mean of 70 and standard deviation
                                            #of 1.

View(data.frame(heights))
plot(density(heights), main = "Sample Distribution of Heights")
abline(v = 70, lwd = 2, lty = 2)
abline(v = 68, lwd = 2, lty = 2, col = "red")
legend("topright", c("True Mean = 70", "H0 Mean = 68"), lwd = 2,
       lty = 2, col = c("black", "red"))

boxplot(heights, main = "Sample Distribution of Heights")
abline(h = 70, lwd = 2, lty = 2)
abline(h = 68, lwd = 2, lty = 2, col = "red")
legend("topright", c("True Mean = 70", "H0 Mean = 68"), lwd = 2,
       lty = 2, col = c("black", "red"))

t.statistic = (mean(heights) - 68)/(sd(heights)/sqrt(100)) #Manually calculating
t.statistic                                                #the t-statistic
                                                           #comparing to 68.

pt(q = t.statistic, df = 99, lower.tail = TRUE) #P-value is extremely small;
                                                 #reject the null hypothesis
                                                 #in favor of the alternative.

t.test(heights, mu = 68, alternative = "greater") #Same test, using the t.test()
                                                  #function.

#The p-value for this test is extremely small (<.0005); thus, we reject the null hypothesis
#that the average height of individuals is 68 inches in favor of the alternative
#that the average height is greater than 68 inches at the 95% confidence level.



###########################
#####Two-Sample T-Test#####
###########################
set.seed(0)
SAT.Spring = rnorm(100, 1550, 200) #Randomly generating 100 normally distributed
                                   #observations with a mean of 1550 and a
                                   #standard deviation of 200.
SAT.Fall = rnorm(80, 1500, 210) #Randomly generating 80 normally distributed
                                #observations with a mean of 1500 and a standard
                                #deviation of 210.

plot(density(SAT.Spring), xlab = "SAT Score",
     main = "Sample Distribution of SAT Scores", col = "red")
lines(density(SAT.Fall), col = "blue")
legend("topright", c("Spring", "Fall"), lwd = 1, col = c("red", "blue"))

boxplot(cbind(SAT.Spring, SAT.Fall), main = "Sample Distribution of SAT Scores",
        col = c("red", "blue"))

#Manually calculating the t-statistic.
t.statistic = (mean(SAT.Spring) - mean(SAT.Fall))/sqrt(var(SAT.Spring)/100 + var(SAT.Fall)/80)
t.statistic

t.test(SAT.Spring, SAT.Fall, alternative = "two.sided") #Conducting the t-test for two
                                                        #samples, assessing the two-sided
                                                        #alternative hypothesis.

#The p-value is 0.01215, which is less than our threshold of 0.05; thus, we reject
#the null hypothesis that the average score on the spring SAT is the same as the
#fall SAT. We conclude in favor of the alternative that the average score of the
#two test administrations is different at the 95% confidence level.



################
#####F-Test#####
################
f.statistic = var(SAT.Fall)/var(SAT.Spring) #Manually calculating the F-statistic.
f.statistic

var.test(SAT.Fall, SAT.Spring, alternative = "two.sided") #Conducting the F-test to
                                                          #compare two variances,
                                                          #assessing the two-sided
                                                          #alternative hypothesis.

#The p-value is 0.1161, which is not less than our threshold of 0.05; we do
#not have evidence against the null hypothesis at the 95% confidence level. Thus,
#we retain the null hypothesis that the variance of test scores for the fall and
#spring SAT administrations is the same.



#######################
#####One-Way ANOVA#####
#######################
set.seed(0)
Low.Calorie = rnorm(200, 10, 1) #Randomly generating weight loss measurements
Low.Carb = rnorm(200, 8.5, 1)   #for various diet types.
Low.Fat = rnorm(200, 8, 1)
Control = rnorm(200, 0, 1)

Weight.Loss = c(Low.Calorie, Low.Carb, Low.Fat, Control) #Combining data into
Category = c(rep("Low Calorie", 200),                    #different consolidated
             rep("Low Carb", 200),                       #vectors.
             rep("Low Fat", 200),
             rep("Control", 200))

boxplot(Weight.Loss ~ Category,
        col = c("red", "orange", "yellow", "green"),
        main = "Distribution of Weight Loss\nfor Various Diets")

summary(aov(Weight.Loss ~ Category)) #Conducting the One-Way ANOVA on the weight
                                     #loss by considering each category of diet.

chi = chisq.test(quiz.data
                 )

#The p-value for this test is extremely small (<.0005). Thus, the average weight
#loss for at least one of the diet groups differs from the average weight loss of
#the others. We reject the null hypothesis that the average weight loss is the
#same for each of the types of diet.



##################################
#####X^2 Test of Independence#####
##################################
quiz.data = matrix(c(44, 21, 12, 18), nrow = 2, ncol = 2, byrow = TRUE)
dimnames(quiz.data) = list(Attendance = c("Present", "Absent"),
                           Grade = c("Pass", "Fail"))

chisq.test(quiz.data) #Conducting the X^2 test of independence data on the quiz
                      #data.

#The p-value for this test is very small (0.02001). This is an indication that
#under our null hypothesis, it would be extremely unlikely to observe results
#like we have observed in our data. The null hypothesis states that the attendance
#and grade variables are independent of one another; however, the test provides
#evidence against this assertion. Therefore, we conclude in favor of the
#alternative hypothesis that the variables of attendance and grade are not
#independent of one another.



########################################
#####Chick Weight Data Set Examples#####
########################################
library(datasets) #Load the base R datasets library.

help(chickwts) #See the help documentation for the chickwts dataset to see a
               #description of the included variables.

chickwts #Visually inspect the data.

summary(chickwts) #Returns the five-number summary, along with the mean,
                  #for any continuous variables; returns the count information
                  #for any categorical variables.

sd(chickwts$weight) #Returns the standard deviation of the weight variable.

var(chickwts$weight) #Returns the variance of the weight variable.

table(chickwts) #Returns a contingency table of weight and feed.

#We are told that the true average weight of chicks should be around 280 grams;
#however, we do not believe this is the case. We want to test the notion that the
#weight of chicks should be centered at a value lower than 280 grams. The average
#chick weight of our dataset is 261.3 grams. Is this significantly different for
#us to change our belief? To decide, we use a One-Sample T-Test:

plot(density(chickwts$weight), main = "Overall Distribution of Chick Weights")
abline(v = mean(chickwts$weight), lwd = 2, lty = 2)
abline(v = 280, lwd = 2, lty = 2, col = "red")
legend("topright", c("True Mean = 261.3", "H0 Mean = 280"), lwd = 2,
       lty = 2, col = c("black", "red"))

boxplot(chickwts$weight, main = "Overall Distribution of Chick Weights")
abline(h = mean(chickwts$weight), lwd = 2, lty = 2)
abline(h = 280, lwd = 2, lty = 2, col = "red")
legend("topright", c("True Mean = 261.3", "H0 Mean = 280"), lwd = 2,
       lty = 2, col = c("black", "red"))

t.test(chickwts$weight, mu = 280, alternative = "less")

#The p-value for this test is 0.02376 which is less than our threshold of 0.05.
#Thus, we conclude in favor of the alternative hypothesis; our data supports the
#notion that the true average chick weight is actually less than 280 grams.

#The chicks were fed one of six types of feed; is there evidence that the average
#chick weights are different among the varied categories of feed? To decide, we
#conduct a One-Way ANOVA:

boxplot(chickwts$weight ~ chickwts$feed,
        col = c("red", "orange", "yellow", "green", "blue", "violet"),
        main = "Distribution of Chick Weights\nfor Various Feed Types")

summary(aov(chickwts$weight ~ chickwts$feed))

#The p-value for this test is <.0005. This is extremely strong evidence that the
#average chick weight is different for the various feed types. We reject the null
#hypothesis that the mean value of chick weight is the same for all categories.

#We are specifically interested in the casein and meatmeal diet types. Do the
#reported chick weights have the same variance? To determine, conduct an F-test:
var.test(chickwts$weight[chickwts$feed == "casein"],
         chickwts$weight[chickwts$feed == "meatmeal"],
         alternative = "two.sided")

#The p-value for this test is 0.9739 meaning that, under the null hypothesis, we
#would expect to see results at least as extreme as ours about 97.39% of the time.
#Thus, our data does not support the alternative hypothesis that the variance among
#chick weights is different for the casein and meatmeal diet groups.