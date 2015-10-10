# ￼￼NYC Data Science Bootcamp Fall 2015
# Simple Linear Regression

# Question #1: Anatomical Data from Domestic Cats
# Load the ​catsdataset from the ​MASSlibrary. This dataset includes the body and heart weights of both male and female adult domestic cats.
install.packages("MASS")
library(MASS)
c<- cats

# 1. Create a scatterplot of heart weight versus body weight. From this plot alone, do you think simple linear regression would be a good fit for the data? Why?
ggplot(cats, aes(x=cats$Hwt, y=cats$Bwt)) +
  geom_point(aes(color = Sex)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+   # Add linear regression line 
  coord_cartesian() +
  theme_hc()

hist(cats$Hwt, xlab = "Heart Weight in G", main = "Histogram of HW")
hist(cats$Bwt, xlab = "Body Weight in KG", main = "Histogram of BW")

plot(cats)

lm(cats$Hwt ~ cats$Bwt)

#yes, because they're in a straight line more or les

# 2. Regress heart weight onto body weight. For this model:
# a. Write out the regression equation.
plot(x=cats$Bwt, y=cats$Hwt)

beta1 = sum((cats$Bwt - mean(cats$Bwt)) * (cats$Hwt - mean(cats$Hwt))) /sum((cats$Bwt - mean(cats$Bwt))^2)
beta0 = mean(cats$Hwt) - beta1*mean(cats$Bwt)

abline(beta0, beta1, lty = 2)


# b. Interpret the meaning of the coefficients in context of the problem.
#beta1 is around 4, so for every increase in 1kg of bodyweight you get a 4x increase of heart weight (in g).

#Beta0, the intercempt is 0.3, so a cat weighing -lbs will have a negative heart weight. (that was a joke)

# c. Are the coefficients significant? How can you tell?
ggplot(cats, aes(x=cats$Bwt, y=cats$Hwt)) +
  geom_point(aes(color = cats$Sex)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+   # Add linear regression line 
  coord_cartesian() +
  theme_hc()

model = lm(cats$Bwt ~ cats$Hwt)

# d. Is the overall regression significant? How can you tell? How does the answer to part c. relate?

residuals = cats$Hwt - (beta0 + beta1*cats$Bwt)

segments(cats$Bwt, cats$Hwt,
         cats$Bwt, (beta0 + beta1*cats$Bwt),
         col = "red")
text(cats$Hwt - .5, cars$dist, round(residuals, 2), cex = 0.5)

summary(model)
#Yes, overall 
# e. Find and interpret the RSE.
sum(residuals)
plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for Cars Dataset")
abline(h = 0, lty = 2)

qqnorm(model$residuals)
qqline(model$residuals)

#The residuals don't look like they're normal based on the Q-Q Plot. 

# f. Find and interpret the coefficient of determination.
confint(model)

# 3. Add the regression line to your plot from part 1.

# 4. Add a visualization of the residuals to your plot from part 3. Do any of the residuals seem abnormally large?

# 5. Construct 95% confidence intervals for the model coefficients. Interpret the intervals in context of the problem.

# 6. Assess each of the assumptions of the model.

# 7. Redraw the scatterplot and regression line from part 3 and add both confidence and prediction bands.

# a. Why is the prediction band wider than the confidence band?

# b. Why does the confidence band widen as it travels away from the center of the regression line?

# 8. Construct confidence and prediction intervals for body weights of 2.8 kg, 5 kg, and

# 10 kg. Do you foresee any issues with reporting any of these intervals?


# ￼￼Question #2: Considering Transformations
# Continue with the ​cats​dataset from the ​MASS​library.


# 1. Create a Box-Cox plot for transforming the ​catsregression you created in question 1 above.

# 2. Determine the best value of lambda for a Box-Cox transformation on the ​cats regression. (Hint: Try to balance interpretability and accuracy; when taking this perspective, there isn’t really a completely “correct” answer.)

# 3. Transform your data based on your answer to part 2.

# 4. Construct a new regression now using the transformed data.

# 5. Create a scatterplot of the transformed data and overlay the new regression line.

# 6. Inspect the summary information and validate the assumptions of the linear regression model. Is there anything to be concerned about in the new model?

# 7. Compare the models you created:

# a. Give one reason why you might use the original model instead of the Box-Cox transformed model.

# b. Give one reason why you might use the Box-Cox transformed model instead of the original model.

# 8. Attempt to apply a Box-Cox transformation on the model on which you already applied a Box-Cox transformation. What happens?
