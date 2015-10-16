 ######################################################
######################################################
#####[04] Multiple Linear Regression Lecture Code#####
######################################################
######################################################


#####################################################
#####Example using the State Information Dataset#####
#####################################################å
help(state.x77)
state.x77 #Investigating the state.x77 dataset.

states = as.data.frame(state.x77) #Forcing the state.x77 dataset to be a dataframe.

#Cleaning up the column names so that there are no spaces.
colnames(states)[4] = "Life.Exp"
colnames(states)[6] = "HS.Grad"

#Creating a population density variable.
states[,9] = (states$Population*1000)/states$Area
colnames(states)[9] = "Density"

#Basic graphical EDA for the states dataset.
plot(states)

#Can we estimate an individual's life expectancy based upon the state in which
#they reside?

 #The Modern Applied Statistics library.
# 
# forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
# baic = step(model.full, scope, direction = "backward", k = 2)
# bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
# bothBIC.full = step(model.full, scope, direction = "both", k = log(50))





auto.multiregress <- function(target, dat){
  
  require(MASS)
  assign("dat",dat,envir=.GlobalEnv)
  
  n = nrow(dat)
  x = formula.stringr(target, full=FALSE)
  y = formula.stringr(target, full=TRUE)
  
  model.empty = lm(eval(x), data=dat) # create models 
  model.full = lm(eval(y), data=dat)
  
  # scope of steps
  scope = list(lower = formula(model.empty), upper = formula(model.full))
  
  faic = step(model.empty, scope, direction = "forward", k = 2)
  baic = step(model.full, scope, direction = "backward", k = 2)
  bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
  bothAIC.full = step(model.full, scope, direction = "both", k = 2)
  forwardBIC = step(model.empty, scope, direction = "forward", k = log(n))
  backwardBIC = step(model.full, scope, direction = "backward", k = log(n))
  bothBIC.empty = step(model.empty, scope, direction = "both", k = log(n))
  bothBIC.full = step(model.full, scope, direction = "both", k = log(n))
  
  name = c("Forward AIC", "Backward AIC", "Both AIC Empty", "Both AIC Full", "Forward BIC","backward BIC","both Bic empty", "Both BIC Full")
  lm = list(faic$call, baic$call, bothAIC.empty$call, bothAIC.full$call, forwardBIC$call, backwardBIC$call, bothBIC.empty$call, bothBIC.full$call)
  model = c(as.character(faic$call[2]), as.character(baic$call[2]), as.character(bothAIC.empty$call[2]), as.character(bothAIC.full$call[2]), as.character(forwardBIC$call[2]), as.character(backwardBIC$call[2]), as.character(bothBIC.empty$call[2]), as.character(bothBIC.full$call[2]))
  rsq = c(summary(eval(faic$call))[8]$r.squared, summary(eval(baic$call))[8]$r.squared, summary(eval(bothAIC.empty$call))[8]$r.squared, summary(eval(bothAIC.full$call))[8]$r.squared, summary(eval(forwardBIC$call))[8]$r.squared, summary(eval(backwardBIC$call))[8]$r.squared, summary(eval(bothBIC.empty$call))[8]$r.squared, summary(eval(bothAIC.full$call))[8]$r.squared)
  
  pval= lapply(lm, function(x) lmp(eval(x)))
  
  df = data.frame(name, model, rsq)
  
  return (df)
}

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


formula.stringr <- function (target, full){
  ifelse(full == TRUE, return(paste(target,'~ .')), return(form = paste(target,'~ 1')))
}



##########################
###   Test Functions
##########################

df.tbl=auto.multiregress("Life.Exp", states)



