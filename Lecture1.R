##StatR 502 Lecture Notes 1/4/2018 

##clean up work space
rm(list = ls())

#should know function predict
?predict


#fortunes exercise
install.packages("fortunes")

library(fortunes)

fortunes::fortune(50)




##child iq data set 
install.packages("foreign")
library(foreign)
iq <- read.dta("C:\\Users\\BT0X\\Downloads\\kidiq.dta")


head(iq)

#install ggally 
install.packages("GGally")
library(GGally)


##summarizes relationships in data
ggpairs(iq)


##effect size  -- strength of correlation in the data 
#effect size is the value of the coefficient in a linear model


##Example 
set.seed(47) #sets the same starting point for random numbers
x <- c(1,2,3)
y <- c(2,3,4) + rnorm(3)

#bring in ggplot2
library(ggplot2)


#plot the data
plot(x,y,xlim = c(0,3), ylim = c(0,5))


mod1 <- lm(y ~ x)

summary(mod1) ##get regression summary

abline(mod1)


#estimate betas using beta hat waltz (x transpose x) inverse x transpose y!
#t(x) transposes x 
#solve() takes the inverse

solve(t(x) %*% x) %*% t(x) %*% y 



##fit linear models to exclude the intercept
mod2 <- lm(y ~ x + 0 ) ##forces the intercept to go thru 0 
summary(mod2)
#now we get the same output as the inital beta hat waltz


##we didn't provide an intercept, so need to add that to x 
#cbinds adds a column to an existing matrix

xx <- cbind(1,x)

#validate it worked correctly
xx

##lets redo beta hat waltz!! 

solve(t(xx) %*% xx) %*% t(xx) %*% y 

##now we get the same coefficients 



