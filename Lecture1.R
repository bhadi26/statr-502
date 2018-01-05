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



