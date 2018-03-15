#packages
library(ggplot2)
library(dplyr)
library(broom)
library(car)
library(arm) #invlogit
library(ggthemes)


#Bring in data 
setwd("~/Documents/StatR-502/finalproject")
movies.pr <- read.csv("Data/processed_movies.csv")


#Let's start modeling!
#Because our outcome variable is binary, we want to use logistic regression (which is a binomial GLM)

mod1 <-  glm(data = movies.pr, profitable ~ budget , family = binomial(link = "logit"))
mod2 <-  glm(data = movies.pr, profitable ~ log(budget), family = binomial(link = "logit"))
mod3 <-  glm(data = movies.pr, profitable ~ budget + revenue, family = binomial(link = "logit"))
mod4 <-  glm(data = movies.pr, profitable ~ log(budget) + log(revenue), family = binomial(link = "logit"))
mod5 <-  glm(data = movies.pr, profitable ~ revenue, family = binomial(link = "logit"))
mod6 <-  glm(data = movies.pr, profitable ~ log(revenue), family = binomial(link = "logit"))
mod7 <-  glm(data = movies.pr,
            profitable ~ log(revenue) + release_year + event + runtime + vote_count + 
              vote_average + genre, 
             family = binomial(link = "logit"))

#does a log transform on run time help?  (not really based on AIC)
mod8 <-  glm(data = movies.pr,
             profitable.ind ~ log(revenue) + release_year + event + log(runtime) + vote_count + 
               vote_average + genre, 
             family = binomial(link = "logit"))


#augment model
mod7.augment <- augment(mod7, type.predict = "response")


BIC(mod1,mod2)
#scale run time and votecount 
movies.pr$vote_count.z <- (movies.pr$vote_count - mean(movies.pr$vote_count)) / sd(movies.pr$vote_count)
movies.pr$runtime.z <- (movies.pr$runtime - mean(movies.pr$runtime)) / sd(movies.pr$runtime)
movies.pr$revenue.z <- (movies.pr$revenue - mean(movies.pr$revenue)) / sd(movies.pr$revenue)



#add in scaled variables (take off the transform on run time)

mod9 <-  glm(data = movies.pr,
             profitable.ind ~ log(revenue) + release_year + event + runtime.z + vote_count.z + 
               vote_average + genre, 
             family = binomial(link = "logit"))

summary(mod9)

#does the log tranform make a difference?
mod12 <-  glm(data = movies.pr,
             profitable.ind ~ revenue + release_year + event + runtime.z + vote_count.z + 
               vote_average + genre, 
             family = binomial(link = "logit"))

#yes, let's use the log transform


#does using a 1 or 0 for the response variable make a difference? 
mod10 <-  glm(data = movies.pr, profitable.ind ~ budget , family = binomial(link = "logit"))

#replace revenue with scaled revenue (this doesn't work b/c of the log transform)
mod11 <-  glm(data = movies.pr,
                       profitable.ind ~ log(revenue.z) + release_year + event + runtime.z + vote_count.z + 
                         vote_average + genre, 
                       family = binomial(link = "logit"))

#Does event actually help? 
mod13 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue) + release_year +  runtime.z + vote_count.z + 
                vote_average + genre, 
              family = binomial(link = "logit"))


display(mod9)

#compare models

BIC(mod1,mod2, mod3, mod4, mod5, mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13) 

#remove genre
mod14 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue) + release_year +  runtime.z + vote_count.z +  vote_average, 
              family = binomial(link = "logit"))


BIC(mod13,mod14, mod15,mod16,mod17, mod18, final.mod, mod19)

#remove release year 
mod15 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue)  +  runtime.z + vote_count.z +  vote_average, 
              family = binomial(link = "logit"))

#remove vote average
mod16 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue)  +  runtime.z + vote_count.z, 
              family = binomial(link = "logit"))


#remove vote count 
mod17 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue)  +  runtime.z + vote_average, 
              family = binomial(link = "logit"))

#test model again
mod18 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue)  +  runtime.z + vote_average + vote_count.z, 
              family = binomial(link = "logit"))

#add genre back in 
mod19 <-  glm(data = movies.pr,
              profitable.ind ~ log(revenue)  +  runtime.z + vote_average + vote_count.z + genre, 
              family = binomial(link = "logit"))


#this tells us we shouldn't use both budget and revenue
vif(mod3)
vif(mod4)



#Choose moodel 
final.mod <-  glm(data = movies.pr,
                  profitable.ind ~ log(revenue)  +  runtime.z + vote_average + vote_count.z, 
                  family = binomial(link = "logit"))





#how's the vif? 
vif(final.mod)


#let's augment 
final.mod.aug <- augment(final.mod, type.predict = "response")

#let's plot!  (#just a regular smoother)
ggplot(data = final.mod.aug, aes(x = log.revenue., y = profitable.ind)) + 
        geom_jitter(height = 0.02) +  
        geom_point(data = final.mod.aug, aes(x = log.revenue., y = .fitted), color = "#00cdcd", alpha = 0.2) +
        geom_smooth(method = "glm", se = F, 
                    method.args = list(family = "binomial")) + 
        theme_few() + 
        labs(x = "Log Revenue", y = "Probability of Profitability") +
        ggtitle("Profitability - Actual vs. Predicted")
        

#Plot redisuals (binned)
#bin
profit.bin <- final.mod.aug %>%
                mutate(q_group = cut(log.revenue.,
                                     breaks = quantile(log.revenue., seq(0, 1, length.out = 30),
                                                       include.lowest = TRUE))) %>%
                group_by(q_group) %>%
                summarize(.resid = mean(.resid),
                          log.revenue = mean(log.revenue.))

#plot
resid.plot1 <- ggplot(profit.bin, aes(x = log.revenue, y = .resid)) +
                geom_point() +
                geom_smooth() + 
                ggtitle("Final Model") + 
                labs(x = "Log Revenue", y = "Residual")

resid.plot1 + theme_few()


#Compare predicted to actual 
final.mod.aug$predicted <- round(final.mod.aug$.fitted)

knitr::kable(with(final.mod.aug, table(profitable.ind, predicted)))

knitr::kable(with(final.mod.aug, table(predicted, profitable.ind)))

nrow(filter(final.mod.aug, predicted == 1 & profitable.ind == 0))


#plot with comparison of prediction
ggplot(data = final.mod.aug, aes(x = log.revenue., y = profitable.ind)) + 
  geom_jitter(height = 0.02) +  
  geom_point(data = final.mod.aug, aes(x = log.revenue., y = .fitted), color = "#00cdcd", alpha = 0.2) +
  theme_few() + 
  labs(x = "Log Revenue", y = "Probability of Profitability") +
  ggtitle("Profitability - Actual vs. Predicted") + 
  facet_grid(profitable.ind ~ predicted)










