library(tidyverse)

#Load data
load("rda/rain.rda")

#Disable scientific notation
options(scipen = 999)

#-------------------------------------------------------------------------------------------------#
#                                 General Properties                                              #
#-------------------------------------------------------------------------------------------------#
#Dataset Dimensions
dim(dat)

#Dataset Structure
str(dat, vec.len = 2)

#Small sample of observations
head(dat) 

#Summary Statistics
summary(dat)

e1071::skewness(dat)

#Distinct values of each predictors
dat %>%
  summarise_all(~n_distinct(.))

#-------------------------------------------------------------------------------------------------#
#                                  Data Exploration                                               #
#-------------------------------------------------------------------------------------------------#
#The first thing to explore is the distribution of all predictors. That is a important aspect to know 
#before preprocessing the data set.
library(Hmisc)
dat %>% 
  select_if(is.numeric) %>%  
  hist.data.frame()
  
#As we can see, many variables are normally or right skewed normally distributed. However, Cloud9am and
#Cloud3pm doesn't look like continuous variables; rainfall and evaporation are extremely skewed; and, 
#is not normally distributed.

#The prediction of "RainTomorrow" is the project's goal. Therefore, it is worthwhile to understand its relationship
#with the other features.
dat %>%
  select(RainToday, RainTomorrow) %>%
  gather(cols, value) %>%
  ggplot(aes(cols, fill = value)) +
  geom_bar(position = "stack") +
  geom_label(aes(label = ..count..), stat = "count", position = position_stack(0.5), show.legend = FALSE) +
  theme_bw() +
  ylab("Frequency") +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank())

#"RainToday" and "RainTomorrow" have a similar number of No's and Yes's. It is not likely that every time that rains today in 
#Australia also rains tomorrow. This would be the perfect scenario. However, how much are we able to predict only using the
#"RainToday" predictor? There is also a bunch of NA's and a observation with no value at all. This should be fixed before modeling. 
tab <- table(dat$RainToday, dat$RainTomorrow)[2:3,2:3]
names(attributes(tab)$dimnames) <- c("RainToday", "RainTomorrow")

#From 140543, in 107103 the "RainToday" and "RainTomorrow" were the same. This means a staggering 76% accuracy. 
#This is a great start, and will be our baseline model.

#Before plotting, get rid of the observation with no value.
dat <- dat[1:145211, ]

#The other predictors should have also important patterns to predict if it is going to rain or not tomorrow.
dat %>%
  select_if(is.numeric) %>%
  mutate(RainTomorrow = dat$RainTomorrow) %>%
  gather(cols, value, -RainTomorrow) %>%
  na.omit(.) %>%
  ggplot(aes(RainTomorrow, value, fill = RainTomorrow)) +
  geom_boxplot() +
  facet_wrap(~cols, scales = "free")

#From this plot, it is clear that cloud3pm, humidity3pm and sunshine are the most important features to predict
#if it is going to rain tomorrow. A special note for Humidity3pm. Approximated 70% of the time when 
#the humidity is higher than 60, it rains in the next day.




