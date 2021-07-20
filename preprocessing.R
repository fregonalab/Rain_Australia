library(tidyverse)
library(caret)
library(corrplot)
library(vcd)
library(e1071)

#Load data
load("rda/rain.rda")

#Preprocessing steps:
# - 1º - Remove features with very few non-unique values
#or close to zero variation
# - 2º - Remove predictors highly correlated
# - 3º - Handle NA values
# - 4º - Transform a few predictors (if necessary)
# - 5º - Scale the whole data set
#-----------------------------------------------------------------------------------------------------------------#
#                                          1. Near zero variance Features                                         #
#-----------------------------------------------------------------------------------------------------------------#
#Predictions are based on variations in our data. 
#Features with null or low standard deviation do
#not have predictive power, and must be removed
#from the data set.

#Columns with near zero variance in the data
nzv <- nearZeroVar(dat) 

#None of the predictors have zero variance!

#-----------------------------------------------------------------------------------------------------------------#
#                                          2. Remove highly correlated predictors                                 #
#-----------------------------------------------------------------------------------------------------------------#
#In a nutshell, to decrease redundancy in the data set,
#highly correlated predictors must be removed. However,
#to find if there is multicollinearity in our data, we
#need to investigate numeric and categorical predictors,
#separately.

#---------------------------------------------------------#
#                2.1 Numerical Predictors                 #
#---------------------------------------------------------#
#The numerical predictors are:
str(dat)

#Calculate the correlation between numeric features:
corr <- cor(dat[ , c(3:7,9,12:21)], use = "pairwise.complete.obs")

#Plot of correlation
corrplot(corr, method = c("circle")) 

#It looks like that there are multiple predictors highly correlated to each other, such as:
# - MinTemp, MaxTemp, Temp9am and Temp3pm
# - Sunshine, Cloud9am, Cloud3pm
# - Pressure9am, Pressure3pm
corr[c(1:2,5,11:16) ,c(1:2,5,11:16)]

#Based on the correlation values, Sunshine; Cloud9am; Cloud3pm are indeed correlated
#to each other, but not that much to get rid of them. 
#Temp9am and Temp3pm are highly correlated between them and with MaxTemp. Therefore, 
#there is no need to keep them.
#Pressure9am and Pressure3pm are also highly correlated to each other, and one
#of them should be removed.
clean_num <- dat[ ,c(1:16, 18:19,22:23)]
  
#---------------------------------------------------------#
#                2.1 Categorical Predictors               #
#---------------------------------------------------------#
#The categorical predictors are:
str(dat)

#Dataframe with only the categorical predictors, and no NA values
dat_cat <- na.omit(dat[ , c(2,8,10:11,22)])

#A direct correlation value for nominal categorical data is
#not available in the literature. However, the dependency 
#between those variables can also be found. 
#Cramer's V is a statistic measuring the strength of 
#association or dependency between two (nominal) categorical 
#variables.

#Null Hypothesis:
#Ho = The categorical variables are independent

#Load Function - catcor()
source("functions/catcor.R")

#Define variables to be correlated
vars <- c("WindGustDir","Location","WindDir9am","WindDir3pm","RainToday")

#Apply function
catcor(vars,dat_cat)

#From the Cramer V's matrix, no pair of features are highly correlated.

#Clean Global Environment
rm(dat_cat, catcor, vars, dat)

#-----------------------------------------------------------------------------------------------------------------#
#                                          3. Handle NA values                                                    #
#-----------------------------------------------------------------------------------------------------------------#
#First, let's understand how much data are missing.

#Plot with missing data highlighted in white
Amelia::missmap(clean_num)

#Table with how many values are missing per predictor.
clean_num %>%
  summarise(across(everything(), function(x) round(sum(is.na(x))/length(x), 3)*100))

#As we can see from the plot, 10% of the data set is missing. Also, the predictors with less data available are
#sunshine(n = 47.9%), Cloud3pm(n = 40.8%), Evaporation(n = 43.1%) and Cloud9am(n = 38.4%).

#There are many ways to deal with missing data. The approach we chose is called imputation. In a nutshell, 
#imputation is a process of replace missing data by something else. Here, we will apply the simplest approach.
#The categorical missing data will be replaced by the modal value of each predictor, and the numerical imputation 
#by the predictors mean(few outliers, normal distributed) or median (many outliers). Using knn as a imputation
#mechanism would be highly recommended, but it will not be performed in this project.






#-----------------------------------------------------------------------------------------------------------------#
#                                                4. Transformations                                               #
#-----------------------------------------------------------------------------------------------------------------#










