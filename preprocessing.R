library(tidyverse)
library(caret)
library(corrplot)
library(vcd)
library(e1071)
library(stats)
library(lubridate)
library(DescTools)

#Load data
load("rda/rain.rda")

#Preprocessing steps:
# - 1º - Split data set into training set and validation set
# - 2º - Remove features with very few non-unique values
#or close to zero variation
# - 3º - Remove predictors highly correlated
# - 4º - Handle NA values
# - 5º - Transform few skewed predictors (if necessary)
# - 6º - Feature Scaling
# - 7º - Split training set and validation set into outcome and predictors

#Before anything, we will also remove the observation with a "blank" value. 
ind_blank <- which(dat$RainTomorrow == "")
dat <- dat[-ind_blank, ]

#To facilitate the preprocessing step, we are going to turn categorical data into factors.
#As we have seen in EDA, Location; WindGustDir, WindGustSpeed, WindDir9am, WindDir3pm, Cloud3pm, Cloud9am,
#RainToday and RainTomorrow.

#Turn all character features to factors
eng_dat <- as.data.frame(unclass(dat), stringsAsFactors = TRUE)

#There are still 3 categorical predictors problematic, date, Cloud3pm and Cloud9am.

#Turn Date predictor into date class 
eng_dat[,"Date"] <- ymd(eng_dat$Date)

#Turn Cloud3pm and Cloud9am into factor
ind <- match(c("Cloud3pm","Cloud9am"), names(eng_dat))
eng_dat[,ind] <- lapply(eng_dat[ , ind],factor) 

#Check if the features were rightly converted
str(eng_dat)

#-----------------------------------------------------------------------------------------------------------------#
#                                          1. Splitting data set                                                  #
#-----------------------------------------------------------------------------------------------------------------#
#Before doing any preprocessing, it is recommended to split the data into train and test set. 
#The reason for that is to avoid data leak from the test set into the training set, 
#so all the preprocessing should be based on the training data.
set.seed(1, sample.kind = "Rounding")
test_ind <- createDataPartition(eng_dat$Location, times = 1, p = 0.1, list = FALSE)
test_set <- eng_dat[test_ind, ]
train_set <- eng_dat[-test_ind, ]

#-----------------------------------------------------------------------------------------------------------------#
#                                          2. Near zero variance Features                                         #
#-----------------------------------------------------------------------------------------------------------------#
#Predictions are based on variations in our data. 
#Features with null or low standard deviation do
#not have predictive power, and must be removed
#from the data set.

#Columns with near zero variance in the data
nzv <- nearZeroVar(train_set) 

#None of the predictors have zero variance!

#-----------------------------------------------------------------------------------------------------------------#
#                                          3. Remove highly correlated predictors                                 #
#-----------------------------------------------------------------------------------------------------------------#
#In a nutshell, to decrease redundancy in the data set,
#highly correlated predictors must be removed. However,
#to find if there is multicollinearity in our data, we
#need to investigate numeric and categorical predictors,
#separately.

#---------------------------------------------------------#
#                4.1 Numerical Predictors                 #
#---------------------------------------------------------#
#The numerical predictors are:
str(train_set)

#Calculate the correlation between numeric features:
names_col <- train_set %>% select_if(is.numeric) %>% names()
ind_col <- match(names_col, names(train_set))
corr <- cor(train_set[ , ind_col], use = "pairwise.complete.obs")

#Plot of correlation
corrplot(corr, method = "circle", order = "hclust", addrect = 6, addCoef.col="black") 

#It looks like that there are multiple predictors highly correlated to each other, such as:
# - MinTemp, MaxTemp, Temp9am and Temp3pm
# - Pressure9am, Pressure3pm

#Based on the correlation values, the following variables are highly correlated
#to each other. Therefore, there is no need to keep all of them. 
#Temp9am and Temp3pm 
#Temp9am and MaxTemp
#Temp3pm and MaxTemp
#Pressure9am and Pressure3pm 

#We will remove one of each pair to prevent multicollinearity.
ind_col <- match(c("Temp9am","Temp3pm","Pressure9am"), names(train_set))
train_set <- train_set[ ,-ind_col]
test_set <- test_set[ ,-ind_col]

#---------------------------------------------------------#
#                4.1 Categorical Predictors               #
#---------------------------------------------------------#
#The categorical predictors are:
str(train_set)

#Dataframe with only the categorical predictors
names_col <- train_set %>% select_if(is.factor) %>% select(-RainTomorrow) %>% names()
ind_col <- match(names_col, names(train_set))
dat_cat <- train_set[ , names_col]

#A direct correlation value for nominal categorical data is
#not available in the literature. However, the dependency 
#between those variables can also be found. 
#Cramer's V is a statistic measuring the strength of 
#association or dependency between two (nominal) categorical 
#variables.

#Null Hypothesis:
#Ho = The categorical variables are independent

#Calculate Cramer V association metric, and plot it in a corrplot
corrplot::corrplot(DescTools::PairApply(as.matrix(dat_cat), DescTools::CramerV, useNA = "ifany"), 
                   addCoef.col = "black",
                   method = "color")

#From the Cramer V's matrix, no pair of features are highly correlated.

#Cleaning Global environment
rm(ind, ind_col, test_ind, eng_dat, dat, ind_blank, dat_cat, corr, nzv, names_col)

#-----------------------------------------------------------------------------------------------------------------#
#                                          4. Handle NA values                                                    #
#-----------------------------------------------------------------------------------------------------------------#
#First, let's understand how much data are missing.

#Plot with missing data highlighted in white
Amelia::missmap(train_set)

#Table with how many values are missing per predictor.
train_set %>%
  summarise(across(everything(), function(x) round(sum(is.na(x))/length(x), 3)*100))

#As we can see from the plot, 11% of the data set is missing. Also, the predictors with less data available are
#sunshine(n = 47.9%), Cloud3pm(n = 40.8%), Evaporation(n = 43.1%) and Cloud9am(n = 38.4%).

#There are many ways to deal with missing data. The approach we chose is called imputation. In a nutshell, 
#imputation is a process of replace missing data by something else. Here, we will apply the simplest approach.
#The categorical missing data will be replaced by the modal value of each predictor, and the numerical imputation 
#by the predictors mean(few outliers, normal distributed) or median (important outliers). Using knn as a imputation
#mechanism would be highly recommended, but it will not be performed in this project. The caret package provide a
#way of using this approach.

#A important note is that we can't impute the predictor RainTomorrow for missing values, it will be predicted.
#Thus, we have to drop the rows with missing values first, before imputing. This is done in the train and test set.
train_set <- train_set[!is.na(train_set$RainTomorrow), ]
test_set <- test_set[!is.na(test_set$RainTomorrow), ]

#For imputation and Normalization, the standard deviation (sd),
#the mean, median and mode must be define in the training set.
#And afterwards, used to impute, scale and center both the
#test and the training set.

#Define the values of sd, mean, median and mode for the training set
train_mean <- train_set %>% 
  summarise(across(where(is.numeric) & 
                     !c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"), 
                   ~mean(., na.rm = TRUE)))

train_median <- train_set %>% 
  select(c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall")) %>% 
  summarise(across(everything(), ~median(., na.rm = TRUE)))

#Base R does not have the mode operator. We have built it, and stored it 
#as a function.

#Download mode function
source("functions/mode.R")

#Define mode for all predictors
train_mode <- train_set %>% 
  select_if(is.factor) %>% 
  na.omit(.) %>%
  summarise(across(everything(), ~Mode(.)))

#---------------------------------------------------------#
#          4.1. Imputation - Numeric Features             #
#---------------------------------------------------------#
#The function for the imputation process is define as follows:
source("functions/imputation.R")

#----------------------------------------#
#           4.1.1 Training Set           #
#----------------------------------------#
#The data exploration showed that Evaporation, WindGustSpeed3pm, 
#WindSpeed9am,WindSpeed3pm, and Rainfall have huge outliers values.
#Those are going to be imputed by their median values.

#Impute NA's with median values
vars <- c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall")
train_set <- Imputation(train_set, train_median, vars)

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#Impute NA's with mean in all remaining numeric variables
vars <- c("MinTemp","MaxTemp","Sunshine","Humidity9am","Humidity3pm","Pressure3pm")
train_set <- Imputation(train_set, train_mean, vars)

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#----------------------------------------#
#             4.1.2 Test Set             #
#----------------------------------------#
#Here, we will use the same statistical parameters to 
#perform imputation in our test set.

#Impute NA's with median values in "Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"
vars <- c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall")
test_set <- Imputation(test_set, train_median, vars)

#Check if the imputation was done correctly
test_set %>% summarise(across(everything(), ~sum(is.na(.))))

#Impute NA's with mean in all remaining numeric variables
vars <- c("MinTemp","MaxTemp","Sunshine","Humidity9am","Humidity3pm","Pressure3pm")
test_set <- Imputation(test_set, train_mean, vars)

#Check if the imputation was done correctly
test_set %>% summarise(across(everything(), ~sum(is.na(.))))

#---------------------------------------------------------#
#          4.2. Imputation - Categorical Features         #
#---------------------------------------------------------#

#----------------------------------------#
#           4.1.1 Training Set           #
#----------------------------------------#
#Impute NA values in categorical variables
vars <- c("Location","WindGustDir","WindDir9am","WindDir3pm","Cloud9am","Cloud3pm","RainToday")
train_set <- Imputation(train_set, train_mode, vars)

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#----------------------------------------#
#             4.1.2 Test Set             #
#----------------------------------------#
#Impute NA values in categorical variables
vars <- c("Location","WindGustDir","WindDir9am","WindDir3pm","Cloud9am","Cloud3pm","RainToday")
test_set <- Imputation(test_set, train_mode, vars)

#Check if the imputation was done correctly
test_set %>% summarise(across(everything(), ~sum(is.na(.))))

#Clean Global Environment
rm(train_mode, train_median, train_mean, Imputation, Mode, vars) 
#-----------------------------------------------------------------------------------------------------------------#
#                                                5. Transformations                                               #
#-----------------------------------------------------------------------------------------------------------------#
#According to the data exploration, the distribution of "Evaporation", "Rainfall", "WindSpeed9am", and "WindSpeed3pm" 
#are overly asymmetric. The skewness function is a numeric evidence of this observation. Values higher than 1 must be
#transformed.
train_set %>%
  summarise(across(where(is.numeric), ~skewness(.)))

#As we can see, "Evaporation" and "Rainfall" are extremely skewed and must be transformed.
#We are going to apply a cube root transformation, because all of them are extremely right skewed.
train_set <- train_set %>%
  mutate(across(c("Evaporation", "Rainfall"), function(x) x^(1/3)))

#Rainfall is still right skewed. 
train_set %>%
  summarise(across(c("Evaporation", "Rainfall"), ~skewness(.)))

#-----------------------------------------------------------------------------------------------------------------#
#                                              6. Feature Scaling                                                 #
#-----------------------------------------------------------------------------------------------------------------#
#Feature scaling is a method used to normalize the range of features. This domain is subdivided in two main categories,
#Normalization and Standardization. Normalization is a scaling technique in which values are shifted and re scaled 
#so that they end up ranging between 0 and 1. On the other hand, Standardization is a technique where the values 
#are centered around the mean with a unit standard deviation. This means that the mean of the attribute becomes 
#zero and the resultant distribution has a unit standard deviation. Here, we are going to apply both.

#A important note is that the mean and sd are from the training set for either the training and validation sets. The
#caret package has a function called preProcess, which facilitates the whole process.

#Apply preProcess in the train_set
train_param <- preProcess(train_set,
                    method = "scale","center")

#Scale train_set with parameters obtained before
train_set <- predict(train_param, train_set)

##Scale train_set with the same parameters obtained before
test_set <- predict(train_param, test_set)

#-----------------------------------------------------------------------------------------------------------------#
#                       7.Split training set and validation set into outcome and predictors                       #                            #
#-----------------------------------------------------------------------------------------------------------------#
#To improve coding performance, we are going to split the data sets into y and x. y represents the outcomes,
#and x the predictors. 

# Splitting the data into y and x
x_train <- subset(train_set, select = -c(RainTomorrow,Date,Location))
y_train <- subset(train_set, select = c(RainTomorrow))

x_test <- subset(test_set, select = -c(RainTomorrow,Date,Location))
y_test <- subset(test_set, select = c(RainTomorrow))

#Clean Global Environment
rm(test_set,train_set,train_param)
#-----------------------------------------------------------------------------------------------------------------#
#                                              8. Saving Final Data Sets                                          #
#-----------------------------------------------------------------------------------------------------------------#
#Saving final data sets as .rda files
save(x_train,file = "rda/x_train.rda")
save(y_train,file = "rda/y_train.rda")
save(x_test,file = "rda/x_test.rda")
save(y_test,file = "rda/y_test.rda")

