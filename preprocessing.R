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
# - 1º - Feature Engineering 
# - 2º - Splitting Training set and Validation set
# - 3º - Remove features with very few non-unique values
#or close to zero variation
# - 4º - Remove predictors highly correlated
# - 5º - Handle NA values
# - 6º - Transform few skewed predictors (if necessary)
# - 7º - Normalization
#-----------------------------------------------------------------------------------------------------------------#
#                                          1. Feature Engineering                                                 #
#-----------------------------------------------------------------------------------------------------------------#
#The first step during pre processing is turning variables into classes that facilitates data modeling.
#It is recommended to use categorical predictors as factors when coding machine learning applications in R.
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
#                                          2. Splitting data set                                                  #
#-----------------------------------------------------------------------------------------------------------------#
#Before doing any preprocessing, it is recommended to split the data into train and test set. 
#The reason for that is to avoid data leak from the test set into the training set, 
#so all the preprocessing should be based on the training data.
set.seed(1, sample.kind = "Rounding")
test_ind <- createDataPartition(eng_dat$Location, times = 1, p = 0.1, list = FALSE)
validation_set <- eng_dat[test_ind, ]
train_set <- eng_dat[-test_ind, ]

#-----------------------------------------------------------------------------------------------------------------#
#                                          3. Near zero variance Features                                         #
#-----------------------------------------------------------------------------------------------------------------#
#Predictions are based on variations in our data. 
#Features with null or low standard deviation do
#not have predictive power, and must be removed
#from the data set.

#Columns with near zero variance in the data
nzv <- nearZeroVar(train_set) 

#None of the predictors have zero variance!

#-----------------------------------------------------------------------------------------------------------------#
#                                          4. Remove highly correlated predictors                                 #
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
validation_set <- validation_set[ ,-ind_col]

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

#-----------------------------------------------------------------------------------------------------------------#
#                                          5. Handle NA values                                                    #
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
validation_set <- validation_set[!is.na(validation_set$RainTomorrow), ]

#For imputation and Normalization, the standard deviation (sd),
#the mean, median and mode must be define in the training set.
#And afterwards, used to impute, scale and center both the
#test and the training set.

#Define the values of sd, mean, median and mode for the training set
train_mean <- train_set %>% 
  summarise(across(where(is.numeric) & 
                     !c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"), 
                   ~mean(.)))

train_median <- train_set %>% 
  select(c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall")) %>% 
  summarise(across(everything(), ~median(.)))

train_sd <- train_set %>% 
  select_if(is.numeric) %>%
  summarise(across(everything(), ~sd(.)))

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
#          5.1. Imputation - Numeric Features             #
#---------------------------------------------------------#
#----------------------------------------#
#           5.1.1 Training Set           #
#----------------------------------------#
#The data exploration showed that Evaporation, WindGustSpeed3pm, 
#WindSpeed9am,WindSpeed3pm, and Rainfall have huge outliers values.
#Those are going to be imputed by their median values.

#Impute NA's with median values
train_set <- train_set %>%
  mutate(across(c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"), function(x){
    y <- ifelse(is.na(x), median(x, na.rm = TRUE), x)
  })) 

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#Impute NA's with mean in all remaining numeric variables
train_set <- train_set %>%
  mutate(across(where(is.numeric) & !c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"), function(x){
                  y <- ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  })) 

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#----------------------------------------#
#             5.1.2 Test Set             #
#----------------------------------------#
#Here, we will use the same statistical parameters to 
#perform imputation in our test set.

#Impute NA's with median values in "Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"
for (i in 1:5){
  index <- match(c("Evaporation", "WindGustSpeed", "WindSpeed3pm","WindSpeed9am", "Rainfall"),names(validation_set))
  validation_set[,index[i]] <- ifelse(is.na(validation_set[,index[i]]), train_median[1,i], validation_set[,index[i]])
}

#Check if the imputation was done correctly
validation_set %>% summarise(across(everything(), ~sum(is.na(.))))

#Impute NA's with mean in all remaining numeric variables
for (i in 1:6){
  index <- match(c("MinTemp","MaxTemp","Sunshine","Humidity9am","Humidity3pm", "Pressure3pm"), names(validation_set))
  validation_set[,index[i]] <- ifelse(is.na(validation_set[,index[i]]), train_mean[1,i], validation_set[,index[i]])
}

#Check if the imputation was done correctly
validation_set %>% summarise(across(everything(), ~sum(is.na(.))))

#---------------------------------------------------------#
#          5.2. Imputation - Categorical Features         #
#---------------------------------------------------------#

#----------------------------------------#
#           5.1.1 Training Set           #
#----------------------------------------#
#Impute NA values in categorical variables
for (i in 1:7){
  index <- match(c("Location","WindGustDir","WindDir9am","WindDir3pm","Cloud9am","Cloud3pm","RainToday"), names(train_set))
  train_set[,index[i]] <- ifelse(is.na(train_set[,index[i]]), train_mode[1,i], train_set[,index[i]])
}

#Check if the imputation was done correctly
train_set %>% summarise(across(everything(), ~sum(is.na(.))))

#----------------------------------------#
#             5.1.2 Test Set             #
#----------------------------------------#
#Impute NA values in categorical variables
for (i in 1:7){
  index <- match(c("Location","WindGustDir","WindDir9am","WindDir3pm","Cloud9am","Cloud3pm","RainToday"), names(validation_set))
  validation_set[,index[i]] <- ifelse(is.na(validation_set[,index[i]]), train_mode[1,i], validation_set[,index[i]])
}

#Check if the imputation was done correctly
validation_set %>% summarise(across(everything(), ~sum(is.na(.))))

#-----------------------------------------------------------------------------------------------------------------#
#                                                6. Transformations                                               #
#-----------------------------------------------------------------------------------------------------------------#



#-----------------------------------------------------------------------------------------------------------------#
#                                                7. Normalization                                                 #
#-----------------------------------------------------------------------------------------------------------------#
## centered
sweep(trainData, 2L, trainMean) # using the default "-" to subtract mean column-wise   
## centered AND scaled
norm2.testData <- sweep(sweep(testData, 2L, trainMean), 2, trainSd, "/")




#-----------------------------------------------------------------------------------------------------------------#
#                                              8. Saving Final Data Sets                                          #
#-----------------------------------------------------------------------------------------------------------------#



#Cleaning Global Environment
rm(index, test_ing, eng_dat, dat, nzv, ind_col, corr, dat_cat, names_col)
