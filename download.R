library(kaggler)

#Saving keys for accessing Kaggle API
kgl_auth(username = "fregonalab", key = "41fbf3ce8122d1a75626a08ade49a209")

#List of all data sets available on Kaggle.
#To filter the results, use:
# - search: filter by Kaggle's data set name - word filtering
# - page: filter by the page's number on Kaggle data set list
kgl_datasets_list(search = "Rain",
                  page = 1)

#Download Kaggle's API adress to the data
kgl_api <- kgl_datasets_download_all(owner_dataset = "jsphyg/weather-dataset-rattle-package")

#Download file based on API's adress
temp <- tempfile()
download.file(kgl_api$url, temp)

#Unzip and download .csv file 
dat <- read.csv(unz(temp, "weatherAUS.csv"))

#Remove unnecessary objects
rm(temp, kgl_api)

#Save data as .rda files in order to backup
#original data before cleaning
save(dat, file = "rda/rain.rda")


