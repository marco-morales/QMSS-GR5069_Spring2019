
#   ############################################################################
#       File-Name:        predict_population.R
#       Version:          R version 3.4.1 (2017-06-30)
#       Platform:         x86_64-apple-darwin15.6.0
#       Purpose:          Pull data from the Colorado Gov API about 
#                         female population and predict future population
#       Input Files:      API
#       Output Files:     MongoDB Collection
#       Status:           Completed
#   ############################################################################


library(fpp2)
library(tidyverse)
library(rjson)
library(RCurl)
library(ggplot2)
library(mongolite)

base_url = "https://data.colorado.gov/resource/tv8u-hswn.json?"
full_url = paste0(base_url, "county=Boulder",
                  "&$where=age between 20 and 40",
                  "&$select=year,age,femalepopulation")

full_url <- URLencode(full_url)

col_population_df <- fromJSON(getURL(full_url))

# turn columns to numeric and remove NA values
col_population_df$age <- as.numeric(col_population_df$age)
col_population_df$year <- as.numeric(col_population_df$year)
col_population_df$femalepopulation <- as.numeric(col_population_df$femalepopulation)


df <- data.frame(matrix(unlist(col_population_df), nrow=length(col_population_df), byrow=T))

names(df)<-c("age","population","year")

#specify the age for modeling
model_age = 23

#train data prepration
train<-filter(df,age == model_age)
train<-train[order(train$year),]
train <- distinct(train,age,population,year)
mytimeseries<-ts(train$population,start=c(1990),end=c(2011),frequency=1)

#fit the model
h_length=length(train$year)-(2011-1990+1)
fit1<-holt(mytimeseries,h=h_length,damped=TRUE,level=c(80,95),exponential = FALSE)

#organise the model outputs
model_fit<-fit1$fitted
model_predict<-fit1$mean
model_combined<-as.data.frame(append(model_fit,model_predict))
names(model_combined)<-c("population_prediction")

#append the outputs to the train data
outputs<-cbind(train,model_combined)

#write data into MongoDB database 
m <- mongo("{collection_name}", url = "mongodb+srv://{username}:{password}@{hostname}/{database_name}?serverSelectionTryOnce=false&retryWrites=true")
m$insert(outputs)