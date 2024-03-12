#Library
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)

# Read file
setwd("E:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", 
                            col_types = cols(date = col_date(format = "%m/%d/%Y")))
owid_covid_data$new_cases <- abs(owid_covid_data$new_cases)  
owid_covid_data$new_deaths <- abs(owid_covid_data$new_deaths)

# Select data
countries_covid_data<-subset(owid_covid_data, location == "Venezuela" | location == "Brazil" | location == "Chile") %>% group_by(location)

countries_covid_data<- countries_covid_data[,c('location', 'new_cases', 'new_deaths')] %>% group_by(location)


#Problem iii
#1/
no_report_new_cases <- countries_covid_data %>% summarise(no_report_days = sum(is.na(new_cases))+ sum (new_cases == 0, na.rm = TRUE))
print(no_report_new_cases)

no_report_new_deaths <- countries_covid_data %>% summarise(no_report_days  = sum(is.na(new_deaths)) + sum(new_deaths == 0, na.rm = TRUE))
print(no_report_new_deaths)

# Prepare data for 2
report_new_cases<- subset(countries_covid_data, !(is.na(new_cases)))
report_new_cases<- subset(countries_covid_data, new_cases != 0)
report_new_deaths<- subset(countries_covid_data, !(is.na(new_deaths)))
report_new_deaths<- subset(countries_covid_data, new_deaths != 0)

#2/
new_cases_min<- report_new_cases %>% summarise(min_days = sum(new_cases == min(new_cases)))
print(new_cases_min)

new_deaths_min <- report_new_deaths %>% summarise(min_days = sum(new_deaths == min(new_deaths)))
print(new_deaths_min)

#3/
new_cases_max <- report_new_cases %>% summarise(max_days = sum(new_cases == max(new_cases)))
print(new_cases_max)

new_deaths_max <- report_new_deaths %>% summarise(max_days = sum(new_deaths == max(new_deaths)))
print(new_deaths_max)

#4/
no_report_data <- countries_covid_data %>% summarise(Infection = sum(is.na(new_cases) | new_cases == 0), Deaths = sum(is.na(new_deaths) | new_deaths == 0) ) %>% rename(Countries = location)
print(no_report_data)

report_data <- countries_covid_data %>% summarise(Infection = sum(!(is.na(new_cases) | new_cases == 0)), Deaths = sum(!(is.na(new_deaths) | new_deaths == 0)) ) %>% rename(Countries = location)
print(report_data)

#Prepare data for 5,6,7,8
countries_covid_data<-subset(owid_covid_data, location == "Venezuela" | location == "Brazil" | location == "Chile") %>% group_by(location)
Countries<- data.frame(c("Brazil", "Chile", "Venezuela"))
colnames(Countries) = c("Countries")

min_days_no_report = function(x,y){
  count<-1
  country_data<-subset(x, x$location == y)
  if(dim(country_data)[1] == 0) return(0)
  country_date<-country_data[4]
  len = dim(country_date)[1]
  arr_count<-c(len)
  if(len == 1) return(1)
  for (i in 2:len){
    if((country_date[i,] - country_date[i-1,]) == 1){
      count = count + 1
    }
    else {
      arr_count<-append(arr_count, count)
      count = 1
    }
  }
  arr_count<-append(arr_count, count)
  return (min(arr_count))
}

max_days_no_report = function(x,y){
  count<-1
  country_data<-subset(x, x$location == y)
  if(dim(country_data)[1] == 0) return(0)
  country_date<-country_data[4]
  len = dim(country_date)[1]
  arr_count<-c(1)
  if(len == 1) return(1)
  for (i in 2:len){
    if((country_date[i,] - country_date[i-1,]) == 1){
      count = count + 1
    }
    else {
      arr_count<-append(arr_count, count)
      count = 1
    }
  }
  arr_count<-append(arr_count, count)
  return (max(arr_count))
}

#Prepare data for 5 and 6
no_report_countries <- subset(countries_covid_data, is.na(new_cases) | is.na(new_deaths))

#5/
df_min<-Countries
df_min$min_days<-0
for (k in 1:3){
  country<-as.character(Countries[k,1])
  df_min$min_days[k] = min_days_no_report(no_report_countries, country)
}
print(df_min)

#6
df_max<-Countries
df_max$max_days<-0
for (k in 1:3){
  country<-as.character(Countries[k,1])
  df_max$max_days[k] = max_days_no_report(no_report_countries, country)
}
print(df_max)

#Prepare data for 7 and 8
no_new_cases <- subset(countries_covid_data, new_cases == 0)

#7
df_min_nocase<-Countries
df_min_nocase$min_days<-0
for (k in 1:3){
  country<-as.character(Countries[k,1])
  df_min_nocase$min_days[k] = min_days_no_report(no_new_cases, country)
}
print(df_min_nocase)

#8
df_max_nocase<-Countries
df_max_nocase$max_days<-0
for (k in 1:3){
  country<-as.character(Countries[k,1])
  df_max_nocase$max_days[k] = max_days_no_report(no_new_cases, country)
}
print(df_max_nocase)