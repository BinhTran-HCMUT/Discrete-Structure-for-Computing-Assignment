#Library
library(readr)
library(tidyverse)
library(dbplyr)
library(plyr)
library(lubridate)
library(data.table)

#Read file
setwd("E:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
owid_covid_data$new_cases <- abs(owid_covid_data$new_cases) #Change negative number
owid_covid_data$new_deaths <- abs(owid_covid_data$new_deaths)

#Clean data
countries_covid_data<-subset(owid_covid_data, continent == 'Asia' | continent == 'Europe' | continent == 'Oceania' | continent == 'Africa'| continent == 'North America'| continent == 'South America')

#1.
unique.location<-distinct(countries_covid_data,location,.keep_all=TRUE)
data4<-arrange(distinct(countries_covid_data,location,.keep_all=TRUE),continent)

barplot(cumsum(table(data4$continent)),
        main="Cumulative Frequency",
        xlab="continent",
        ylab="number of location",
        ylim=c(0,300),
        col=topo.colors(10))
#2.
barplot(table(data4$continent)/length(data4$continent),
        main="Relative Frequency",
        xlab="continent",
        ylab="number of location",
        ylim=c(0,0.3),
        col=topo.colors(10))

#3
#Brazil
Brazil<-subset(countries_covid_data,location=="Brazil")
Brazil<-Brazil[(length(Brazil$location)-6):length(Brazil$location),c(4,5)]
barplot(Brazil$new_cases,
        main="new cases in the last 7 days in Brazil",
        names.arg = Brazil$date,
        xlab="date",
        ylab="new cases",
        ylim=c(0,150000),
        col=topo.colors(10))
#Chile
Chile<-subset(countries_covid_data,location=="Chile")
length(Chile$location)
Chile<-Chile[(length(Chile$location)-6):length(Chile$location),c(4,5)]
barplot(Chile$new_cases,
        main="new cases in the last 7 days in Chile",
        names.arg = Chile$date,
        xlab="date",
        ylab="new cases",
        ylim=c(0,40000),
        col=topo.colors(10))
#Venezuela
Venezuela<-subset(countries_covid_data,location=="Venezuela")
length(Venezuela$location)
Venezuela<-Venezuela[(length(Venezuela$location)-6):length(Venezuela$location),c(4,5)]
barplot(Venezuela$new_cases,
        main="new cases in the last 7 days in Venezuela",
        names.arg = Venezuela$date,
        xlab="date",
        ylab="new cases",
        ylim=c(0,2000),
        col=topo.colors(10))

#Clean data
countries_covid_data <- subset(owid_covid_data, location == "Brazil" | location == "Chile" | location == "Venezuela") %>% group_by(location)
countries_covid_data$new_cases[is.na(countries_covid_data$new_cases)] <- 0
countries_covid_data$new_deaths[is.na(countries_covid_data$new_deaths)] <- 0

### Question IV
## Question IV.4
#Brazil
Brazil<-subset(countries_covid_data,location=="Brazil")
Brazil<-Brazil[(length(Brazil$location)-6):length(Brazil$location),c(4,6)]
barplot(Brazil$new_deaths,
        main="new deaths in the last 7 days in Brazil",
        names.arg = Brazil$date,
        xlab="date",
        ylab="new deaths",
        ylim=c(0,1500),
        col=topo.colors(10))
#Chile
Chile<-subset(countries_covid_data,location=="Chile")
Chile<-Chile[(length(Chile$location)-6):length(Chile$location),c(4,6)]
barplot(Chile$new_deaths,
        main="new deaths in the last 7 days in Chile",
        names.arg = Chile$date,
        xlab="date",
        ylab="new deaths",
        ylim=c(0,200),
        col=topo.colors(10))
#Venezuela
Venezuela<-subset(countries_covid_data,location=="Venezuela")
length(Venezuela$location)
Venezuela<-Venezuela[(length(Venezuela$location)-6):length(Venezuela$location),c(4,6)]
barplot(Venezuela$new_deaths,
        main="new deaths in the last 7 days in Venezuela",
        names.arg = Venezuela$date,
        xlab="date",
        ylab="new deaths",
        ylim=c(0,20),
        col=topo.colors(10))

## Question IV.5 
#Brazil
Brazil_data <- subset(countries_covid_data, location == "Brazil") 
Q1=quantile(Brazil_data$new_cases,0.25) 
Q2=quantile(Brazil_data$new_cases,0.5) 
Q3=quantile(Brazil_data$new_cases,0.75) 
Outliers_new.cases_Brazil=sum(Brazil_data$new_cases<Q1-1.5*(Q3-Q1)) + sum(Brazil_data$new_cases>Q3+1.5*(Q3-Q1))

#Chile
Chile_data <- subset(countries_covid_data, location == "Chile") 
Q1=quantile(Chile_data$new_cases,0.25) 
Q2=quantile(Chile_data$new_cases,0.5) 
Q3=quantile(Chile_data$new_cases,0.75) 
Outliers_new.cases_Chile=sum(Chile_data$new_cases<Q1-1.5*(Q3-Q1)) + sum(Chile_data$new_cases>Q3+1.5*(Q3-Q1))

#Venezuela
Venezuela_data <- subset(countries_covid_data, location == "Venezuela") 
Q1=quantile(Venezuela_data$new_cases,0.25) 
Q2=quantile(Venezuela_data$new_cases,0.5) 
Q3=quantile(Venezuela_data$new_cases,0.75) 
Outliers_new.cases_Venezuela=sum(Venezuela_data$new_cases<Q1-1.5*(Q3-Q1)) + sum(Venezuela_data$new_cases>Q3+1.5*(Q3-Q1))

Outliers_new.cases=c(Outliers_new.cases_Brazil, Outliers_new.cases_Chile, Outliers_new.cases_Venezuela)

barplot(Outliers_new.cases,
        main = "Outliers of new cases",
        xlab = "Country",
        ylab = "Outliers",
        names.arg = c('Brazil', 'Chile', 'Venezuela'),
        col=topo.colors(3))

## Question IV.6
#Brazil
Brazil_data <- subset(countries_covid_data, location == "Brazil") 
Q1=quantile(Brazil_data$new_deaths,0.25) 
Q2=quantile(Brazil_data$new_deaths,0.5) 
Q3=quantile(Brazil_data$new_deaths,0.75) 
Outliers_new.deaths_Brazil=sum(Brazil_data$new_deaths<Q1-1.5*(Q3-Q1)) + sum(Brazil_data$new_deaths>Q3+1.5*(Q3-Q1))

#Chile
Chile_data <- subset(countries_covid_data, location == "Chile") 
Q1=quantile(Chile_data$new_deaths,0.25) 
Q2=quantile(Chile_data$new_deaths,0.5) 
Q3=quantile(Chile_data$new_deaths,0.75) 
Outliers_new.deaths_Chile=sum(Chile_data$new_deaths<Q1-1.5*(Q3-Q1)) + sum(Chile_data$new_deaths>Q3+1.5*(Q3-Q1))

#Venezuela
Venezuela_data <- subset(countries_covid_data, location == "Venezuela") 
Q1=quantile(Venezuela_data$new_deaths,0.25) 
Q2=quantile(Venezuela_data$new_deaths,0.5) 
Q3=quantile(Venezuela_data$new_deaths,0.75) 
Outliers_new.deaths_Venezuela=sum(Venezuela_data$new_deaths<Q1-1.5*(Q3-Q1)) + sum(Venezuela_data$new_deaths>Q3+1.5*(Q3-Q1))

Outliers_new.deaths=c(Outliers_new.deaths_Brazil, Outliers_new.deaths_Chile, Outliers_new.deaths_Venezuela)

barplot(Outliers_new.deaths,
        main = "Outliers of new deaths",
        xlab = "Country",
        ylab = "Outliers",
        names.arg = c('Brazil', 'Chile', 'Venezuela'),
        col=topo.colors(3))