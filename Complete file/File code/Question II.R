
#Library
library(readr)
library(tidyverse)
library(dbplyr)
library(ggplot2)

#Read file
setwd("D:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", col_types = cols(date = col_date(format ="%m/%d/%Y")))
#Change negative number
owid_covid_data$new_cases <- abs(owid_covid_data$new_cases)
owid_covid_data$new_deaths <- abs(owid_covid_data$new_deaths)

countries_covid_group_data <- subset(owid_covid_data, location == "Brazil" | location == "Chile" | location == "Venezuela") %>% group_by(location)
countries_covid_group_data$new_cases[is.na(countries_covid_group_data$new_cases)] <- 0
countries_covid_group_data$new_deaths[is.na(countries_covid_group_data$new_deaths)] <- 0


Brazil_data <- subset(countries_covid_group_data, location == "Brazil") 
Status <- c("Min", "Q1", "Q2", "Q3", "Max", "Avg", "Std", "Outlier")
Min=min(Brazil_data$new_cases) 
Q1=quantile(Brazil_data$new_cases,0.25) 
Q2=quantile(Brazil_data$new_cases,0.5) 
Q3=quantile(Brazil_data$new_cases,0.75) 
Max=max(Brazil_data$new_cases) 
Avg=format(round(mean(Brazil_data$new_cases),2),2) 
Std=format(round(sd(Brazil_data$new_cases),2),2)
Outlier=sum(Brazil_data$new_cases<Q1-1.5*(Q3-Q1))+sum(Brazil_data$new_cases>Q3+1.5*(Q3-Q1))
New_cases<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

Min=min(Brazil_data$new_deaths) 
Q1=quantile(Brazil_data$new_deaths,0.25) 
Q2=quantile(Brazil_data$new_deaths,0.5) 
Q3=quantile(Brazil_data$new_deaths,0.75) 
Max=max(Brazil_data$new_deaths) 
Avg=format(round(mean(Brazil_data$new_deaths),2),2) 
Std=format(round(sd(Brazil_data$new_deaths),2),2)
Outlier=sum(Brazil_data$new_deaths<Q1-1.5*(Q3-Q1))+sum(Brazil_data$new_deaths>Q3+1.5*(Q3-Q1))
New_deaths<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

output_Brazil <- data.frame(Status, New_cases, New_deaths)

Chile_data <- subset(countries_covid_group_data, location == "Chile") 
Status <- c("Min", "Q1", "Q2", "Q3", "Max", "Avg", "Std", "Outlier")
Min=min(Chile_data$new_cases) 
Q1=quantile(Chile_data$new_cases,0.25) 
Q2=quantile(Chile_data$new_cases,0.5) 
Q3=quantile(Chile_data$new_cases,0.75) 
Max=max(Chile_data$new_cases) 
Avg=format(round(mean(Chile_data$new_cases),2),2) 
Std=format(round(sd(Chile_data$new_cases),2),2)
Outlier=sum(Chile_data$new_cases<Q1-1.5*(Q3-Q1))+sum(Chile_data$new_cases>Q3+1.5*(Q3-Q1))
New_cases<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

Min=min(Chile_data$new_deaths) 
Q1=quantile(Chile_data$new_deaths,0.25) 
Q2=quantile(Chile_data$new_deaths,0.5) 
Q3=quantile(Chile_data$new_deaths,0.75) 
Max=max(Chile_data$new_deaths) 
Avg=format(round(mean(Chile_data$new_deaths),2),2) 
Std=format(round(sd(Chile_data$new_deaths),2),2)
Outlier=sum(Chile_data$new_deaths<Q1-1.5*(Q3-Q1))+sum(Chile_data$new_deaths>Q3+1.5*(Q3-Q1))
New_deaths<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

output_Chile <- data.frame(Status, New_cases, New_deaths)

Venezuela_data <- subset(countries_covid_group_data, location == "Venezuela") 
Status <- c("Min", "Q1", "Q2", "Q3", "Max", "Avg", "Std", "Outlier")
Min=min(Venezuela_data$new_cases) 
Q1=quantile(Venezuela_data$new_cases,0.25) 
Q2=quantile(Venezuela_data$new_cases,0.5) 
Q3=quantile(Venezuela_data$new_cases,0.75) 
Max=max(Venezuela_data$new_cases) 
Avg=format(round(mean(Venezuela_data$new_cases),2),2) 
Std=format(round(sd(Venezuela_data$new_cases),2),2)
Outlier=sum(Venezuela_data$new_cases<Q1-1.5*(Q3-Q1))+sum(Venezuela_data$new_cases>Q3+1.5*(Q3-Q1))
New_cases<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

Min=min(Venezuela_data$new_deaths) 
Q1=quantile(Venezuela_data$new_deaths,0.25) 
Q2=quantile(Venezuela_data$new_deaths,0.5) 
Q3=quantile(Venezuela_data$new_deaths,0.75) 
Max=max(Venezuela_data$new_deaths) 
Avg=format(round(mean(Venezuela_data$new_deaths),2),2) 
Std=format(round(sd(Venezuela_data$new_deaths),2),2)
Outlier=sum(Venezuela_data$new_deaths<Q1-1.5*(Q3-Q1))+sum(Venezuela_data$new_deaths>Q3+1.5*(Q3-Q1))
New_deaths<-c(Min, Q1, Q2, Q3, Max, Avg, Std, Outlier)

output_Venezuela <- data.frame(Status, New_cases, New_deaths)

country_new_cases <- as.data.frame(countries_covid_group_data) 
ggplot(country_new_cases, aes(location , new_cases, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new cases by countries", 
       x = "Country",
       y = "New cases",
       fill = "Country") +
  theme_bw() +  
  theme(panel.grid.major.x = element_blank(),)

brazil_data <- subset(countries_covid_group_data, location == "Brazil")
brazil <- as.data.frame(brazil_data) 
ggplot(brazil, aes(location ,new_cases, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new cases by countries",
       x = "Brazil",
       y = "New cases") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

chile_data <- subset(countries_covid_group_data, location == "Chile")
chile <- as.data.frame(chile_data) 
ggplot(chile, aes(location , new_cases, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new cases by countries", 
       x = "Chile",
       y = "New cases") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

venezuela_data <- subset(countries_covid_group_data, location == "Venezuela")
venezuela <- as.data.frame(venezuela_data) 
ggplot(venezuela, aes(location , new_cases, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new cases by countries", 
       x = "Venezuela",
       y = "New cases") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")


country <- as.data.frame(countries_covid_group_data) 
ggplot(country, aes(location , new_deaths, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new deaths by countries", 
       x = "Country",
       y = "New deaths",
       fill = "Country") +
  theme_bw() +  
  theme(panel.grid.major.x = element_blank(),)

brazil_data <- subset(countries_covid_group_data, location == "Brazil")
brazil <- as.data.frame(brazil_data) 
ggplot(brazil, aes(location ,new_deaths, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new deaths by countries",
       x = "Brazil",
       y = "New deaths") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

chile_data <- subset(countries_covid_group_data, location == "Chile")
chile <- as.data.frame(chile_data) 
ggplot(chile, aes(location , new_deaths, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new deaths by countries", 
       x = "Chile",
       y = "New deaths") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

venezuela_data <- subset(countries_covid_group_data, location == "Venezuela")
venezuela <- as.data.frame(venezuela_data) 
ggplot(venezuela, aes(location , new_deaths, fill=location ))+
  geom_boxplot()+
  labs(title = "Overview of new deaths by countries", 
       x = "Venezuela",
       y = "New deaths") +
  theme_bw() +  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

                      