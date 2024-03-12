#Library
library(readr)
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(scales)

#Read file
setwd("D:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", col_types = cols(date = col_date(format ="%m/%d/%Y")))
#Change negative number
owid_covid_data$new_cases <- abs(owid_covid_data$new_cases) 

countries_covid_group_data <- subset(owid_covid_data, location == "Brazil" | location == "Chile" | location == "Venezuela") %>% group_by(location)
countries_covid_group_data$new_cases[is.na(countries_covid_group_data$new_cases)] <- 0
countries_covid_group_data$new_deaths[is.na(countries_covid_group_data$new_deaths)] <-0
#1
countries_group_7day_data <- subset(countries_covid_group_data, 
                                    date == "2022-02-13" |
                                      date == "2022-02-14" | 
                                      date == "2022-02-15" | 
                                      date == "2022-02-16" | 
                                      date == "2022-02-17" |
                                      date == "2022-02-18" |
                                      date == "2022-02-19")
countries_group_7day_data %>% 
ggplot(aes(date, new_cases, color=location))+
        geom_line()+
        scale_x_date(labels = date_format("%d/%m/%y")) +
        labs(title = "Overview of new cases by countries") +
        theme_bw()

brazil_data <- subset(countries_group_7day_data, subset = (location=="Brazil")) 
brazil_data %>%
  ggplot(aes(date, new_cases, color=location))+
  geom_line()+
  scale_x_date(labels = date_format("%d/%m/%y")) +
  labs(title = "Overview of new cases of Brazil") +
  theme_bw()

chile_data <- subset(countries_group_7day_data, subset = (location=="Chile")) 
chile_data %>%
  ggplot(aes(date, new_cases, color=location))+
  geom_line()+
  scale_x_date(labels = date_format("%d/%m/%y")) +
  labs(title = "Overview of new cases of Chile") +
  theme_bw()

venezuela_data <- subset(countries_group_7day_data, subset = (location=="Venezuela")) 
venezuela_data %>%
  ggplot(aes(date, new_cases, color=location))+
  geom_line()+
  scale_x_date(labels = date_format("%d/%m/%y")) +
  labs(title = "Overview of new cases of Venezuela") +
  theme_bw()

#3

countries_covid_group <- unlist(countries_covid_group_data$date)
countries_covid_group <- unname(countries_covid_group)
col_date <- countries_covid_group
countries_covid_group_data$year <- substr(col_date,0,4)
countries_covid_group_data$month <- substr(col_date,6,7)

group_data <- countries_covid_group_data %>% group_by(location, month, year) %>% summarise(Infections = sum(new_cases), Deaths = sum(new_deaths))
group_data[order(group_data$year),]

brazil <- subset(group_data, subset = (location=="Brazil"))
brazil <- brazil[order(brazil$year),]
brazil$time <- c(as.integer(brazil$month[1]):(length(brazil$location)+as.integer(brazil$month[1])-1))
sum <- sum(brazil$Infections)
brazil$Infection<-brazil$Infections/sum
temp <- 0
for(i in 1:length(brazil$location)) {
  temp <- temp+brazil[i, "Infection"]
  brazil[i, "Infection"]<-temp
}
brazil[1, "difference_infection"]<-0
for(i in 2:length(brazil$location)) {
  brazil[i, "difference_infection"]<-brazil[i, "Infection"]-brazil[i-1, "Infection"]
}

diff_value<-quantile(brazil$difference_infection,0.75)
brazil_increase_infection<-brazil[brazil$difference_infection>=diff_value,]
brazil_increase_infection <- data.frame(location = brazil_increase_infection$location, month = brazil_increase_infection$month, year = brazil_increase_infection$year)

diff_value<-quantile(brazil$difference_infection,0.25)
brazil_decrease_infection<-brazil[brazil$difference_infection<=diff_value & brazil$difference_infection>0,]
brazil_decrease_infection <- data.frame(location = brazil_decrease_infection$location, month = brazil_decrease_infection$month, year = brazil_decrease_infection$year)

sum <- sum(brazil$Deaths)
brazil$Death<-brazil$Deaths/sum
temp <- 0
for(i in 1:length(brazil$location)) {
  temp <- temp+brazil[i, "Death"]
  brazil[i, "Death"] <- temp
}
brazil[1, "difference_death"]<-0
for(i in 2:length(brazil$location)) {
  brazil[i, "difference_death"]<-brazil[i, "Death"]-brazil[i-1, "Death"]
}

diff_value<-quantile(brazil$difference_death,0.75)
brazil_increase_death<-brazil[brazil$difference_death>=diff_value,]
brazil_increase_death <- data.frame(location = brazil_increase_death$location, month = brazil_increase_death$month,year = brazil_increase_death$year)

diff_value<-quantile(brazil$difference_death,0.25)
brazil_decrease_death<-brazil[brazil$difference_death<=diff_value & brazil$difference_death>0,]
brazil_decrease_death <- data.frame(location = brazil_decrease_death$location, month = brazil_decrease_death$month,year = brazil_decrease_death$year)


ggplot()+
  geom_line(data=brazil, mapping=aes(x=time, y=Infection), color="green")+
  geom_line(data=brazil, mapping=aes(x=time, y=Death), color="red")+
  labs(title = "Infections and Deaths of Brazil")+
  xlab("Months from 2020") +ylab("Frequency")+
  scale_y_continuous(
    labels = scales::percent)+
  scale_x_continuous(
  breaks = seq(
    from = 0,
    to = 30,
    by = 5))+
  theme_bw()



chile <- subset(group_data, subset = (location=="Chile"))
chile <- chile[order(chile$year),]
chile$time <- c(as.integer(chile$month[1]):(length(chile$location)+as.integer(chile$month[1])-1))
sum <- sum(chile$Infections)
chile$Infection<-chile$Infections/sum
temp <- 0
for(i in 1:length(chile$location)) {
  temp <- temp+chile[i, "Infection"]
  chile[i, "Infection"]<-temp
}
chile[1, "difference_infection"]<-0
for(i in 2:length(chile$location)) {
  chile[i, "difference_infection"]<-chile[i, "Infection"]-chile[i-1, "Infection"]
}

diff_value<-quantile(chile$difference_infection,0.75)
chile_increase_infection<-chile[chile$difference_infection>=diff_value,]
chile_increase_infection <- data.frame(location = chile_increase_infection$location, month = chile_increase_infection$month,year = chile_increase_infection$year)

diff_value<-quantile(chile$difference_infection,0.25)
chile_decrease_infection<-chile[chile$difference_infection<=diff_value & chile$difference_infection>0,]
chile_decrease_infection <- data.frame(location = chile_decrease_infection$location, month = chile_decrease_infection$month,year = chile_decrease_infection$year)

sum <- sum(chile$Deaths)
chile$Death<-chile$Deaths/sum
temp <- 0
for(i in 1:length(chile$location)) {
  temp <- temp+chile[i, "Death"]
  chile[i, "Death"] <- temp
}
chile[1, "difference_death"]<-0
for(i in 2:length(chile$location)) {
  chile[i, "difference_death"]<-chile[i, "Death"]-chile[i-1, "Death"]
}

diff_value<-quantile(chile$difference_death,0.75)
chile_increase_death<-chile[chile$difference_death>=diff_value,]
chile_increase_death <- data.frame(location = chile_increase_death$location, month = chile_increase_death$month,year = chile_increase_death$year)

diff_value<-quantile(chile$difference_death,0.25)
chile_decrease_death<-chile[chile$difference_death<=diff_value & chile$difference_death>0,]
chile_decrease_death <- data.frame(location = chile_decrease_death$location, month = chile_decrease_death$month,year = chile_decrease_death$year)

ggplot()+
  geom_line(data=chile, mapping=aes(x=time, y=Infection), color="green")+
  geom_line(data=chile, mapping=aes(x=time, y=Death), color="red")+
  labs(title = "Infections and Deaths of Chile")+
  xlab("Months from 2020") +ylab("Frequency")+
  scale_y_continuous(
    labels = scales::percent)+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()


venezuela <- subset(group_data, subset = (location=="Venezuela"))
venezuela <- venezuela[order(venezuela$year),]
venezuela$time <- c(as.integer(venezuela$month[1]):(length(venezuela$location)+as.integer(venezuela$month[1])-1))
sum <- sum(venezuela$Infections)
venezuela$Infection<-venezuela$Infections/sum
temp <- 0
for(i in 1:length(venezuela$location)) {
  temp <- temp+venezuela[i, "Infection"]
  venezuela[i, "Infection"]<-temp
}

venezuela[1, "difference_infection"]<-0
for(i in 2:length(venezuela$location)) {
  venezuela[i, "difference_infection"]<-venezuela[i, "Infection"]-venezuela[i-1, "Infection"]
}

diff_value<-quantile(venezuela$difference_infection,0.75)
venezuela_increase_infection<-venezuela[venezuela$difference_infection>=diff_value,]
venezuela_increase_infection <- data.frame(location = venezuela_increase_infection$location, month = venezuela_increase_infection$month,year = venezuela_increase_infection$year)

diff_value<-quantile(venezuela$difference_infection,0.25)
venezuela_decrease_infection<-venezuela[venezuela$difference_infection<=diff_value & venezuela$difference_infection>0,]
venezuela_decrease_infection <- data.frame(location = venezuela_decrease_infection$location, month = venezuela_decrease_infection$month,year = venezuela_decrease_infection$year)


sum <- sum(venezuela$Deaths)
venezuela$Death<-venezuela$Deaths/sum
temp <- 0
for(i in 1:length(venezuela$location)) {
  temp <- temp+venezuela[i, "Death"]
  venezuela[i, "Death"] <- temp
}

venezuela[1, "difference_death"]<-0
for(i in 2:length(venezuela$location)) {
  venezuela[i, "difference_death"]<-venezuela[i, "Death"]-venezuela[i-1, "Death"]
}

diff_value<-quantile(venezuela$difference_death,0.75)
venezuela_increase_death<-venezuela[venezuela$difference_death>=diff_value,]
venezuela_increase_death <- data.frame(location = venezuela_increase_death$location, month = venezuela_increase_death$month,year = venezuela_increase_death$year)

diff_value<-quantile(venezuela$difference_death,0.25)
venezuela_decrease_death<-venezuela[venezuela$difference_death<=diff_value & venezuela$difference_death>0,]
venezuela_decrease_death <- data.frame(location = venezuela_decrease_death$location, month = venezuela_decrease_death$month,year = venezuela_decrease_death$year)


ggplot()+
  geom_line(data=venezuela, mapping=aes(x=time, y=Infection), color="green")+
  geom_line(data=venezuela, mapping=aes(x=time, y=Death), color="red")+
  labs(title = "Infections and Deaths of Venezuela")+
  xlab("Months from 2020") +ylab("Frequency")+
  scale_y_continuous(
    labels = scales::percent)+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

#6

ggplot()+
  geom_line(data=brazil, mapping=aes(x=time, y=Infections), color="green")+
  geom_line(data=chile, mapping=aes(x=time, y=Infections), color="blue")+
  geom_line(data=venezuela, mapping=aes(x=time, y=Infections), color="red")+
  labs(title = "Infections of countries")+
  xlab("Months from 2020") +ylab("Infections")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(brazil$Infections,0.75)
value<-c(k)
brazil$value<-value
brazil_infections<-brazil[brazil$Infections>=k,]
brazil_infections <- data.frame(location = brazil_infections$location, month = brazil_infections$month,year = brazil_infections$year)

ggplot()+
  geom_line(data=brazil, mapping=aes(x=time, y=Infections), color="green")+
  geom_line(data=brazil, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Infections of Brazil")+
  xlab("Months from 2020") +ylab("Infections")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(chile$Infections,0.75)
value<-c(k)
chile$value<-value
chile_infections<-chile[chile$Infections>=k,]
chile_infections <- data.frame(location = chile_infections$location, month = chile_infections$month,year = chile_infections$year)

ggplot()+
  geom_line(data=chile, mapping=aes(x=time, y=Infections), color="blue")+
  geom_line(data=chile, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Infections of Chile")+
  xlab("Months from 2020") +ylab("Infections")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(venezuela$Infections,0.75)
value<-c(k)
venezuela$value<-value
venezuela_infections<-venezuela[venezuela$Infections>=k,]
venezuela_infections <- data.frame(location = venezuela_infections$location, month = venezuela_infections$month,year = venezuela_infections$year)

ggplot()+
  geom_line(data=venezuela, mapping=aes(x=time, y=Infections), color="red")+
  geom_line(data=venezuela, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Infections of Venezuela")+
  xlab("Months from 2020") +ylab("Infections")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

#7

ggplot()+
  geom_line(data=brazil, mapping=aes(x=time, y=Deaths), color="green")+
  geom_line(data=chile, mapping=aes(x=time, y=Deaths), color="blue")+
  geom_line(data=venezuela, mapping=aes(x=time, y=Deaths), color="red")+
  labs(title = "Deaths of countries")+
  xlab("Months from 2020") +ylab("Deaths")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(brazil$Deaths,0.75)
value<-c(k)
brazil$value<-value
brazil_deaths<-brazil[brazil$Deaths>=k,]
brazil_deaths <- data.frame(location = brazil_deaths$location, month = brazil_deaths$month,year = brazil_deaths$year)

ggplot()+
  geom_line(data=brazil, mapping=aes(x=time, y=Deaths), color="green")+
  geom_line(data=brazil, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Deaths of Brazil")+
  xlab("Months from 2020") +ylab("Deaths")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(chile$Deaths,0.75)
value<-c(k)
chile$value<-value
chile_deaths<-chile[chile$Deaths>=k,]
chile_deaths <- data.frame(location = chile_deaths$location, month = chile_deaths$month,year = chile_deaths$year)

ggplot()+
  geom_line(data=chile, mapping=aes(x=time, y=Deaths), color="blue")+
  geom_line(data=chile, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Deaths of Chile")+
  xlab("Months from 2020") +ylab("Deaths")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

k<-quantile(venezuela$Deaths,0.75)
value<-c(k)
venezuela$value<-value
venezuela_deaths<-venezuela[venezuela$Deaths>=k,]
venezuela_deaths <- data.frame(location = venezuela_deaths$location, month = venezuela_deaths$month,year = venezuela_deaths$year)

ggplot()+
  geom_line(data=venezuela, mapping=aes(x=time, y=Deaths), color="red")+
  geom_line(data=venezuela, mapping=aes(x=time, y=value), color="orange", linetype=20)+
  labs(title = "Deaths of Venezuela")+
  xlab("Months from 2020") +ylab("Deaths")+
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 30,
      by = 5))+
  theme_bw()

