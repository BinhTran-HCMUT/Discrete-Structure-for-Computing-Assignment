#import owid_covid_data.csv & change the "date" column from 'character' format (mm/dd/yy) to 'date' format (yyyy-mm-dd) 
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)

#read the csv file
setwd("E:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))

###############################################################################################
#viii

#######################

#Select the data from every country (location = World)
world_covid_data <- subset(owid_covid_data, location == "World")
#Create day, month and year columns
world_covid_data$day <- as.numeric(format(world_covid_data$date, format = "%d")) #Set numeric data to avoid error
world_covid_data$month <- format(world_covid_data$date, format = "%m")
world_covid_data$year <- format(world_covid_data$date, format = "%Y")

#1
#Calculate 7-recent-day average of total new cases
world_covid_data$mean_cases_7_days <- world_covid_data$new_cases
sum <- world_covid_data$new_cases[1]
x <- 2

for(i in 2:nrow(world_covid_data)){
    if(x<=7){
        sum <- sum + world_covid_data$new_cases[i]
        world_covid_data$mean_cases_7_days[i] <- ceiling(sum/x)
        x <- x + 1
    }
    else{
        sum <- sum - world_covid_data$new_cases[i - 7] + world_covid_data$new_cases[i]
        world_covid_data$mean_cases_7_days[i] <- ceiling(sum/7)
    }
}


#Select row with months in group {1, 3, 6, 7} 
MADE_covid_data <- subset(world_covid_data, month == "01" | month == "03" | month == "07" | month == "06")

#Plot the data
ggplot(MADE_covid_data,(aes(x = day, y = mean_cases_7_days))) +
      geom_line(aes(color = month)) +
      facet_wrap( ~ year) +
      scale_color_manual(labels = c("Thang 1", "Thang 3", "Thang 7", "Thang 6"), values = c("blue", "red", "green", "orange")) +
      labs(title = "Bieu do so ca nhiem benh theo thoi gian la thang",
           subtitle = "Theo trung binh 7 ngay gan nhat",
           y = "So ca nhiem benh",
           x = "Ngay",
           color = "Chu thich")



#######################
#2
#Calculate 7-recent-day  average of new deaths
world_covid_data$mean_deaths_7_days <- world_covid_data$new_deaths
sum <- world_covid_data$new_deaths[1]
x <- 2

for(i in 2:nrow(world_covid_data)){
    if(x<=7){
        sum <- sum + world_covid_data$new_deaths[i]
        world_covid_data$mean_deaths_7_days[i] <- ceiling(sum/x)
        x <- x + 1
    }
    else{
        sum <- sum - world_covid_data$new_deaths[i - 7] + world_covid_data$new_deaths[i]
        world_covid_data$mean_deaths_7_days[i] <- ceiling(sum/7)
    }
}

#Remove any row with months other than 2,4,7,10
MADE_covid_data <- subset(world_covid_data, month == "01" | month == "03" | month == "07" | month == "06")

#Plot the data
ggplot(MADE_covid_data,(aes(x = day, y = mean_deaths_7_days))) +
      geom_line(aes(color = month)) +
      facet_wrap( ~ year) +
      scale_color_manual(labels = c("Thang 1", "Thang 3", "Thang 7", "Thang 6"), values = c("blue", "red", "green", "orange")) +
      labs(title = "Bieu do so ca tu vong theo thoi gian la thang",
           subtitle = "Theo trung binh 7 ngay gan nhat",
           y = "So ca tu vong",
           x = "Ngay",
           color = "Chu thich")


#######################
#Prepare data for 3, 4
world_2months_covid_data <- subset(world_covid_data, month == "11" | month == "12")
world_2months_covid_data$mean_cases_7_days <- world_2months_covid_data$new_cases
world_2months_covid_data$mean_deaths_7_days <- world_2months_covid_data$new_deaths

#######################
#3

#calculate 7-day rolling average of new cases
sum <- world_2months_covid_data$new_cases[1]
x <- 2
for(i in 2:nrow(world_2months_covid_data)){
  if(world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11)
  {
    x <- 1
    sum <- 0
  }
  if(x<=7){
    sum <- sum + world_2months_covid_data$new_cases[i]
    world_2months_covid_data$mean_cases_7_days[i] <- ceiling(sum/x)
    x <- x + 1
  }
  else{
    sum <- sum - world_2months_covid_data$new_cases[i - 7] + world_2months_covid_data$new_cases[i]
    world_2months_covid_data$mean_cases_7_days[i] <- ceiling(sum/7)
  }
}


#Plot the data
ggplot(world_2months_covid_data,(aes(x = date, y = mean_cases_7_days))) +
  geom_line() +
  facet_wrap( ~ year, scales = "free") +
  labs(title = "Bieu do so ca nhiem benh theo thoi gian la 2 thang cuoi nam",
       subtitle = "Theo trung binh 7 ngay gan nhat",
       y = "So ca nhiem benh",
       x = "Ngay",
       color = "Chu thich")

#4
#calculate 7-day rolling average of new cases
sum <- world_2months_covid_data$new_deaths[1]
x <- 2
for(i in 2:nrow(world_2months_covid_data)){
  if(world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11)
  {
    x <- 1
    sum <- 0
  }
  if(x<=7){
    sum <- sum + world_2months_covid_data$new_deaths[i]
    world_2months_covid_data$mean_deaths_7_days[i] <- ceiling(sum/x)
    x <- x + 1
  }
  else{
    sum <- sum - world_2months_covid_data$new_deaths[i - 7] + world_2months_covid_data$new_deaths[i]
    world_2months_covid_data$mean_deaths_7_days[i] <- ceiling(sum/7)
  }
}


#Plot the data
ggplot(world_2months_covid_data,(aes(x = date, y = mean_deaths_7_days))) +
  geom_line() +
  facet_wrap( ~ year, scales = "free") +
  labs(title = "Bieu do so ca tu vong theo thoi gian la 2 thang cuoi nam",
       subtitle = "Theo trung binh 7 ngay gan nhat",
       y = "So ca nhiem benh",
       x = "Ngay",
       color = "Chu thich")

#######################
#Prepare data for 5, 6
world_2months_covid_data <- subset(world_covid_data, month == "11" | month == "12")
world_2months_covid_data$mean_accumulated_cases <- world_2months_covid_data$new_cases
world_2months_covid_data$mean_accumulated_deaths <- world_2months_covid_data$new_deaths
#5
#Calculate the accumulated cases
world_2months_covid_data$mean_accumulated_cases[1] <- world_2months_covid_data$new_cases[1]
for(i in 2:nrow(world_2months_covid_data)){
  if (world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11) 
    world_2months_covid_data$mean_accumulated_cases[i] = world_2months_covid_data$new_cases[i]
  else
    world_2months_covid_data$mean_accumulated_cases[i] <- world_2months_covid_data$mean_accumulated_cases[i - 1] + world_2months_covid_data$new_cases[i]
}

#calculate 7-day rolling average of new cases
sum <- world_2months_covid_data$mean_accumulated_cases[1]
x <- 2

for(i in 2:nrow(world_2months_covid_data)){
  if(world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11)
  {
    x <- 1
    sum <- 0
  }
  if(x<=7){
    sum <- sum + world_2months_covid_data$mean_accumulated_cases[i]
    world_2months_covid_data$mean_accumulated_cases[i] <- ceiling(sum/x)
    x <- x + 1
  }
  else{
    sum <- sum - world_2months_covid_data$mean_accumulated_cases[i - 7] + world_2months_covid_data$mean_accumulated_cases[i]
    world_2months_covid_data$mean_accumulated_cases[i] <- ceiling(sum/7)
  }
}

#Plot the data
ggplot(world_2months_covid_data,(aes(x = date, y = mean_accumulated_cases))) +
  geom_line() +
  facet_wrap( ~ year, scales = "free") +
  labs(title = "Bieu do so ca nhiem benh tich luy theo thoi gian la 2 thang cuoi nam",
       subtitle = "Theo trung binh 7 ngay gan nhat",
       y = "So ca nhiem benh",
       x = "Ngay",
       color = "Chu thich")


#6
#Calculate the accumulated deaths
world_2months_covid_data$mean_accumulated_deaths[1] <- world_2months_covid_data$new_deaths[1]
for(i in 2:nrow(world_2months_covid_data)){
  if (world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11) 
    world_2months_covid_data$mean_accumulated_deaths[i] = world_2months_covid_data$new_deaths[i]
  else
    world_2months_covid_data$mean_accumulated_deaths[i] <- world_2months_covid_data$mean_accumulated_deaths[i - 1] + world_2months_covid_data$new_deaths[i]
}

#calculate 7-day rolling average of new cases
sum <- world_2months_covid_data$mean_accumulated_deaths[1]
x <- 2

for(i in 2:nrow(world_2months_covid_data)){
  if(world_2months_covid_data$day[i] == 1 & world_2months_covid_data$month[i] == 11)
  {
    x <- 1
    sum <- 0
  }
  if(x<=7){
    sum <- sum + world_2months_covid_data$mean_accumulated_deaths[i]
    world_2months_covid_data$mean_accumulated_deaths[i] <- ceiling(sum/x)
    x <- x + 1
  }
  else{
    sum <- sum - world_2months_covid_data$mean_accumulated_deaths[i - 7] + world_2months_covid_data$mean_accumulated_deaths[i]
    world_2months_covid_data$mean_accumulated_deaths[i] <- ceiling(sum/7)
  }
}

#Plot the data
ggplot(world_2months_covid_data,(aes(x = date, y = mean_accumulated_deaths))) +
  geom_line() +
  facet_wrap( ~ year, scales = "free") +
  labs(title = "Bieu do so ca tu vong tich luy theo thoi gian la 2 thang cuoi nam",
       subtitle = "Theo trung binh 7 ngay gan nhat",
       y = "So ca nhiem benh",
       x = "Ngay",
       color = "Chu thich")






