#Library
library(readr)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(Rmpfr)

#Read file
setwd("D:/CTRR")
owid_covid_data <- read_csv("owid-covid-data.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
owid_covid_data$new_cases <- abs(owid_covid_data$new_cases) #Change negative number
owid_covid_data$new_cases[is.na(owid_covid_data$new_cases)] <- 0
owid_covid_data$new_deaths[is.na(owid_covid_data$new_deaths)] <- 0

#vii
#Seperate data for 1,2
vii<-subset(owid_covid_data, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
vii<-summarise(vii, date, new_cases, new_deaths)
vii<-aggregate(cbind(vii$new_cases, vii$new_deaths), by= list (Date = vii$date), FUN=sum)
names(vii)<-c("date", "new_cases", "new_deaths")

vii_2020<-subset(vii, year(date) == 2020)
vii_2021<-subset(vii, year(date) == 2021)
vii_2022<-subset(vii, year(date) == 2022)

#1
#2020
plot(vii_2020$new_cases[1:31], type = "l", col = "red", ylim = c(0, max(vii_2020$new_cases)), xlab = "Day", ylab = "New cases", main = "New cases in the wolrd in 2020")
lines(vii_2020$new_cases[32:62], col = "yellow", type = "l")
lines(vii_2020$new_cases[63:92], col = "green", type = "l")
lines(vii_2020$new_cases[93:123], col = "blue", type = "l")
legend("topleft", legend = c("January", "March", "June", "July"), lty = 1, col = c("red","yellow", "green", "blue"), ncol = 1, bty = "o", text.col = c("red","yellow", "green", "blue"))
#2021
plot(vii_2021$new_cases[1:31], type = "l", col = "red", ylim = c(0, max(vii_2021$new_cases)), xlab = "Day", ylab = "New cases", main = "New cases in the wolrd in 2021")
lines(vii_2021$new_cases[32:62], col = "yellow", type = "l")
lines(vii_2021$new_cases[63:92], col = "green", type = "l")
lines(vii_2021$new_cases[93:123], col = "blue", type = "l")
legend("bottomright", legend = c("January", "March", "June", "July"), lty = 1, col = c("red","yellow", "green", "blue"), ncol = 1, bty = "o", text.col = c("red","yellow", "green", "blue"))
#2022
plot(vii_2022$new_cases, type = "l", col = "red", ylim = c(0, max(vii_2022$new_cases)), xlab = "Day", ylab = "New cases", main = "New cases in the wolrd in 2022")
legend("topleft", legend = c("January"), lty = 1, col = c("red"), ncol = 1, bty = "o", text.col = c("red"))

#2
#2020
plot(vii_2020$new_deaths[1:31], type = "l", col = "red", ylim = c(0, max(vii_2020$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New deaths in the wolrd in 2020")
lines(vii_2020$new_deaths[32:62], col = "yellow", type = "l")
lines(vii_2020$new_deaths[63:92], col = "green", type = "l")
lines(vii_2020$new_deaths[93:123], col = "blue", type = "l")
legend("topleft", legend = c("January", "March", "June", "July"), lty = 1, col = c("red","yellow", "green", "blue"), ncol = 1, bty = "o", text.col = c("red","yellow", "green", "blue"))
#2021
plot(vii_2021$new_deaths[1:31], type = "l", col = "red", ylim = c(0, max(vii_2021$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New deaths in the wolrd in 2021")
lines(vii_2021$new_deaths[32:62], col = "yellow", type = "l")
lines(vii_2021$new_deaths[63:92], col = "green", type = "l")
lines(vii_2021$new_deaths[93:123], col = "blue", type = "l")
legend("bottomright", legend = c("January", "March", "June", "July"), lty = 1, col = c("red","yellow", "green", "blue"), ncol = 1, bty = "o", text.col = c("red","yellow", "green", "blue"))
#2022
plot(vii_2022$new_deaths, type = "l", col = "red", ylim = c(0, max(vii_2022$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New deaths in the wolrd in 2022")
legend("topleft", legend = c("January"), lty = 1, col = c("red"), ncol = 1, bty = "o", text.col = c("red"))

#Seperate data for 3,4
vii<-subset(owid_covid_data, month(date) == 11 | month(date) == 12)
vii<-summarise(vii, date, new_cases, new_deaths)
vii<-aggregate(cbind(vii$new_cases, vii$new_deaths), by= list (Date = vii$date), FUN=sum)
names(vii)<-c("date", "new_cases", "new_deaths")

vii_2020<-subset(vii, year(date) == 2020)
vii_2021<-subset(vii, year(date) == 2021)

#3
#2020
plot(vii_2020$new_cases[1:30], type = "l", col = "red", ylim = c(0, max(vii_2020$new_cases)), xlab = "Day", ylab = "New cases", main = "New cases in the wolrd in 2020")
lines(vii_2020$new_cases[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))
#2021
plot(vii_2021$new_cases[1:30], type = "l", col = "red", ylim = c(0, max(vii_2021$new_cases)), xlab = "Day", ylab = "New cases", main = "New cases in the wolrd in 2021")
lines(vii_2021$new_cases[31:61], col = "yellow", type = "l")
legend("topleft", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))

#4
#2020
plot(vii_2020$new_deaths[1:30], type = "l", col = "red", ylim = c(0, max(vii_2020$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New deaths in the wolrd in 2020")
lines(vii_2020$new_deaths[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))
#2021
plot(vii_2021$new_deaths[1:30], type = "l", col = "red", ylim = c(0, max(vii_2021$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New deaths in the wolrd in 2021")
lines(vii_2021$new_deaths[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))

#Seperate data for 5,6
vii<-summarise(owid_covid_data, date, new_cases, new_deaths)
vii<-aggregate(cbind(vii$new_cases, vii$new_deaths), by= list (Date = vii$date), FUN=sum)
names(vii)<-c("date", "new_cases", "new_deaths")
vii$new_cases<-cumsum(vii$new_cases)
vii$new_deaths<-cumsum(vii$new_deaths)

vii_2020<-subset(vii, year(date) == 2020 & (month(date) == 11 | month(date) == 12))
vii_2021<-subset(vii, year(date) == 2021 & (month(date) == 11 | month(date) == 12))

#5
#2020
plot(vii_2020$new_cases[1:30], type = "l", col = "red", ylim = c(0, max(vii_2020$new_cases)), xlab = "Day", ylab = "New cases", main = "New cumulative cases in the wolrd in 2020")
lines(vii_2020$new_cases[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))
#2021
plot(vii_2021$new_cases[1:30], type = "l", col = "red", ylim = c(0, max(vii_2021$new_cases)), xlab = "Day", ylab = "New cases", main = "New cumulative cases in the wolrd in 2021")
lines(vii_2021$new_cases[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))

#6
#2020
plot(vii_2020$new_deaths[1:30], type = "l", col = "red", ylim = c(0, max(vii_2020$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New cumulative deaths in the wolrd in 2020")
lines(vii_2020$new_deaths[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))
#2021
plot(vii_2021$new_deaths[1:30], type = "l", col = "red", ylim = c(0, max(vii_2021$new_deaths)), xlab = "Day", ylab = "New deaths", main = "New cumulative deaths in the wolrd in 2021")
lines(vii_2021$new_deaths[31:61], col = "yellow", type = "l")
legend("bottomright", legend = c("November", "December"), lty = 1, col = c("red", "yellow"), ncol = 1, bty = "o", text.col = c("red", "yellow"))