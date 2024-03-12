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

#MADE: 1763
#v
#Seperate data for 1,2,3
brazil <- subset(owid_covid_data, location == "Brazil")
brazil <- subset(brazil, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
brazil.2020 <- subset(brazil, year(date) == 2020)
brazil.2021 <- subset(brazil, year(date) == 2021)
brazil.2022 <- subset(brazil, year(date) == 2022)
chile <- subset(owid_covid_data, location == "Chile")
chile <- subset(chile, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
chile.2020 <- subset(chile, year(date) == 2020)
chile.2021 <- subset(chile, year(date) == 2021)
chile.2022 <- subset(chile, year(date) == 2022)
venezuela <- subset(owid_covid_data, location == "Venezuela")
venezuela <- subset(venezuela, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
venezuela.2020 <- subset(venezuela, year(date) == 2020)
venezuela.2021 <- subset(venezuela, year(date) == 2021)
venezuela.2022 <- subset(venezuela, year(date) == 2022)

#1
plot(brazil.2020$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_cases)), xlab = "Month", ylab = "New cases", main = "New cases in 2020")
lines(chile.2020$new_cases, col = "yellow", type = "l")
lines(venezuela.2020$new_cases, col = "green", type = "l")
axis(1, at = c(0,31,62), labels = c("Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

plot(brazil.2021$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_cases)), xlab = "Month", ylab = "New cases", main = "New cases in 2021")
lines(chile.2021$new_cases, col = "yellow", type = "l")
lines(venezuela.2021$new_cases, col = "green", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Jan", "Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

plot(brazil.2022$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2022$new_cases)), xlab = "Month", ylab = "New cases", main = "New cases in 2022")
lines(chile.2022$new_cases, col = "yellow", type = "l")
lines(venezuela.2022$new_cases, col = "green", type = "l")
axis(1, at = c(0,31), labels = c("Jan", "Feb"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#2
#2020
plot(brazil.2020$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New deaths in 2020")
lines(chile.2020$new_deaths, col = "yellow", type = "l")
lines(venezuela.2020$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31,62), labels = c("Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2021
plot(brazil.2021$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New deaths in 2021")
lines(chile.2021$new_deaths, col = "yellow", type = "l")
lines(venezuela.2021$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Jan", "Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2022
plot(brazil.2022$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2022$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New deaths in 2021")
lines(chile.2022$new_deaths, col = "yellow", type = "l")
lines(venezuela.2022$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31), labels = c("Jan", "Feb"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#3
#2020
plot(brazil.2020$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_cases)), xlab = "Month", ylab = "New cases/deaths", main = "New cases/deaths in 2020")
lines(chile.2020$new_cases, col = "yellow", type = "l")
lines(venezuela.2020$new_cases, col = "green", type = "l")
lines(brazil.2020$new_deaths, col = "blue", type = "l")
lines(chile.2020$new_deaths, col = "violet", type = "l")
lines(venezuela.2020$new_deaths, col = "orange", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Mar", "Jun", "Jul", "Aug"))
legend("topleft", legend = c("Brazil_New cases", "Chile_New cases", "Venezuela_New cases", "Brazil_New deaths", "Chile_New deaths", "Venezuela_New deaths"), 
                  col = c("red","yellow", "green", "blue", "violet", "orange"), lty = 1, ncol = 1, bty = "o", 
                  text.col = c("red","yellow", "green", "blue", "violet", "orange"))
#2021
plot(brazil.2021$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_cases)), xlab = "Month", ylab = "New cases/deaths", main = "New cases/deaths in 2021")
lines(chile.2021$new_cases, col = "yellow", type = "l")
lines(venezuela.2021$new_cases, col = "green", type = "l")
lines(brazil.2021$new_deaths, col = "blue", type = "l")
lines(chile.2021$new_deaths, col = "violet", type = "l")
lines(venezuela.2021$new_deaths, col = "orange", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Jan","Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil_New cases", "Chile_New cases", "Venezuela_New cases", "Brazil_New deaths", "Chile_New deaths", "Venezuela_New deaths"), 
       col = c("red","yellow", "green", "blue", "violet", "orange"), lty = 1, ncol = 1, bty = "o",
       text.col = c("red","yellow", "green", "blue", "violet", "orange"), cex = 0.8)
#2022
plot(brazil.2022$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2022$new_cases)), xlab = "Month", ylab = "New cases/deaths", main = "New cases/deaths in 2022")
lines(chile.2022$new_cases, col = "yellow", type = "l")
lines(venezuela.2022$new_cases, col = "green", type = "l")
lines(brazil.2022$new_deaths, col = "blue", type = "l")
lines(chile.2022$new_deaths, col = "violet", type = "l")
lines(venezuela.2022$new_deaths, col = "orange", type = "l")
axis(1, at = c(0,31), labels = c("Jan","Feb"))
legend("topleft", legend = c("Brazil_New cases", "Chile_New cases", "Venezuela_New cases", "Brazil_New deaths", "Chile_New deaths", "Venezuela_New deaths"), 
       col = c("red","yellow", "green", "blue", "violet", "orange"), lty = 1, ncol = 1, bty = "o",
       text.col = c("red","yellow", "green", "blue", "violet", "orange"))

#Seperate data for 4,5,6
brazil <- subset(owid_covid_data, location == "Brazil")
brazil <- subset(brazil, month(date) == 11 | month(date) == 12)
brazil.2020 <- subset(brazil, year(date) == 2020)
brazil.2021 <- subset(brazil, year(date) == 2021)
chile <- subset(owid_covid_data, location == "Chile")
chile <- subset(chile, month(date) == 11 | month(date) == 12)
chile.2020 <- subset(chile, year(date) == 2020)
chile.2021 <- subset(chile, year(date) == 2021)
venezuela <- subset(owid_covid_data, location == "Venezuela")
venezuela <- subset(venezuela, month(date) == 11 | month(date) == 12)
venezuela.2020 <- subset(venezuela, year(date) == 2020)
venezuela.2021 <- subset(venezuela, year(date) == 2021)

#4
#2020
plot(brazil.2020$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_cases)), xlab = "Month", ylab = "New cases", main = "New cases in the last 2 months of 2020")
lines(chile.2020$new_cases, col = "yellow", type = "l")
lines(venezuela.2020$new_cases, col = "green", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2021
plot(brazil.2021$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_cases)), xlab = "Month", ylab = "New cases", main = "New cases in the last 2 months of 2021")
lines(chile.2021$new_cases, col = "yellow", type = "l")
lines(venezuela.2021$new_cases, col = "green", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topright", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#5
#2020
plot(brazil.2020$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New deaths in the last 2 months of 2020")
lines(chile.2020$new_deaths, col = "yellow", type = "l")
lines(venezuela.2020$new_deaths, col = "green", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2021
plot(brazil.2021$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New deaths in the last 2 months of 2021")
lines(chile.2021$new_deaths, col = "yellow", type = "l")
lines(venezuela.2021$new_deaths, col = "green", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topright", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#6
#2020
plot(brazil.2020$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_cases)), xlab = "Month", ylab = "New cases/deaths", main = "New cases/deaths in the last 2 months of 2020")
lines(chile.2020$new_cases, col = "yellow", type = "l")
lines(venezuela.2020$new_cases, col = "green", type = "l")
lines(brazil.2020$new_deaths, col = "blue", type = "l")
lines(chile.2020$new_deaths, col = "violet", type = "l")
lines(venezuela.2020$new_deaths, col = "orange", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topleft", legend = c("Brazil_New cases", "Chile_New cases", "Venezuela_New cases", "Brazil_New deaths", "Chile_New deaths", "Venezuela_New deaths"), 
       col = c("red","yellow", "green", "blue", "violet", "orange"), lty = 1, ncol = 1, bty = "o", 
       text.col = c("red","yellow", "green", "blue", "violet", "orange"), cex = 0.8)
#2021
plot(brazil.2021$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_cases)), xlab = "Month", ylab = "New cases/deaths", main = "New cases/deaths in the last 2 months of 2021")
lines(chile.2021$new_cases, col = "yellow", type = "l")
lines(venezuela.2021$new_cases, col = "green", type = "l")
lines(brazil.2021$new_deaths, col = "blue", type = "l")
lines(chile.2021$new_deaths, col = "violet", type = "l")
lines(venezuela.2021$new_deaths, col = "orange", type = "l")
axis(1, at = c(0,30), labels = c("Nov", "Dec"))
legend("topright", legend = c("Brazil_New cases", "Chile_New cases", "Venezuela_New cases", "Brazil_New deaths", "Chile_New deaths", "Venezuela_New deaths"), 
       col = c("red","yellow", "green", "blue", "violet", "orange"), lty = 1, ncol = 1, bty = "o", 
       text.col = c("red","yellow", "green", "blue", "violet", "orange"), cex = 0.8)

#Seperate data for 7, 8
brazil <- subset(owid_covid_data, location == "Brazil")
brazil$new_cases <- cumsum(brazil$new_cases)
brazil$new_deaths <- cumsum(brazil$new_deaths)
brazil <- subset(brazil, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
brazil.2020 <- subset(brazil, year(date) == 2020)
brazil.2021 <- subset(brazil, year(date) == 2021)
brazil.2022 <- subset(brazil, year(date) == 2022)
chile <- subset(owid_covid_data, location == "Chile")
chile$new_cases <- cumsum(chile$new_cases)
chile$new_deaths <- cumsum(chile$new_deaths)
chile <- subset(chile, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
chile.2020 <- subset(chile, year(date) == 2020)
chile.2021 <- subset(chile, year(date) == 2021)
chile.2022 <- subset(chile, year(date) == 2022)
venezuela <- subset(owid_covid_data, location == "Venezuela")
venezuela$new_cases <- cumsum(venezuela$new_cases)
venezuela$new_deaths <- cumsum(venezuela$new_deaths)
venezuela <- subset(venezuela, month(date) == 1 | month(date) == 3 | month(date) == 6 |month(date) == 7)
venezuela.2020 <- subset(venezuela, year(date) == 2020)
venezuela.2021 <- subset(venezuela, year(date) == 2021)
venezuela.2022 <- subset(venezuela, year(date) == 2022)

#7
#2020
plot(brazil.2020$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_cases)), xlab = "Month", ylab = "New cases", main = "New cumulative cases in 2020")
lines(chile.2020$new_cases, col = "yellow", type = "l")
lines(venezuela.2020$new_cases, col = "green", type = "l")
axis(1, at = c(0,31,62), labels = c("Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2021
plot(brazil.2021$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_cases)), xlab = "Month", ylab = "New cases", main = "New cumulative cases in 2021")
lines(chile.2021$new_cases, col = "yellow", type = "l")
lines(venezuela.2021$new_cases, col = "green", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Jan", "Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2022
plot(brazil.2022$new_cases, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2022$new_cases)), xlab = "Month", ylab = "New cases", main = "New cumulative cases in 2022")
lines(chile.2022$new_cases, col = "yellow", type = "l")
lines(venezuela.2022$new_cases, col = "green", type = "l")
axis(1, at = c(0,31), labels = c("Jan", "Feb"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#8
#2020
plot(brazil.2020$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2020$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New cumulative deaths in 2020")
lines(chile.2020$new_deaths, col = "yellow", type = "l")
lines(venezuela.2020$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31,62), labels = c("Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2021
plot(brazil.2021$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2021$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New cumulative deaths in 2021")
lines(chile.2021$new_deaths, col = "yellow", type = "l")
lines(venezuela.2021$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31,62,92), labels = c("Jan", "Mar", "Jun", "Jul"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))
#2022
plot(brazil.2022$new_deaths, type = "l",xaxt="n", col = "red", ylim = c(0, max(brazil.2022$new_deaths)), xlab = "Month", ylab = "New deaths", main = "New cumulative deaths in 2021")
lines(chile.2022$new_deaths, col = "yellow", type = "l")
lines(venezuela.2022$new_deaths, col = "green", type = "l")
axis(1, at = c(0,31), labels = c("Jan", "Feb"))
legend("topleft", legend = c("Brazil", "Chile", "Venezuela"), lty = 1, col = c("red","yellow", "green"), ncol = 1, bty = "o", text.col = c("red","yellow", "green"))

#--------------------------------------------------------------------------------------------------------
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


