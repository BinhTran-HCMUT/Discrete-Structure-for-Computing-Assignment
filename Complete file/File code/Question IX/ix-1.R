owid_covid_data<-read.csv ("owid-covid-data.csv", header=TRUE) 
owid_covid_data[is.na(owid_covid_data)]<- 0
owid_covid_data$new_cases<-abs(owid_covid_data$new_cases)
owid_covid_data$new_deaths<-abs(owid_covid_data$new_deaths)


brazil<-subset(owid_covid_data,location=="Brazil")
require(tidyr)
brazil$cases_tichluy<-cumsum(replace_na(brazil$new_cases, 0))
brazil$death_tichluy<-cumsum(replace_na(brazil$new_deaths, 0))
brazil$cases_pc<-brazil$cases_tichluy/sum(brazil[, 'new_cases'], na.rm = TRUE)*100
brazil$death_pc<-brazil$death_tichluy/sum(brazil[, 'new_deaths'], na.rm = TRUE)*100
brazil$date<-format(as.Date(brazil$date, format = "%m/%d/%Y"), "%Y-%m-%d")
library(ggplot2)
ggplot()+geom_line(data=brazil, aes(x=date, y=cases_pc,group=1,col="red"))+geom_line(data=brazil, aes(x=date, y=death_pc,group=1,col="blue"))+labs(x="Thoi gian", y="%")+ggtitle("Bieu do the hien phan tram giua nhiem benh tich luy tren tong nhiem benh và phan tram
tu vong tich luy tren tong so tu vong cua Brazil trong giai đoan tu 26/2/2020 đen 19/2/2022")+scale_color_manual(labels = c("Tu vong", "Nhiem benh"), values = c("blue", "red"))  +scale_x_discrete(breaks=c("2020-02-26","2020-10-19","2021-06-15","2022-02-19"))+scale_color_manual(labels = c( "Nhiem benh","Tu Vong"), values= c("red", "blue"))  

chile<-subset(owid_covid_data,location=="Chile")
chile[is.na(chile)]<- 0
require(tidyr)
chile$cases_tichluy<-cumsum(replace_na(chile$new_cases, 0))
chile$death_tichluy<-cumsum(replace_na(chile$new_deaths, 0))
chile$cases_pc<-chile$cases_tichluy/sum(chile[, 'new_cases'], na.rm = TRUE)*100
chile$death_pc<-chile$death_tichluy/sum(chile[, 'new_deaths'], na.rm = TRUE)*100
chile$date<-format(as.Date(chile$date, format = "%m/%d/%Y"), "%Y-%m-%d")
library(ggplot2)
ggplot()+geom_line(data=chile, aes(x=date, y=cases_pc,group=1,col="red"))+geom_line(data=brazil, aes(x=date, y=death_pc,group=1,col="blue"))+labs(x="Thoi gian", y="%")+ggtitle("Bieu do the hien phan tram giua nhiem benh tich luy tren tong nhiem benh và phan tram
tu vong tich luy tren tong so tu vong cua Chile trong giai đoan tu 23/2/2020 đen 19/2/2022")+scale_color_manual(labels = c("Tu vong", "Nhiem benh"), values = c("blue", "red"))  +scale_x_discrete(breaks=c("2020-02-23","2020-10-19","2021-06-15","2022-02-19")) +scale_color_manual(labels = c( "Nhiem benh","Tu Vong"), values= c("red", "blue"))  

venezuela<-subset(owid_covid_data,location=="Venezuela")
venezuela[is.na(venezuela)]<- 0
require(tidyr)
venezuela$cases_tichluy<-cumsum(replace_na(venezuela$new_cases, 0))
venezuela$death_tichluy<-cumsum(replace_na(venezuela$new_deaths, 0))
venezuela$cases_pc<-venezuela$cases_tichluy/sum(venezuela[, 'new_cases'], na.rm = TRUE)*100
venezuela$death_pc<-venezuela$death_tichluy/sum(venezuela[, 'new_deaths'], na.rm = TRUE)*100
venezuela$date<-format(as.Date(venezuela$date, format = "%m/%d/%Y"), "%Y-%m-%d")
library(ggplot2)
ggplot()+geom_line(data=venezuela, aes(x=date, y=cases_pc,group=1,col="red"))+geom_line(data=brazil, aes(x=date, y=death_pc,group=1,col="blue"))+labs(x="Thoi gian", y="%")+ggtitle("Bieu do the hien phan tram giua nhiem benh tich luy tren tong nhiem benh và phan tram
tu vong tich luy tren tong so tu vong cua Venezuela trong giai đoan tu 14/3/2020 đen 19/2/2022")+scale_color_manual(labels = c("Tu vong", "Nhiem benh"), values = c("blue", "red"))  +scale_x_discrete(breaks=c("2020-03-14","2020-10-19","2021-06-15","2022-02-19")) +scale_color_manual(labels = c( "Nhiem benh","Tu Vong"), values= c("red", "blue"))  


