owid_covid_data<-read.csv ("owid-covid-data.csv", header=TRUE) 
owid_covid_data[is.na(owid_covid_data)]<- 0

m<-function(x){
  n <- length(x)
  y <- rep(0,n)
  for (i in 1:n){
    if (i <= 6){
      y[i] = mean(x[1:i])
    }else{
      y[i] = mean(x[(i-6):i])
    }
  }
  y
}

brazil<-subset(owid_covid_data,location=="Brazil")
brazil$new_cases<-abs(brazil$new_cases)
brazil$new_deaths<-abs(brazil$new_deaths)
brazil$date<-format(as.Date(brazil$date, format = "%m/%d/%Y"), "%Y-%m-%d")
brazil$year <- strftime(brazil$date, "%Y")    
brazil$month <- strftime(brazil$date, "%m")
brazil$dates<- strftime(brazil$date, "%d")
brazil$new_cases_tb<-m(brazil$new_cases)
brazil$new_cases_tb<-ceiling(brazil$new_cases_tb)
brazil$new_deaths_tb<-m(brazil$new_deaths)
brazil$new_deaths_tb<-ceiling(brazil$new_deaths_tb)
brazil_2tc<-subset(brazil,(month=="11")|(month=="12"))
brazil_2tc_2020<-subset(brazil_2tc,(year=="2020"))
brazil_2tc_2021<-subset(brazil_2tc,(year=="2021"))
library(ggplot2)
ggplot()+geom_line(data=brazil_2tc_2020, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2020-11-01","2020-12-01","2020-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Brazil thang 11,12/2020",x="Thoi gian", y="So ca tu vong moi")
ggplot()+geom_line(data=brazil_2tc_2021, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2021-11-01","2021-12-01","2021-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Brazil thang 11,12/2021",x="Thoi gian", y="So ca tu vong moi")


chile<-subset(owid_covid_data,location=="Chile")
chile$new_cases<-abs(chile$new_cases)
chile$new_deaths<-abs(chile$new_deaths)
chile$date<-format(as.Date(chile$date, format = "%m/%d/%Y"), "%Y-%m-%d")
chile$year <- strftime(chile$date, "%Y")    
chile$month <- strftime(chile$date, "%m")
chile$dates<- strftime(chile$date, "%d")
chile$new_cases_tb<-m(chile$new_cases)
chile$new_cases_tb<-ceiling(chile$new_cases_tb)
chile$new_deaths_tb<-m(chile$new_deaths)
chile$new_deaths_tb<-ceiling(chile$new_deaths_tb)
chile_2tc<-subset(chile,(month=="11")|(month=="12"))
chile_2tc_2020<-subset(chile_2tc,(year=="2020"))
chile_2tc_2021<-subset(chile_2tc,(year=="2021"))
library(ggplot2)
ggplot()+geom_line(data=chile_2tc_2020, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2020-11-01","2020-12-01","2020-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Chile thang 11,12/2020",x="Thoi gian", y="So ca tu vong moi")
ggplot()+geom_line(data=chile_2tc_2021, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2021-11-01","2021-12-01","2021-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Chile thang 11,12/2021",x="Thoi gian", y="So ca tu vong moi")


venezuela<-subset(owid_covid_data,location=="Venezuela")
venezuela$new_cases<-abs(venezuela$new_cases)
venezuela$new_deaths<-abs(venezuela$new_deaths)
venezuela$date<-format(as.Date(venezuela$date, format = "%m/%d/%Y"), "%Y-%m-%d")
venezuela$year <- strftime(venezuela$date, "%Y")    
venezuela$month <- strftime(venezuela$date, "%m")
venezuela$dates<- strftime(venezuela$date, "%d")
venezuela$new_cases_tb<-m(venezuela$new_cases)
venezuela$new_cases_tb<-ceiling(venezuela$new_cases_tb)
venezuela$new_deaths_tb<-m(venezuela$new_deaths)
venezuela$new_deaths_tb<-ceiling(venezuela$new_deaths_tb)
venezuela_2tc<-subset(venezuela,(month=="11")|(month=="12"))
venezuela_2tc_2020<-subset(venezuela_2tc,(year=="2020"))
venezuela_2tc_2021<-subset(venezuela_2tc,(year=="2021"))
library(ggplot2)
ggplot()+geom_line(data=venezuela_2tc_2020, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2020-11-01","2020-12-01","2020-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Venezuela thang 11,12/2020",x="Thoi gian", y="So ca tu vong moi")
ggplot()+geom_line(data=venezuela_2tc_2021, aes(x=date, y=new_deaths_tb,group=1,col="red"))+scale_x_discrete(breaks=c("2021-11-01","2021-12-01","2021-12-31"))+labs(title = "Bieu do the hien thu thap tu vong o Venezuela thang 11,12/2021",x="Thoi gian", y="So ca tu vong moi")

