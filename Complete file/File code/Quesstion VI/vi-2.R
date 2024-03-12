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
brazil_Made<-subset(brazil,(month=="01")|(month=="03")|(month=="06")|(month=="07"))
brazil_Made_2020<-subset(brazil_Made,(year=="2020"))
brazil_Made_2021<-subset(brazil_Made,(year=="2021"))
brazil_Made_2022<-subset(brazil_Made,(year=="2022"))

library(ggplot2)
ggplot(brazil_Made_2020,(aes(x = dates, y = new_deaths_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 03,06,07-2020 cua Brazil",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(brazil_Made_2021,(aes(x = dates, y = new_deaths_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 01,03,06,07-2021 cua Brazil",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(brazil_Made_2022,(aes(x = dates, y = new_deaths_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao thang 01-2022 cua Brazil",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 


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
chile_Made<-subset(chile,(month=="01")|(month=="03")|(month=="06")|(month=="07"))
chile_Made_2020<-subset(chile_Made,(year=="2020"))
chile_Made_2021<-subset(chile_Made,(year=="2021"))
chile_Made_2022<-subset(chile_Made,(year=="2022"))

library(ggplot2)
ggplot(chile_Made_2020,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 03,06,07-2020 cua Chile",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(chile_Made_2021,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 01,03,06,07-2021 cua Chile",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(chile_Made_2022,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao thang 01-2022 cua Chile",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31"))


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
venezuela_Made<-subset(venezuela,(month=="01")|(month=="03")|(month=="06")|(month=="07"))
venezuela_Made_2020<-subset(venezuela_Made,(year=="2020"))
venezuela_Made_2021<-subset(venezuela_Made,(year=="2021"))
venezuela_Made_2022<-subset(venezuela_Made,(year=="2022"))

library(ggplot2)
ggplot(venezuela_Made_2020,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 03,06,07-2020 cua Venezuela",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(venezuela_Made_2021,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao cac thang 01,03,06,07-2021 cua Venezuela",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31")) 
ggplot(venezuela_Made_2022,(aes(x = dates, y = new_cases_tb))) + geom_line(aes(color = month),group=1) +facet_wrap( ~ month)  +labs(title = "Bieu do the hien thu thap tu vong vao thang 01-2022 cua Venezuela",subtitle = "Theo trung binh 7 ngay gan nhat",y = "So ca tu vong",x = "Ngay",color = "Chu thich")+scale_x_discrete(breaks=c("10","20","31"))
