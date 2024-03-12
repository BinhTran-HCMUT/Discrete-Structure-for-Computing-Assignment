owid_covid_data<-read.csv ("owid-covid-data.csv", header=TRUE) 
owid_covid_data[is.na(owid_covid_data)]<- 0
owid_covid_data$new_cases<-abs(owid_covid_data$new_cases)
owid_covid_data$new_deaths<-abs(owid_covid_data$new_deaths)

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
brazil$date<-format(as.Date(brazil$date, format = "%m/%d/%Y"), "%Y-%m-%d")
brazil$year <- strftime(brazil$date, "%Y")    
brazil$month <- strftime(brazil$date, "%m")
brazil$dates<- strftime(brazil$date, "%d")

brazil$new_cases_tb<-m(brazil$new_cases)
brazil$new_cases_tb<-ceiling(brazil$new_cases_tb)
brazil$new_deaths_tb<-m(brazil$new_deaths)
brazil$new_deaths_tb<-ceiling(brazil$new_deaths_tb)

brazil_2020<-subset(brazil,(year=="2020"))
brazil_2021<-subset(brazil,(year=="2021"))
brazil_2022<-subset(brazil,(year=="2022"))

brazil_2020_3<-subset(brazil_2020,(month=="03"))
brazil_2020_6<-subset(brazil_2020,(month=="06"))
brazil_2020_7<-subset(brazil_2020,(month=="07"))

brazil_2021_1<-subset(brazil_2021,(month=="01"))
brazil_2021_3<-subset(brazil_2021,(month=="03"))
brazil_2021_6<-subset(brazil_2021,(month=="06"))
brazil_2021_7<-subset(brazil_2021,(month=="07"))

brazil_2022_1<-subset(brazil_2022,(month=="01"))
library(ggplot2)

ggplot(brazil_2020_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2020 cua Brazil") + geom_smooth()
print(cor(brazil_2020_3$new_cases_tb,brazil_2020_3$new_deaths_tb))

ggplot(brazil_2020_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2020 cua Brazil") +  geom_smooth()
print(cor(brazil_2020_6$new_cases_tb,brazil_2020_6$new_deaths_tb)) 

ggplot(brazil_2020_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2020 cua Brazil") + geom_smooth()
print(cor(brazil_2020_7$new_cases_tb,brazil_2020_7$new_deaths_tb)) 

ggplot(brazil_2021_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2021 cua Brazil") + geom_smooth()
print(cor(brazil_2021_1$new_cases_tb,brazil_2021_1$new_deaths_tb)) 

ggplot(brazil_2021_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2021 cua Brazil")+ geom_smooth()
print(cor(brazil_2021_3$new_cases_tb,brazil_2021_3$new_deaths_tb)) 

ggplot(brazil_2021_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2021 cua Brazil")+  geom_smooth()
print(cor(brazil_2021_6$new_cases_tb,brazil_2021_6$new_deaths_tb)) 

ggplot(brazil_2021_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2021 cua Brazil")+ geom_smooth()
print(cor(brazil_2021_7$new_cases_tb,brazil_2021_7$new_deaths_tb)) 

ggplot(brazil_2022_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2022 cua Brazil")+ geom_smooth()
print(cor(brazil_2022_1$new_cases_tb,brazil_2022_1$new_deaths_tb))




chile<-subset(owid_covid_data,location=="Chile")
chile$date<-format(as.Date(chile$date, format = "%m/%d/%Y"), "%Y-%m-%d")
chile$year <- strftime(chile$date, "%Y")    
chile$month <- strftime(chile$date, "%m")
chile$dates<- strftime(chile$date, "%d")

chile$new_cases_tb<-m(chile$new_cases)
chile$new_cases_tb<-ceiling(chile$new_cases_tb)
chile$new_deaths_tb<-m(chile$new_deaths)
chile$new_deaths_tb<-ceiling(chile$new_deaths_tb)

chile_2020<-subset(chile,(year=="2020"))
chile_2021<-subset(chile,(year=="2021"))
chile_2022<-subset(chile,(year=="2022"))

chile_2020_3<-subset(chile_2020,(month=="03"))
chile_2020_6<-subset(chile_2020,(month=="06"))
chile_2020_7<-subset(chile_2020,(month=="07"))

chile_2021_1<-subset(chile_2021,(month=="01"))
chile_2021_3<-subset(chile_2021,(month=="03"))
chile_2021_6<-subset(chile_2021,(month=="06"))
chile_2021_7<-subset(chile_2021,(month=="07"))

chile_2022_1<-subset(chile_2022,(month=="01"))

library(ggplot2)

ggplot(chile_2020_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2020 cua Chile") + geom_smooth()
print(cor(chile_2020_3$new_cases_tb,chile_2020_3$new_deaths_tb))

ggplot(chile_2020_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2020 cua Chile") +  geom_smooth()
print(cor(chile_2020_6$new_cases_tb,chile_2020_6$new_deaths_tb)) 

ggplot(chile_2020_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2020 cua Chile") + geom_smooth()
print(cor(chile_2020_7$new_cases_tb,chile_2020_7$new_deaths_tb)) 

ggplot(chile_2021_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2021 cua Chile") + geom_smooth()
print(cor(chile_2021_1$new_cases_tb,chile_2021_1$new_deaths_tb)) 

ggplot(chile_2021_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2021 cua Chile")+ geom_smooth()
print(cor(chile_2021_3$new_cases_tb,chile_2021_3$new_deaths_tb)) 

ggplot(chile_2021_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2021 cua Chile")+  geom_smooth()
print(cor(chile_2021_6$new_cases_tb,chile_2021_6$new_deaths_tb)) 

ggplot(chile_2021_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2021 cua Chile")+ geom_smooth()
print(cor(chile_2021_7$new_cases_tb,chile_2021_7$new_deaths_tb)) 

ggplot(chile_2022_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2022 cua Chile")+ geom_smooth()
print(cor(chile_2022_1$new_cases_tb,chile_2022_1$new_deaths_tb))




venezuela<-subset(owid_covid_data,location=="Venezuela")
venezuela$date<-format(as.Date(venezuela$date, format = "%m/%d/%Y"), "%Y-%m-%d")
venezuela$year <- strftime(venezuela$date, "%Y")    
venezuela$month <- strftime(venezuela$date, "%m")
venezuela$dates<- strftime(venezuela$date, "%d")

venezuela$new_cases_tb<-m(venezuela$new_cases)
venezuela$new_cases_tb<-ceiling(venezuela$new_cases_tb)
venezuela$new_deaths_tb<-m(venezuela$new_deaths)
venezuela$new_deaths_tb<-ceiling(venezuela$new_deaths_tb)

venezuela_2020<-subset(venezuela,(year=="2020"))
venezuela_2021<-subset(venezuela,(year=="2021"))
venezuela_2022<-subset(venezuela,(year=="2022"))

venezuela_2020_3<-subset(venezuela_2020,(month=="03"))
venezuela_2020_6<-subset(venezuela_2020,(month=="06"))
venezuela_2020_7<-subset(venezuela_2020,(month=="07"))

venezuela_2021_1<-subset(venezuela_2021,(month=="01"))
venezuela_2021_3<-subset(venezuela_2021,(month=="03"))
venezuela_2021_6<-subset(venezuela_2021,(month=="06"))
venezuela_2021_7<-subset(venezuela_2021,(month=="07"))

venezuela_2022_1<-subset(venezuela_2022,(month=="01"))

library(ggplot2)

ggplot(venezuela_2020_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2020 cua Venezuela") + geom_smooth()
print(cor(venezuela_2020_3$new_cases_tb,venezuela_2020_3$new_deaths_tb))

ggplot(venezuela_2020_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2020 cua Venezuela") +  geom_smooth()
print(cor(venezuela_2020_6$new_cases_tb,venezuela_2020_6$new_deaths_tb)) 

ggplot(venezuela_2020_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2020 cua Venezuela") + geom_smooth()
print(cor(venezuela_2020_7$new_cases_tb,venezuela_2020_7$new_deaths_tb)) 

ggplot(venezuela_2021_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2021 cua Venezuela") + geom_smooth()
print(cor(venezuela_2021_1$new_cases_tb,venezuela_2021_1$new_deaths_tb)) 

ggplot(venezuela_2021_3,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 3/2021 cua Venezuela")+ geom_smooth()
print(cor(venezuela_2021_3$new_cases_tb,venezuela_2021_3$new_deaths_tb)) 

ggplot(venezuela_2021_6,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 6/2021 cua Venezuela")+  geom_smooth()
print(cor(venezuela_2021_6$new_cases_tb,venezuela_2021_6$new_deaths_tb)) 

ggplot(venezuela_2021_7,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 7/2021 cua Venezuela")+ geom_smooth()
print(cor(venezuela_2021_7$new_cases_tb,venezuela_2021_7$new_deaths_tb)) 

ggplot(venezuela_2022_1,(aes(x=new_cases_tb,y=new_deaths_tb)))+geom_point(shape=1, color="blue")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2022 cua Venezuela")+ geom_smooth()
print(cor(venezuela_2022_1$new_cases_tb,venezuela_2022_1$new_deaths_tb))
