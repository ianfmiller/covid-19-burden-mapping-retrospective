# set up
library(RCurl)
library(mgcv)
setwd("~/Documents/GitHub/covid-19-burden-mapping-retrospective")

# prep data 

## load burden data
if(!any(grepl("nyt_county_data",dir())))
{
  download <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  data <- read.csv (text = download)
  file.name<-paste0("nyt_county_data_",Sys.Date(),".csv")
  write.csv(data,file=file.name)
} else {burden.data<-read.csv(dir()[grep("nyt_county_data",dir())])}

## load demography data
raw.demog<-read.csv("us.demog.data.csv",skip=1) #read from github
demog<-raw.demog[,c(1,2,3,grep("2018",colnames(raw.demog)))] #pull out 2018 data
colnames(demog)<-gsub("Population.Estimate..as.of.July.1....2018...","",colnames(demog)) #make column names easier to read
fips<-as.character(unlist(strsplit(as.character(demog$Id),"US"))[seq(2,nrow(demog)*2,2)]) #pull out fips codes
demog<-data.frame("fips"=fips,demog) #add on fips column
demog<-demog[order(demog$fips),] #sort by fips code

## urban/rural data https://data.census.gov/cedsci/table?q=urban%20area%20deliniation&hidePreview=true&tid=DECENNIALSF12010.P2&vintage=2010&g=0100000US.050000
urban.rural<-read.csv("US.pop.urban.rural.csv")
urban.rural$fips[which(nchar(urban.rural$fips)==4)]<-paste0("0",urban.rural$fips[which(nchar(urban.rural$fips)==4)]) #correct fips with leading 0s

## correct fips codes for counties that changed names https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
urban.rural$fips[93]<-"02158"
urban.rural$fips[2418]<-"46102"
urban.rural<-urban.rural[-which(!urban.rural$fips %in% fips),]
urban.rural<-urban.rural[match(fips,urban.rural$fips),]

data.fips<-c()
name<-c()
p.rural<-c()
tot.pop<-c()
cases<-c()
deaths<-c()

## join relevant data
for(fips in demog$Id2)
{
  ### if fips does not correspond to NYC or Alaskan FIPS codes without data (Hoonah-Angoon Census Area,Yakutat City and Borough)
  if(!(fips %in% c(36005,36047,36061,36081,36085,02105,02282)))
  {
    new.name<-demog[which(demog$Id2==fips),"Geography"]
    new.tot.pop<-demog[which(demog$Id2==fips),"Both.Sexes..Total"]
    new.p.rural<-urban.rural[which(as.numeric(urban.rural$fips)==fips),"Total.Rural"]/urban.rural[which(as.numeric(urban.rural$fips)==fips),"Total"]
    sub.1.burden.data<-burden.data[which(burden.data$fips==fips),]
    sub.2.burden.data<-sub.1.burden.data[which(sub.1.burden.data$date==max(sub.1.burden.data$date)),]
    new.cases<-sub.2.burden.data$cases
    new.deaths<-sub.2.burden.data$deaths
    data.fips<-c(data.fips,fips)
    name<-c(name,new.name)
    p.rural<-c(p.rural,new.p.rural)
    tot.pop<-c(tot.pop,new.tot.pop)
    cases<-c(cases,new.cases)
    deaths<-c(deaths,new.deaths)
  } else{
    ### if fips does correspond to NYC; only run once
    if(fips==36005)
    {
      new.name<-"New York City"
      new.tot.pop<-sum(demog[which(demog$Id2 %in% c(36005,36047,36061,36081,36085)),"Both.Sexes..Total"])
      new.p.rural<-sum(urban.rural[which(as.numeric(urban.rural$fips) %in% c(36005,36047,36061,36081,36085)),"Total.Rural"])/sum(urban.rural[which(as.numeric(urban.rural$fips) %in% c(36005,36047,36061,36081,36085)),"Total"])
      sub.1.burden.data<-burden.data[which(burden.data$county=="New York City"),]
      sub.2.burden.data<-sub.1.burden.data[which(sub.1.burden.data$date==max(sub.1.burden.data$date)),]
      new.cases<-sub.2.burden.data$cases
      new.deaths<-sub.2.burden.data$deaths
      data.fips<-c(data.fips,fips)
      name<-c(name,new.name)
      p.rural<-c(p.rural,new.p.rural)
      tot.pop<-c(tot.pop,new.tot.pop)
      cases<-c(cases,new.cases)
      deaths<-c(deaths,new.deaths)
    }
  }

}

data<-data.frame(fips=data.fips,name=name,p.rural=p.rural,tot.pop=tot.pop,cases=cases,deaths=deaths,cases.per.cap=cases/tot.pop,deaths.per.cap=deaths/tot.pop)

# analysis

## plot data

par(mfrow=c(1,2))
plot(data$p.rural,data$cases.per.cap,xlab="% rural",ylab="cases per capita")
plot(data$p.rural,data$deaths.per.cap,xlab="% rural",ylab="deaths per capita")

## gams

case.mod.gam<-gam(cases.per.cap~s(p.rural),data=data)
case.mod.gamlss<-gam(list(cases.per.cap~s(p.rural),~s(p.rural)),data=data,family=gaulss())
death.mod.gam<-gam(deaths.per.cap~s(p.rural),data=data)
death.mod.gamlss<-gam(list(deaths.per.cap~s(p.rural),~s(p.rural)),data=data,family=gaulss())

xx<-data.frame(p.rural=seq(0,1,.01))
case.pred<-predict.gam(case.mod.gamlss,newdata = xx,type="response")
plot(case.pred[,1],ylim=c(.0,.035),type="l")
points(case.pred[,1]+1/case.pred[,2],type="l",lty=2)
points(case.pred[,1]-1/case.pred[,2],type="l",lty=2)

death.pred<-predict.gam(death.mod.gamlss,newdata = xx,type="response")
plot(death.pred[,1],ylim=c(-1.1,1.1),type="l")
points(death.pred[,1]+1/death.pred[,2],type="l",lty=2)
points(death.pred[,1]-1/death.pred[,2],type="l",lty=2)

# how do deaths per capita compare to cases per capita? Do we see an unusually high death burden relative to case burden in rural areas?

## linear model approach
mod<-lm(deaths.per.cap~cases.per.cap+p.rural,data=data)
plot(data$cases.per.cap,data$deaths.per.cap,xlab="cases per capita",ylab="deaths per capita")
abline(8.921e-05+0*-8.861e-05,2.063e-02,col=rainbow(3)[1])
abline(8.921e-05+0.5*-8.861e-05,2.063e-02,col=rainbow(3)[2])
abline(8.921e-05+1.0*-8.861e-05,2.063e-02,col=rainbow(3)[3])
legend("topright",legend=c("0% rural","50% rural","100% rural"),lty=1,col=rainbow(3))

## gam approach
mod<-gam(deaths.per.cap~s(cases.per.cap)+s(p.rural),data=data)
xx<-seq(0,.15,.001)
yy1<-rep(0,times=length(xx))
yy2<-rep(.5,times=length(xx))
yy3<-rep(1,times=length(xx))

new.data1<-data.frame(cases.per.cap=xx,p.rural=yy1)
new.data2<-data.frame(cases.per.cap=xx,p.rural=yy2)
new.data3<-data.frame(cases.per.cap=xx,p.rural=yy3)

plot(data$cases.per.cap,data$deaths.per.cap,xlab="cases per capita",ylab="deaths per capita")
legend("topright",legend=c("0% rural","50% rural","100% rural"),lty=1,col=rainbow(3))

points(xx,predict(mod,newdata = new.data1,type="response"),type="l",col=rainbow(3)[1])
points(xx,predict(mod,newdata = new.data2,type="response"),type="l",col=rainbow(3)[2])
points(xx,predict(mod,newdata = new.data3,type="response"),type="l",col=rainbow(3)[3])

