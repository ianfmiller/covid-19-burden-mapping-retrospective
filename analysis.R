# set up
library(RCurl)
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

data<-data.frame(fips=data.fips,name=name,p.rural=p.rural,tot.pop=tot.pop,cases=cases,deaths=deaths)

