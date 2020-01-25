library("tidyverse")
library("RSocrata")

# download data from open data site
nypd_complaint <- read.socrata("https://data.cityofnewyork.us/resource/qgea-i56i.json?$where=cmplnt_fr_dt between '2010-01-01T00:00:00.000' and '2019-12-31T23:59:59.000'",
  email = socrataEmail, 
  password = socrataPassword)
  
# up date by year and offense. keep data for first 19 days of the year
  nypd_sum<-nypd_complaint %>%
    mutate(julian=as.numeric(strftime(cmplnt_fr_dt, format = "%j"))) %>%
    mutate(year=as.numeric(strftime(cmplnt_fr_dt, format = "%Y"))) %>%
    filter(julian<20) %>% 
    group_by(ofns_desc,year) %>%
    summarize(total.count = n())  %>%
    ungroup(ofns_desc,year) %>%
    mutate(bail.reform=0)
 
  # add 2020 data 
  total.count <- c(10,75,802,944,666) 
  year<-c(2020,2020,2020,2020,2020)
  ofns_desc<- c("MURDER","RAPE","ROBBERY","FELONY ASSAULT","BURGLARY") 
  bail.reform<-c(1,1,1,1,1)
  
  crime2020ytd<-data.frame(ofns_desc,total.count,year,bail.reform)
  
  nyped_complaints_19days <- rbind(nypd_sum, crime2020ytd)
  
  # overall correlation 
  cor(nyped_complaints_19days$total.count, nyped_complaints_19days$bail.reform,method="pearson")  
  
  # robbery correlation
  ROBBERY<-nyped_complaints_19days %>%
    filter(ofns_desc=="ROBBERY")
  
  cor(ROBBERY$total.count, ROBBERY$bail.reform,method="pearson")  
  
  # burglary correlation
  BURGLARY<-nyped_complaints_19days %>%
    filter(ofns_desc=="BURGLARY")
  
  cor(BURGLARY$total.count, BURGLARY$bail.reform,method="pearson")  
  
 
