####################################################
# Final script - Climate Change Opinion Prediction
# Author: Michael Beller
# R capstone individual project
####################################################

##########################################
# Load required packages and libraries
##########################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readr)
library(httr)  #Because readxl functions don't accept URLs
library(readxl)
library(caret)
library(Rborist)

##########################################
# Load raw external data files
##########################################

#All data sources can be found in this github repository
mb_gitpath <- "https://github.com/mbeller2016/Climate-Change-Opinion-Prediction/raw/main/Data/"

#Get the geocode file (cross-reference county/state and FIPS code) from Census Bureau
#Note: apparently can't use URLs with the readxl functions, so had to use the 
# httr library and write_disk to get these transferred over, per stackoverflow
GET(paste(mb_gitpath,"all-geocodes-v2019.xlsx",sep=""),
    write_disk(gfile <- tempfile(fileext = ".xlsx")))
#First 4 lines are title information, so skip them
GEOCODE_file <- read_xlsx(path=gfile, skip=4)

#Load the state name/abbreviation file
STABBR_file <- read_csv(paste(mb_gitpath,"csvData.csv",sep=""))

#Load climate change opinion data from Yale Climate Change Communication Project
YCOM_file <- read_csv(paste(mb_gitpath,"YCOM_2020_Data.csv",sep=""))

#Load population-related data from US Census Bureau, at
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
CEN_file <- read_csv(paste(mb_gitpath,"co-est2019-alldata.csv",sep=""))

#Load economic and income data from BEA at
#https://apps.bea.gov/regional/downloadzip.cfm
GDP_file <- read_csv(paste(mb_gitpath,"CAGDP1__ALL_AREAS_2001_2019.csv",sep=""))
INC_file <- read_csv(paste(mb_gitpath,"CAINC1__ALL_AREAS_1969_2019.csv",sep=""))

#Load coastline counties list, using the same httr workaround
GET(paste(mb_gitpath,"coastline-counties-list.xlsx",sep=""),
    write_disk(ccfile <- tempfile(fileext = ".xlsx")))
#First 3 lines are title information, so skip them
COAST_file <- read_xlsx(path=ccfile, skip=3)

#Load climate data from ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
#State codes in county-readme.txt
#Precip: climdiv-pcpncy-v1.0.0-20201205
#Max temp: climdiv-tmaxcy-v1.0.0-20201205
#Min temp: climdiv-tmincy-v1.0.0-20201205
#Temperature: climdiv-tmpccy-v1.0.0-20201205
#These files have no delimiters, but instead come with documentation outlining how each
#column should be interpreted.  Thus, they are read in with the read_lines function, then
#parsed.
CLIMREADME_file <- read_lines(paste(mb_gitpath,"county-readme.txt",sep=""))
TEMP_file <- read_lines(paste(mb_gitpath,"climdiv-tmpccy-v1.0.0-20201205",sep=""))
TMAX_file <- read_lines(paste(mb_gitpath,"climdiv-tmaxcy-v1.0.0-20201205",sep=""))
TMIN_file <- read_lines(paste(mb_gitpath,"climdiv-tmincy-v1.0.0-20201205",sep=""))
PRCP_file <- read_lines(paste(mb_gitpath,"climdiv-pcpncy-v1.0.0-20201205",sep=""))

##########################################
# Preprocess geocode file
##########################################

#Remove anything that isn't a state (Summary Level 040) or a county (Summary Level 050)
#Also remove District of Columbia (State FIPS 11) and Puerto Rico (State FIPS 72)
geocodes <- GEOCODE_file %>% 
  filter(`Summary Level`=="040" | `Summary Level`=="050") %>%
  filter(`State Code (FIPS)` != "72" &
           `State Code (FIPS)` != "11") %>%
  rename(level = `Summary Level`,
         state = `State Code (FIPS)`,
         county = `County Code (FIPS)`,
         name = `Area Name (including legal/statistical area description)`) %>%
  select(level, state, county, name) %>%
  arrange(state, level, county)

#Extract state codes and names, convert to all upper case
geostate <- geocodes %>%
  filter(level=="040") %>%
  select(state, name) %>%
  rename(sname = name) %>%
  mutate(sname = toupper(sname))

#Some data files only have state abbreviations -- add them to geostate
STABBR_file <- STABBR_file %>%
  mutate(sname = toupper(State))
geostate <- inner_join(geostate,STABBR_file,by="sname")

#Add state name to county record, convert
geocodes <- geocodes %>%
  filter(level=="050") %>%
  rename(cname = name) %>%
  mutate(cname = toupper(cname),
         cfips = paste(state,county,sep="")) %>%
  select(cfips, state, county, cname)
geocodes <- inner_join(geocodes,geostate,by="state") %>%
  select(-State,-Abbrev)

#Begin master county file
county_master <- geocodes

##########################################
# Preprocess coastal counties file
##########################################

#There are weird extra spaces and tabs in the column names, so rename them
colnames(COAST_file) <- c("cfips","state","county","county_name","state_name",
                          "coast_region","v2016")
county_coastal <- COAST_file %>%
  select(cfips,coast_region)

#Add a coastal yes=1,no=0 column to the county master file
county_master <- left_join(county_master,county_coastal,by="cfips")
county_master <- county_master %>%
  mutate(coast = ifelse(is.na(coast_region),0,1)) %>%
  select(-coast_region)

rm(county_coastal)

##########################################
# Preprocess census file
##########################################

county_census <- CEN_file %>%
  filter(SUMLEV == "050") %>%
  mutate(cfips = paste(STATE,COUNTY,sep=""),
         pop2019 = POPESTIMATE2019,
         popchgnat = NATURALINC2010 + NATURALINC2011 +
           NATURALINC2012 + NATURALINC2013 + NATURALINC2014 +
           NATURALINC2015 + NATURALINC2016 + NATURALINC2017 +
           NATURALINC2018 + NATURALINC2019,
         popchgiimm = INTERNATIONALMIG2010 + INTERNATIONALMIG2011 +
           INTERNATIONALMIG2012 + INTERNATIONALMIG2013 +
           INTERNATIONALMIG2014 + INTERNATIONALMIG2015 +
           INTERNATIONALMIG2016 + INTERNATIONALMIG2017 +
           INTERNATIONALMIG2018 + INTERNATIONALMIG2019,
         popchgdimm = DOMESTICMIG2010 + DOMESTICMIG2011 +
           DOMESTICMIG2012 + DOMESTICMIG2013 + DOMESTICMIG2014 +
           DOMESTICMIG2015 + DOMESTICMIG2016 + DOMESTICMIG2017 +
           DOMESTICMIG2018 + DOMESTICMIG2019) %>%
  select(cfips,pop2019,popchgnat,popchgiimm,popchgdimm)

#Add population data to the county master file.
county_master <- inner_join(county_master,county_census,by="cfips")

##########################################
# Preprocess GDP file
##########################################

#Need to filter out the non-county data (FIPS ends in "000")
#Need to only include lines from current GDP (line code = 3)
#Filter out some old counties with text <NA>s
#Exclude DC
county_gdp <- GDP_file %>%
  rename(cfips = GeoFIPS,
         cname = GeoName,
         g2019 = `2019`,
         g2009 = `2009`) %>%
  filter(substr(cfips,3,5) != "000" &
           LineCode == 3 &
           substr(g2019,1,1) != "(" &
           substr(cfips,1,2) != "11") %>%
  mutate(gdp2019 = as.numeric(g2019),
         gdp2009 = as.numeric(g2009),
         gdpchg = gdp2019 - gdp2009) %>%
  select(cfips,cname,gdp2019,gdpchg)

c2 <- anti_join(county_gdp,county_master,by="cfips")
c3 <- anti_join(county_master,county_gdp,by="cfips")

#The issue is with 1 2-county combo in Hawaii, and 23 county/independent city groupings
#in Virginia.  How to handle this?  Good question.  Climate data appears to be by the
#independent city, not combo, so maybe assign gdp values to both

#Process c2 records, split into components, get new cfips, then add them to county_gdp
newrecs <- data.frame(cname="Initial",gdp2019=-1,gdpchg=-1)
for (i in 1:nrow(c2)) {
  #Strip off last 5 characters from county name
  Gname <- substr(c2$cname[i],1,nchar(c2$cname[i])-5)
  #Combinations of 3 use a comma.  Replace it with "+"
  Gname <- str_replace(Gname,","," +")
  Gnames <- str_split(Gname," \\+ ",simplify=TRUE)
  #Create new records
  for (y in 1:dim(Gnames)[2]) {
    # Make new name upper case
    newname <- toupper(Gnames[,y])
    # If not Virginia, add "COUNTY" to end
    if (substr(c2$cfips[i],1,2) != "51") {
      newname <- paste(newname,"COUNTY",sep=" ")
    } else {
      #If Virginia, add "COUNTY" if first one, otherwise add "CITY"
      newname <- ifelse(y == 1,
                        paste(newname,"COUNTY",sep=" "),
                        paste(newname,"CITY",sep= " "))
    }
    newrec <- data.frame(cname = newname,
                         gdp2019 = c2$gdp2019[i],
                         gdpchg = c2$gdpchg[i])
    newrecs <- bind_rows(newrecs,newrec)
  }
}
newrecs <- newrecs[-1,]

#Correct for Fairfax City City
newrecs$cname[newrecs$cname=="FAIRFAX CITY CITY"] <- "FAIRFAX CITY"
#Get correct FIPS code for new records
newFIPS <- inner_join(newrecs,geocodes,by="cname")
#Filter for only VA and HI ones, select correct columns
newFIPS <- newFIPS %>%
  filter(state=="51" | state=="15") %>%
  select(cfips,cname,gdp2019,gdpchg)

#Make sure they are all there
c4 <- anti_join(newrecs,newFIPS,by="cname")
nrow(c4)

#Add new split records to county GDP table
county_gdp <- bind_rows(county_gdp,newFIPS)

#Check to see if any don't match
c5 <- anti_join(county_gdp,county_master,by="cfips")
nrow(c5)

#Update county master with GDP data
county_gdp <- county_gdp %>% select(-cname)
county_master <- inner_join(county_master,county_gdp,by="cfips")

#All still there

##########################################
# Preprocess Income file
##########################################

#Need to filter out the non-county data (FIPS ends in "000")
#Need to only include lines with per capita income (line code = 3)
#Filter out some old counties with text <NA>s
#Exclude DC
county_inc <- INC_file %>%
  rename(cfips = GeoFIPS,
         cname = GeoName,
         i2019 = `2019`,
         i2009 = `2009`) %>%
  filter(substr(cfips,3,5) != "000" &
           LineCode == 3 &
           substr(i2019,1,1) != "(" &
           substr(cfips,1,2) != "11") %>%
  mutate(inc2019 = as.numeric(i2019),
         inc2009 = as.numeric(i2009),
         incchg = inc2019 - inc2009) %>%
  select(cfips,cname,inc2019,incchg)

i2 <- anti_join(county_inc,county_master,by="cfips")
i3 <- anti_join(county_master,county_inc,by="cfips")

#The issue is with 1 2-county combo in Hawaii, and 23 county/independent city groupings
#in Virginia.  How to handle this?  Good question.  Climate data appears to be by the
#independent city, not combo, so maybe assign gdp values to both

#Process i2 records, split into components, get new cfips, then add them to county_inc
newrecs <- data.frame(cname="Initial",inc2019=-1,incchg=-1)
for (i in 1:nrow(i2)) {
  #Strip off last 5 characters from county name
  Gname <- substr(i2$cname[i],1,nchar(i2$cname[i])-5)
  #Combinations of 3 use a comma.  Replace it with "+"
  Gname <- str_replace(Gname,","," +")
  Gnames <- str_split(Gname," \\+ ",simplify=TRUE)
  #Create new records
  for (y in 1:dim(Gnames)[2]) {
    # Make new name upper case
    newname <- toupper(Gnames[,y])
    # If not Virginia, add "COUNTY" to end
    if (substr(i2$cfips[i],1,2) != "51") {
      newname <- paste(newname,"COUNTY",sep=" ")
    } else {
      #If Virginia, add "COUNTY" if first one, otherwise add "CITY"
      newname <- ifelse(y == 1,
                        paste(newname,"COUNTY",sep=" "),
                        paste(newname,"CITY",sep= " "))
    }
    newrec <- data.frame(cname = newname,
                         inc2019 = i2$inc2019[i],
                         incchg = i2$incchg[i])
    newrecs <- bind_rows(newrecs,newrec)
  }
}
newrecs <- newrecs[-1,]

#Correct for Fairfax City City
newrecs$cname[newrecs$cname=="FAIRFAX CITY CITY"] <- "FAIRFAX CITY"
#Get correct FIPS code for new records
newFIPS <- inner_join(newrecs,geocodes,by="cname")
#Filter for only VA and HI ones, select correct columns
newFIPS <- newFIPS %>%
  filter(state=="51" | state=="15") %>%
  select(cfips,cname,inc2019,incchg)

#Make sure they are all there
i4 <- anti_join(newrecs,newFIPS,by="cname")
nrow(i4)

#Add new split records to county GDP table
county_inc <- bind_rows(county_inc,newFIPS)

#Check to see if any don't match
i5 <- anti_join(county_inc,county_master,by="cfips")
nrow(i5)

#Update county master with GDP data
county_inc <- county_inc %>% select(-cname)
county_master <- inner_join(county_master,county_inc,by="cfips")

#All still there

##########################################
# Convert climate files to data frames
##########################################

#Convert text files to data frames
convert_climate_file <- function (climfile) {
  statecode <- str_sub(climfile,1,2)
  countycode <- str_sub(climfile,3,5)
  elemcode <- str_sub(climfile,6,7)
  year <- str_sub(climfile,8,11)
  janval <- str_sub(climfile,12,18)
  febval <- str_sub(climfile,19,25)
  marval <- str_sub(climfile,26,32)
  aprval <- str_sub(climfile,33,39)
  mayval <- str_sub(climfile,40,46)
  junval <- str_sub(climfile,47,53)
  julval <- str_sub(climfile,54,60)
  augval <- str_sub(climfile,61,67)
  sepval <- str_sub(climfile,68,74)
  octval <- str_sub(climfile,75,81)
  novval <- str_sub(climfile,82,88)
  decval <- str_sub(climfile,89,95)
  clim_tab <- data.frame(statecode = statecode,
                         countycode = countycode,
                         elemcode = elemcode,
                         year = year,
                         janval = janval,
                         febval = febval,
                         marval = marval,
                         aprval = aprval,
                         mayval = mayval,
                         junval = junval,
                         julval = julval,
                         augval = augval,
                         sepval = sepval,
                         octval = octval,
                         novval = novval,
                         decval = decval)
  clim_tab
}

county_temp <- convert_climate_file(TEMP_file)
county_tmax <- convert_climate_file(TMAX_file)
county_tmin <- convert_climate_file(TMIN_file)
county_prcp <- convert_climate_file(PRCP_file)

#Readme file contains the state codes used -- read it in and build a table so we can get
#the correct FIPS codes

#We only care about lines 74-100
CLIMREADME_file <- CLIMREADME_file[74:100]

#Get the state codes out of their two columns and into a workable format
cs1 <- str_sub(CLIMREADME_file,30,56)
climstates <- data.frame(statecode = str_sub(cs1,1,2),
                         sname = toupper(trimws(str_sub(cs1,4,20))))
cs2 <- str_sub(CLIMREADME_file,57,80)
climstates2 <- data.frame(statecode = str_sub(cs2,1,2),
                          sname = toupper(trimws(str_sub(cs2,4,20))))
climstates <- bind_rows(climstates,climstates2)

#Remove blank rows
climstates <- climstates[!apply(climstates=="",1,all),]

#Add record for Alaska (state code 50 in climate data)
climalaska <- data.frame(statecode="50", sname="ALASKA")
climstates <- bind_rows(climstates,climalaska)

#Cross-reference with geostate to get the correct state code
climstates <- inner_join(climstates,geostate,by="sname")

##########################################
# Convert climate temperature file
##########################################

#Need to filter for the correct years, define values by season, get the fips codes
#Which years?
#Spring: March of year - May of year
#Summer: June of year -- August of year
#Fall: Sep of year -- Nove of year
#Winter: Dec of year -- Feb of next year

county_temp <- county_temp %>%
  filter(year >= "1980") %>%
  mutate(year = as.numeric(year),
         winter23temp = as.numeric(janval) + as.numeric(febval),
         springtemp = ((as.numeric(marval) + as.numeric(aprval) +
                          as.numeric(mayval)) / 3),
         summertemp = ((as.numeric(junval) + as.numeric(julval) +
                          as.numeric(augval)) / 3),
         falltemp = ((as.numeric(sepval) + as.numeric(octval) +
                        as.numeric(novval)) / 3  ),
         winter1temp = as.numeric(decval))

#Get correct state code
county_temp <- inner_join(county_temp,climstates,by="statecode")

#Format the cfips identifier
county_temp <- county_temp %>%
  mutate(cfips = paste(state,countycode,sep="")) %>%
  select(cfips, year, winter23temp, springtemp,
         summertemp, falltemp, winter1temp)

#Fix the "winter crossing year" problem
#Add "winter year" which is the year the Jan and Feb values should be recorded in
county_temp <- county_temp %>%
  mutate(wintyear = year-1)
#Create a separate file showing Jan/Feb by winter year
county_temp_janfeb <- county_temp %>%
  filter(wintyear >= 1980) %>%
  select(-year) %>%
  rename(janfebval = winter23temp,
         year = wintyear) %>%
  select(cfips,year,janfebval)
#Link main file to Jan/Feb by winter year to get the right values.
county_temp <- left_join(county_temp,county_temp_janfeb,by=c("cfips","year"))
#Now calculate winter values
county_temp <- county_temp %>%
  mutate(wintertemp = (janfebval + winter1temp)/3) %>%
  select(cfips, year, springtemp, summertemp, falltemp, wintertemp)

#Will look at current temps, change since 1980, change since 1989, change since 1999,
#change since 2009
county_temp1980 <- county_temp %>%
  filter(year == 1980) %>%
  rename(spring1980 = springtemp,
         summer1980 = summertemp,
         fall1980 = falltemp,
         winter1980 = wintertemp)
county_temp1989 <- county_temp %>%
  filter(year == 1989) %>%
  rename(spring1989 = springtemp,
         summer1989 = summertemp,
         fall1989 = falltemp,
         winter1989 = wintertemp)
county_temp1999 <- county_temp %>%
  filter(year == 1999) %>%
  rename(spring1999 = springtemp,
         summer1999 = summertemp,
         fall1999 = falltemp,
         winter1999 = wintertemp)
county_temp2009 <- county_temp %>%
  filter(year == 2009) %>%
  rename(spring2009 = springtemp,
         summer2009 = summertemp,
         fall2009 = falltemp,
         winter2009 = wintertemp)
county_temp2019 <- county_temp %>%
  filter(year == 2019) %>%
  rename(spring2019 = springtemp,
         summer2019 = summertemp,
         fall2019 = falltemp,
         winter2019 = wintertemp)

county_temp_summ <- inner_join(county_temp2019,county_temp2009,by="cfips")
county_temp_summ <- inner_join(county_temp_summ,county_temp1999,by="cfips")
county_temp_summ <- inner_join(county_temp_summ,county_temp1989,by="cfips")
county_temp_summ <- inner_join(county_temp_summ,county_temp1980,by="cfips")
county_temp_summ <- county_temp_summ %>%
  mutate(sp2019temp = spring2019,
         su2019temp = summer2019,
         fa2019temp = fall2019,
         wi2019temp = winter2019,
         sptempd2009 = spring2019 - spring2009,
         sutempd2009 = summer2019 - summer2009,
         fatempd2009 = fall2019 - fall2009,
         witempd2009 = winter2019 - winter2009,
         sptempd1999 = spring2019 - spring1999,
         sutempd1999 = summer2019 - summer1999,
         fatempd1999 = fall2019 - fall1999,
         witempd1999 = winter2019 - winter1999,
         sptempd1989 = spring2019 - spring1989,
         sutempd1989 = summer2019 - summer1989,
         fatempd1989 = fall2019 - fall1989,
         witempd1989 = winter2019 - winter1989,
         sptempd1980 = spring2019 - spring1980,
         sutempd1980 = summer2019 - summer1980,
         fatempd1980 = fall2019 - fall1980,
         witempd1980 = winter2019 - winter1980) %>%
  select(cfips,
         sp2019temp,
         su2019temp,
         fa2019temp,
         wi2019temp,
         sptempd2009,
         sutempd2009,
         fatempd2009,
         witempd2009,
         sptempd1999,
         sutempd1999,
         fatempd1999,
         witempd1999,
         sptempd1989,
         sutempd1989,
         fatempd1989,
         witempd1989,
         sptempd1980,
         sutempd1980,
         fatempd1980,
         witempd1980)

##########################################
# Convert climate max temperature file
##########################################

#Need to filter for the correct years, define values by season, get the fips codes
#Which years?
#Spring: March of year - May of year
#Summer: June of year -- August of year
#Fall: Sep of year -- Nove of year
#Winter: Dec of year -- Feb of next year

county_tmax <- county_tmax %>%
  filter(year >= "1980") %>%
  mutate(year = as.numeric(year),
         winter23tmax = as.numeric(janval) + as.numeric(febval),
         springtmax = ((as.numeric(marval) + as.numeric(aprval) +
                          as.numeric(mayval)) / 3),
         summertmax = ((as.numeric(junval) + as.numeric(julval) +
                          as.numeric(augval)) / 3),
         falltmax = ((as.numeric(sepval) + as.numeric(octval) +
                        as.numeric(novval)) / 3  ),
         winter1tmax = as.numeric(decval))

#Get correct state code
county_tmax <- inner_join(county_tmax,climstates,by="statecode")

#Format the cfips identifier
county_tmax <- county_tmax %>%
  mutate(cfips = paste(state,countycode,sep="")) %>%
  select(cfips, year, winter23tmax, springtmax,
         summertmax, falltmax, winter1tmax)

#Fix the "winter crossing year" problem
#Add "winter year" which is the year the Jan and Feb values should be recorded in
county_tmax <- county_tmax %>%
  mutate(wintyear = year-1)
#Create a separate file showing Jan/Feb by winter year
county_tmax_janfeb <- county_tmax %>%
  filter(wintyear >= 1980) %>%
  select(-year) %>%
  rename(janfebval = winter23tmax,
         year = wintyear) %>%
  select(cfips,year,janfebval)
#Link main file to Jan/Feb by winter year to get the right values.
county_tmax <- left_join(county_tmax,county_tmax_janfeb,by=c("cfips","year"))
#Now calculate winter values
county_tmax <- county_tmax %>%
  mutate(wintertmax = (janfebval + winter1tmax)/3) %>%
  select(cfips, year, springtmax, summertmax, falltmax, wintertmax)

#Will look at current temps, change since 1980, change since 1989, change since 1999,
#change since 2009
county_tmax1980 <- county_tmax %>%
  filter(year == 1980) %>%
  rename(spring1980 = springtmax,
         summer1980 = summertmax,
         fall1980 = falltmax,
         winter1980 = wintertmax)
county_tmax1989 <- county_tmax %>%
  filter(year == 1989) %>%
  rename(spring1989 = springtmax,
         summer1989 = summertmax,
         fall1989 = falltmax,
         winter1989 = wintertmax)
county_tmax1999 <- county_tmax %>%
  filter(year == 1999) %>%
  rename(spring1999 = springtmax,
         summer1999 = summertmax,
         fall1999 = falltmax,
         winter1999 = wintertmax)
county_tmax2009 <- county_tmax %>%
  filter(year == 2009) %>%
  rename(spring2009 = springtmax,
         summer2009 = summertmax,
         fall2009 = falltmax,
         winter2009 = wintertmax)
county_tmax2019 <- county_tmax %>%
  filter(year == 2019) %>%
  rename(spring2019 = springtmax,
         summer2019 = summertmax,
         fall2019 = falltmax,
         winter2019 = wintertmax)

county_tmax_summ <- inner_join(county_tmax2019,county_tmax2009,by="cfips")
county_tmax_summ <- inner_join(county_tmax_summ,county_tmax1999,by="cfips")
county_tmax_summ <- inner_join(county_tmax_summ,county_tmax1989,by="cfips")
county_tmax_summ <- inner_join(county_tmax_summ,county_tmax1980,by="cfips")
county_tmax_summ <- county_tmax_summ %>%
  mutate(sp2019tmax = spring2019,
         su2019tmax = summer2019,
         fa2019tmax = fall2019,
         wi2019tmax = winter2019,
         sptmaxd2009 = spring2019 - spring2009,
         sutmaxd2009 = summer2019 - summer2009,
         fatmaxd2009 = fall2019 - fall2009,
         witmaxd2009 = winter2019 - winter2009,
         sptmaxd1999 = spring2019 - spring1999,
         sutmaxd1999 = summer2019 - summer1999,
         fatmaxd1999 = fall2019 - fall1999,
         witmaxd1999 = winter2019 - winter1999,
         sptmaxd1989 = spring2019 - spring1989,
         sutmaxd1989 = summer2019 - summer1989,
         fatmaxd1989 = fall2019 - fall1989,
         witmaxd1989 = winter2019 - winter1989,
         sptmaxd1980 = spring2019 - spring1980,
         sutmaxd1980 = summer2019 - summer1980,
         fatmaxd1980 = fall2019 - fall1980,
         witmaxd1980 = winter2019 - winter1980) %>%
  select(cfips,
         sp2019tmax,
         su2019tmax,
         fa2019tmax,
         wi2019tmax,
         sptmaxd2009,
         sutmaxd2009,
         fatmaxd2009,
         witmaxd2009,
         sptmaxd1999,
         sutmaxd1999,
         fatmaxd1999,
         witmaxd1999,
         sptmaxd1989,
         sutmaxd1989,
         fatmaxd1989,
         witmaxd1989,
         sptmaxd1980,
         sutmaxd1980,
         fatmaxd1980,
         witmaxd1980)
#For Hawaii, use state average

##########################################
# Convert climate min temperature file
##########################################

#Need to filter for the correct years, define values by season, get the fips codes
#Which years?
#Spring: March of year - May of year
#Summer: June of year -- August of year
#Fall: Sep of year -- Nove of year
#Winter: Dec of year -- Feb of next year

county_tmin <- county_tmin %>%
  filter(year >= "1980") %>%
  mutate(year = as.numeric(year),
         winter23tmin = as.numeric(janval) + as.numeric(febval),
         springtmin = ((as.numeric(marval) + as.numeric(aprval) +
                          as.numeric(mayval)) / 3),
         summertmin = ((as.numeric(junval) + as.numeric(julval) +
                          as.numeric(augval)) / 3),
         falltmin = ((as.numeric(sepval) + as.numeric(octval) +
                        as.numeric(novval)) / 3  ),
         winter1tmin = as.numeric(decval))

#Get correct state code
county_tmin <- inner_join(county_tmin,climstates,by="statecode")

#Format the cfips identifier
county_tmin <- county_tmin %>%
  mutate(cfips = paste(state,countycode,sep="")) %>%
  select(cfips, year, winter23tmin, springtmin,
         summertmin, falltmin, winter1tmin)

#Fix the "winter crossing year" problem
#Add "winter year" which is the year the Jan and Feb values should be recorded in
county_tmin <- county_tmin %>%
  mutate(wintyear = year-1)
#Create a separate file showing Jan/Feb by winter year
county_tmin_janfeb <- county_tmin %>%
  filter(wintyear >= 1980) %>%
  select(-year) %>%
  rename(janfebval = winter23tmin,
         year = wintyear) %>%
  select(cfips,year,janfebval)
#Link main file to Jan/Feb by winter year to get the right values.
county_tmin <- left_join(county_tmin,county_tmin_janfeb,by=c("cfips","year"))
#Now calculate winter values
county_tmin <- county_tmin %>%
  mutate(wintertmin = (janfebval + winter1tmin)/3) %>%
  select(cfips, year, springtmin, summertmin, falltmin, wintertmin)

#Will look at current temps, change since 1980, change since 1989, change since 1999,
#change since 2009
county_tmin1980 <- county_tmin %>%
  filter(year == 1980) %>%
  rename(spring1980 = springtmin,
         summer1980 = summertmin,
         fall1980 = falltmin,
         winter1980 = wintertmin)
county_tmin1989 <- county_tmin %>%
  filter(year == 1989) %>%
  rename(spring1989 = springtmin,
         summer1989 = summertmin,
         fall1989 = falltmin,
         winter1989 = wintertmin)
county_tmin1999 <- county_tmin %>%
  filter(year == 1999) %>%
  rename(spring1999 = springtmin,
         summer1999 = summertmin,
         fall1999 = falltmin,
         winter1999 = wintertmin)
county_tmin2009 <- county_tmin %>%
  filter(year == 2009) %>%
  rename(spring2009 = springtmin,
         summer2009 = summertmin,
         fall2009 = falltmin,
         winter2009 = wintertmin)
county_tmin2019 <- county_tmin %>%
  filter(year == 2019) %>%
  rename(spring2019 = springtmin,
         summer2019 = summertmin,
         fall2019 = falltmin,
         winter2019 = wintertmin)

county_tmin_summ <- inner_join(county_tmin2019,county_tmin2009,by="cfips")
county_tmin_summ <- inner_join(county_tmin_summ,county_tmin1999,by="cfips")
county_tmin_summ <- inner_join(county_tmin_summ,county_tmin1989,by="cfips")
county_tmin_summ <- inner_join(county_tmin_summ,county_tmin1980,by="cfips")
county_tmin_summ <- county_tmin_summ %>%
  mutate(sp2019tmin = spring2019,
         su2019tmin = summer2019,
         fa2019tmin = fall2019,
         wi2019tmin = winter2019,
         sptmind2009 = spring2019 - spring2009,
         sutmind2009 = summer2019 - summer2009,
         fatmind2009 = fall2019 - fall2009,
         witmind2009 = winter2019 - winter2009,
         sptmind1999 = spring2019 - spring1999,
         sutmind1999 = summer2019 - summer1999,
         fatmind1999 = fall2019 - fall1999,
         witmind1999 = winter2019 - winter1999,
         sptmind1989 = spring2019 - spring1989,
         sutmind1989 = summer2019 - summer1989,
         fatmind1989 = fall2019 - fall1989,
         witmind1989 = winter2019 - winter1989,
         sptmind1980 = spring2019 - spring1980,
         sutmind1980 = summer2019 - summer1980,
         fatmind1980 = fall2019 - fall1980,
         witmind1980 = winter2019 - winter1980) %>%
  select(cfips,
         sp2019tmin,
         su2019tmin,
         fa2019tmin,
         wi2019tmin,
         sptmind2009,
         sutmind2009,
         fatmind2009,
         witmind2009,
         sptmind1999,
         sutmind1999,
         fatmind1999,
         witmind1999,
         sptmind1989,
         sutmind1989,
         fatmind1989,
         witmind1989,
         sptmind1980,
         sutmind1980,
         fatmind1980,
         witmind1980)

##########################################
# Convert climate precipitation file
##########################################

#Need to filter for the correct years, define values by season, get the fips codes
#Which years?
#Spring: March of year - May of year
#Summer: June of year -- August of year
#Fall: Sep of year -- Nove of year
#Winter: Dec of year -- Feb of next year

county_prcp <- county_prcp %>%
  filter(year >= "1980") %>%
  mutate(year = as.numeric(year),
         winter23prcp = as.numeric(janval) + as.numeric(febval),
         springprcp = ((as.numeric(marval) + as.numeric(aprval) +
                          as.numeric(mayval)) / 3),
         summerprcp = ((as.numeric(junval) + as.numeric(julval) +
                          as.numeric(augval)) / 3),
         fallprcp = ((as.numeric(sepval) + as.numeric(octval) +
                        as.numeric(novval)) / 3  ),
         winter1prcp = as.numeric(decval))

#Get correct state code
county_prcp <- inner_join(county_prcp,climstates,by="statecode")

#Format the cfips identifier
county_prcp <- county_prcp %>%
  mutate(cfips = paste(state,countycode,sep="")) %>%
  select(cfips, year, winter23prcp, springprcp,
         summerprcp, fallprcp, winter1prcp)

#Fix the "winter crossing year" problem
#Add "winter year" which is the year the Jan and Feb values should be recorded in
county_prcp <- county_prcp %>%
  mutate(wintyear = year-1)
#Create a separate file showing Jan/Feb by winter year
county_prcp_janfeb <- county_prcp %>%
  filter(wintyear >= 1980) %>%
  select(-year) %>%
  rename(janfebval = winter23prcp,
         year = wintyear) %>%
  select(cfips,year,janfebval)
#Link main file to Jan/Feb by winter year to get the right values.
county_prcp <- left_join(county_prcp,county_prcp_janfeb,by=c("cfips","year"))
#Now calculate winter values
county_prcp <- county_prcp %>%
  mutate(winterprcp = (janfebval + winter1prcp)/3) %>%
  select(cfips, year, springprcp, summerprcp, fallprcp, winterprcp)

#Will look at current temps, change since 1980, change since 1989, change since 1999,
#change since 2009
county_prcp1980 <- county_prcp %>%
  filter(year == 1980) %>%
  rename(spring1980 = springprcp,
         summer1980 = summerprcp,
         fall1980 = fallprcp,
         winter1980 = winterprcp)
county_prcp1989 <- county_prcp %>%
  filter(year == 1989) %>%
  rename(spring1989 = springprcp,
         summer1989 = summerprcp,
         fall1989 = fallprcp,
         winter1989 = winterprcp)
county_prcp1999 <- county_prcp %>%
  filter(year == 1999) %>%
  rename(spring1999 = springprcp,
         summer1999 = summerprcp,
         fall1999 = fallprcp,
         winter1999 = winterprcp)
county_prcp2009 <- county_prcp %>%
  filter(year == 2009) %>%
  rename(spring2009 = springprcp,
         summer2009 = summerprcp,
         fall2009 = fallprcp,
         winter2009 = winterprcp)
county_prcp2019 <- county_prcp %>%
  filter(year == 2019) %>%
  rename(spring2019 = springprcp,
         summer2019 = summerprcp,
         fall2019 = fallprcp,
         winter2019 = winterprcp)

county_prcp_summ <- inner_join(county_prcp2019,county_prcp2009,by="cfips")
county_prcp_summ <- inner_join(county_prcp_summ,county_prcp1999,by="cfips")
county_prcp_summ <- inner_join(county_prcp_summ,county_prcp1989,by="cfips")
county_prcp_summ <- inner_join(county_prcp_summ,county_prcp1980,by="cfips")
county_prcp_summ <- county_prcp_summ %>%
  mutate(sp2019prcp = spring2019,
         su2019prcp = summer2019,
         fa2019prcp = fall2019,
         wi2019prcp = winter2019,
         spprcpd2009 = spring2019 - spring2009,
         suprcpd2009 = summer2019 - summer2009,
         faprcpd2009 = fall2019 - fall2009,
         wiprcpd2009 = winter2019 - winter2009,
         spprcpd1999 = spring2019 - spring1999,
         suprcpd1999 = summer2019 - summer1999,
         faprcpd1999 = fall2019 - fall1999,
         wiprcpd1999 = winter2019 - winter1999,
         spprcpd1989 = spring2019 - spring1989,
         suprcpd1989 = summer2019 - summer1989,
         faprcpd1989 = fall2019 - fall1989,
         wiprcpd1989 = winter2019 - winter1989,
         spprcpd1980 = spring2019 - spring1980,
         suprcpd1980 = summer2019 - summer1980,
         faprcpd1980 = fall2019 - fall1980,
         wiprcpd1980 = winter2019 - winter1980) %>%
  select(cfips,
         sp2019prcp,
         su2019prcp,
         fa2019prcp,
         wi2019prcp,
         spprcpd2009,
         suprcpd2009,
         faprcpd2009,
         wiprcpd2009,
         spprcpd1999,
         suprcpd1999,
         faprcpd1999,
         wiprcpd1999,
         spprcpd1989,
         suprcpd1989,
         faprcpd1989,
         wiprcpd1989,
         spprcpd1980,
         suprcpd1980,
         faprcpd1980,
         wiprcpd1980)

##########################################
# Add climate data to county master
##########################################
county_master <- inner_join(county_master,county_temp_summ,by="cfips")
county_master <- inner_join(county_master,county_tmax_summ,by="cfips")
county_master <- inner_join(county_master,county_tmin_summ,by="cfips")
county_master <- inner_join(county_master,county_prcp_summ,by="cfips")

#This eliminates the 6 county equivalents that don't have climate data
#All five Hawaii counties and the independent city of Lexington VA

######################################################
# Preprocess Yale Climate Communication opinion file
######################################################

#The Yale file includes opinion data at state, county, congressional district, and metro
#area levels.  We only want the county ones, so filter the others out.  Also filter out
#the District of Columbia.  Also, the GEOID field is the FIPS code, but stored as numeric.
#Convert to character, and adjust for missing leading zeros.
clim_opin <- YCOM_file %>%
  filter(GeoType=="County" & GEOID != 11001) %>%
  mutate(cfips = as.character(GEOID))

clim_opin$cfips <- ifelse(nchar(clim_opin$cfips)==4,
                          paste("0",clim_opin$cfips,sep=""),
                          clim_opin$cfips)

#Select only the "Human Caused" support percentage
clim_opin <- clim_opin %>%
  select(cfips, human)

#Add to county master file
county_master <- inner_join(county_master,clim_opin,by="cfips")

######################################################
# Prep for Prediction
######################################################

#Create training/test/validation partitions
set.seed(321, sample.kind="Rounding")
county_val_index <- createDataPartition(
  y = county_master$human, times = 1, p = 0.1, list = FALSE)
human_model <- county_master[-county_val_index,]
human_val <- county_master[county_val_index,]
county_test_index <- createDataPartition(
  y = human_model$human, times = 1, p = 0.2, list = FALSE)
human_train <- human_model[-county_test_index,]
human_test <- human_model[county_test_index,]

#Define loss function (RMSE)
RMSE_f <- function(predicted,actual){
  RMSE <- sqrt(mean((actual - predicted)^2))
}

##############################
# Build random forest model
##############################

#random forest
set.seed(654, sample.kind="Rounding")
ranforh <- train(human ~ popchgnat + popchgiimm + coast + gdp2019 + incchg +
                   sp2019tmax + sutmind1980 + witmind2009 +
                   faprcpd1999 + wiprcpd1980, data=human_train, method = "rf",
                 importance = TRUE)

##############################
# Predict the validation set
##############################

#Human-caused global warming
final_human <- predict(ranforh,human_val)
final_human_RMSE <- RMSE_f(final_human,human_val$human)
print("Validation RMSE (Global warming is caused mostly by human activity):")
final_human_RMSE