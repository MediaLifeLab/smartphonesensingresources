#### data simulation #### 


#### power analysis ####
install.packages("pwr")
require(pwr)
# there are different types of functions depending on the type of analysis you want to run a power analysis for, e.g. pwr.r.test() for a correlation, pwr.f2.test() for a general linear model, pwr.t.test() for a one-sample paired t-test, see full documentation of package for more details: https://cran.r-project.org/web/packages/pwr/index.html
# leave the parameter you want to determine blank
# r=size of your r, n=sample size, sig.level=sig level; usually .05, power=power size

# power analysis for a correlation
# compute necessary n for different sizes of power and effect
pwr.r.test(r=.1, sig.level = .05, power = .8)
pwr.r.test(r=.3, sig.level = .05, power = .8)
pwr.r.test(r=.5, sig.level = .05, power = .8)

# Regression, e.g. n for partial R with 3 total predictors in model
pwr.f2.test(u=3,f2=.02,power=.8,sig.level=.05)

#### pre-process ####
# download the data
#download.file(url="http://studentlife.cs.dartmouth.edu/dataset/dataset.tar.bz2", destfile="~/Desktop/studentlifedataset.tar.bz2")

#library(R.utils)
#bunzip2("studentlifedataset.tar.bz2", "studentlifedataset")

# set working directory
setwd("~/Desktop/dataset/dataset/sensing/gps")

# create a new folder
#dir.create("studentlifedataset")

# unpack the downloaded files there
#untar("studentlifedataset.tar.bz2", files="studentlifedataset") 

# view files
#list.files("studentlifedataset")

# load data
gps <- read.csv("gps_u00.csv")

# remove moving, low accuracy, speed>0(?)

# convert to one file for all users
setwd("~/Google Drive/0 PhD/1.3 Stanford Sep 2017/Chapter w Clemens/dataset/dataset/sensing/gps")

file_list <- list.files()
dataset <- NA
for (file in file_list){
  temp_dataset <-read.csv(file)
  dataset<-rbind(dataset, temp_dataset)
  rm(temp_dataset)}


#### convert to wide format #### 
# (userids rows and dates columns)

#### data cleaning ####
# check for outliers
# remove invalid data with less than 15hrs sensor data

#### descriptive stats #### 
# distrubtion stats for between and within person
# histograms to visualise distributions

#### variance at between and within person level ####

#### modeling (cross-validation) ####

#### enriching with web API ####

# FOURSQUARE
# Go to https://developer.foursquare.com and create an account
# create your app, now your 'CLIENT_ID' and 'CLIENT_SECRET' are being displayed
# CLIENT_ID: XIBNVOYCXYGAQTCLYXIF215AYLY5DJE0YGJNOBYGMBO5FC01 #(Sandrine)
# CLIENT_SECRET: PNTTB4IFO1H2KKPMVLDGY25IJM1FNOEKWEBI4UQF40DHUQPE #(Sandrine)

clientid <- "XIBNVOYCXYGAQTCLYXIF215AYLY5DJE0YGJNOBYGMBO5FC01" # insert your CLIENT_ID (here Sandrine's)
clientsecret <- "PNTTB4IFO1H2KKPMVLDGY25IJM1FNOEKWEBI4UQF40DHUQPE" # insert your CLIENT_SECRET (here Sandrine's)
ll <- "52.204444,0.138385" #(longitue latitude you are interested in (Norfolk St bakery), can loop through)
date <- "20171119" #date you want to use

url <- paste("https://api.foursquare.com/v2/venues/search?ll=",ll,"&client_id=",clientid,"&client_secret=",clientsecret,"&v=",date,sep="")

library(httr)
library(jsonlite)
library(lubridate)
library(rjson)
info <- fromJSON(paste(url))

# find shortest distance
info$response$venues$location$distance

info$response$venues$categories[[1]]$name # get type, e.g. bakery
info$response$venues$stats$checkinsCount[[1]] # get number of checkins
info$response$venues$stats$usersCount[[1]] # get number of users
info$response$venues$location$formattedAddress[[1]] # get address
info$response$venues$name[[1]] # get name
# location$formattedAddress, name



library("RCurl", "RJSONIO")

foursquare<-function(x,y,z){
  url<-paste("https://api.foursquare.com/v2/venues/search?ll=",ll,"&client_id=",clientid,"&client_secret=",clientsecret,"&v=",date,sep="")
  data<-fromJSON(url)
  categorytype=""
  numberofcheckins=""
  numberofusers=""
  for(n in 1:length(data$response$venues)) {
    categorytype[n] = data$response$venues$categories[[1]]$name
    numberofcheckins[n] = data$response$venues$stats$checkinsCount[[1]]
    numberofusers[n] = data$response$venues$stats$usersCount[[1]]
    scraped<-as.data.frame(cbind(locationname, lat, long, zip, herenowcount, likes))
  }
  scraped$dateretrieved=date
  return(scraped)
}
