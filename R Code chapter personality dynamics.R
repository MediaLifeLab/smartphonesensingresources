
# load the data: BIG5 states, BIG5 traits, conversation (sensed)
# [states are in JSON format]

# merge files into one, convert time stamps, create quality files (? -> check the microphone files. remove users?), bring into format for MLM

# plots within/between person variation of BIG5 states and conversation -> check out Penn State stuff

# run MLM predicting conversation from BIG5 states (?)

# EMA JSON (ask Gaby) -> figure out

# BIG 5 EMAs
# Load the package required to read JSON files.
library("rjson")
setwd("~/Google Drive/0 PhD/1.3 Stanford Sep 2017/Chapter w Clemens/dataset/dataset/EMA/response/Behavior")
# load data

d <- (fromJSON(file ="Behavior_u44.json")) # use this in apply to call in each object from list of file names

unique(unlist(sapply(d, names))) # unique variable names across file

f<-sapply(d, length)>2 # filter that removes objects with length less than 2 (empty responses)
d<-d[f] # apply filter
d1<-do.call(rbind, d) # replace rbind with uneven column call # rbind the list elements
d1[d1=="null"]<-NA # replace 'nulls' with NAs
head(d1)
#


#### load and merge the data #### 

# 1. Conversation

# set working directory
setwd("~/Google Drive/0 PhD/1.3 Stanford Sep 2017/Chapter w Clemens/dataset/dataset/sensing/conversation")
# load data and convert to one file for all users
list <- list.files()
conv.all <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in list){
  temp <- read.csv(i) 
  temp$userid <- substring(i, 14, 16) # add a column containing the userid (retrieved from the file name)
  temp <- temp[,c(ncol(temp),1:(ncol(temp)-1))] # put the userid column first
  conv.all<-rbind(conv.all, temp)
  rm(temp)}

# 2. Audio

# set working directory
setwd("~/Google Drive/0 PhD/1.3 Stanford Sep 2017/Chapter w Clemens/dataset/dataset/sensing/audio")
# load data and convert to one file for all users
list <- list.files()
audio <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in list){
  temp <- read.csv(i) 
  temp$userid <- substring(i, 7, 9) # add a column containing the userid (retrieved from the file name)
  temp <- temp[,c(ncol(temp),1:(ncol(temp)-1))] # put the userid column first
  audio<-rbind(audio, temp)
  rm(temp)}

list_df = lapply(list, get)

#### add variables #### 

# convert UNIX time stamp
library(anytime)
conv.all$start_time <- anytime(conv.all$start_timestamp, tz="EST") # time zone according to website
# split into date and time and hour
conv.all$date <- as.Date(conv.all$start_time) 
#conv.all$start_time.split <- format(conv.all$start_time,"%H:%M:%S")
#conv.all$start_hour.split <- format(conv.all$start_time,"%H")

# compute conversation duration
conv.all$duration_min <-  (conv.all$end_timestamp-conv.all$start_timestamp)/60

# create table to show whether users had any conversation data for each day
days <- unique(na.omit(conv.all$date))
users <- unique(na.omit(conv.all$userid))
df1 <- setNames(data.frame(matrix(ncol = length(days), nrow = length(users))), days)
df1 <- cbind(userid = users, df1)
# create loop: table with unique dates as columns, for each userid check whether date in list; if so enter 1, if not enter 0
for (i in 1:length(users)){
  for (j in 1:length(days)) {
    k <- j+1
    if (days[j] %in% unique(conv.all[conv.all$userid==users[i],]$date)==TRUE) {df1[i,k] <- 1} else {df1[i,k] <-0}}}
conv.all.datapresent <- df1
rm(df1,i,j,k,users)

# compute metrics

# aggregate and merge
sums <- as.data.frame(aggregate(duration_min ~ date + userid, data = conv.all, sum))
names(sums)[names(sums) == 'duration_min'] <- 'duration_sum'

means <- as.data.frame(aggregate(duration_min ~ date + userid, data = conv.all, mean))
names(means)[names(means) == 'duration_min'] <- 'duration_mean'

stddev <- as.data.frame(aggregate(duration_min ~ date + userid, data = conv.all, sd))
names(stddev)[names(stddev) == 'duration_min'] <- 'duration_sd'

freq <- as.data.frame(aggregate(duration_min ~ date + userid, data = conv.all, length))
names(freq)[names(freq) == 'duration_min'] <- 'duration_freq'

metrics <- merge(sums, means, by=c("userid", "date"))
metrics <- merge(metrics, stddev, by=c("date", "userid"))
metrics <- merge(metrics, freq, by=c("userid", "date"))
rm(freq, means, stddev, sums)

# audio data
library(anytime)
audio$time <- anytime(audio$timestamp, tz="EST") # time zone according to website
# split into date and time and hour
audio$date <- as.Date(audio$time) 

# check the unique minutes in each hour of each day?
audio$time.split <- format(audio$time,"%H:%M")

audiominutes <- sapply(unique(audio$date), function(day) { # loop over days
  sapply(unique(audio$userid), function(id) { # loop over userids
    sum(sapply(c(unique(audio$time.split)), function(minutes) {length(audio[audio$userid==id & audio$date==day,"time.split"]==minutes)})) # count the unique minutes wit GPS traces
  })
})
colnames(audiominutes) <- as.character(unique(audio$date)) # replaces generic column names with dates


# for each day: start at midnight, take chunk of four minutes, check whether there is any time inference, if so count 4 minutes/add 4mins to the counter, otherwise discard and take next chunk ?

# add the amount of time captured into the metrics file (can also filter out at this stage how much data is required)

# add in the zero days; add NA for days where not participated

# uid | date | hour | 15int | status:  0 / 1 | noise.fraction | conv.fract
# save as R object; compress=F
# in all files; run by user; 

