#Data Science Project
#Predicting Airline Arrival and Departure Delays
#data sets can be found at the link below:
#https://drive.google.com/drive/folders/1DlqE5DgZ22W4h7Ma_snJ2907otgsC_ZO?usp=sharing

#Exploratory data analysis

rm(list=ls())
complete.tr=read.csv(file.choose())
head(complete.tr)
tail(complete.tr)
names(complete.tr)
str(complete.tr)
dim(complete.tr)
library(rJava)
library(plyr)  #used for count function
library(MASS)


#Getting basic information about dataset
attach(complete.tr)

#A flight is delayed when an airline flight takes off and/or lands later
#than its scheduled time. The Federal Aviation Administration (FAA) 
#considers a flight to be delayed when it is 15 minutes later 
#than its scheduled time

departureDelayed = DEP_DELAY >= 15
arrivalDelayed = ARR_DELAY >= 15
bothDelayed = DEP_DELAY >= 15 & ARR_DELAY >= 15
length(complete.tr)
nrow(complete.tr)

#Percentage departure delay out of all flights
Percentage_dep_delay_flights=sum(departureDelayed)/nrow(complete.tr)
Percentage_dep_delay_flights

#Percentage arrival delay out of all flights
Percentage_arr_delay_flights=sum(arrivalDelayed)/nrow(complete.tr)
Percentage_arr_delay_flights

#Arrival delayed if departure was already delayed
Percentage_arr_delay_given_dep_delay=sum(bothDelayed)/sum(arrivalDelayed)
Percentage_arr_delay_given_dep_delay



#Which Airports are best or worst
#Finding best departure airport

##code
?subset
attach(complete.tr)
#Taking the subset of data containing columns 'ORIGIIN', 'DEP_DELAY'
#and 'ORIGIN_CITY_NAME' to find best airport for Departure
#We use subset fuction to perfom this task
#Also, we are considering only positive departure delays
dep<- subset(complete.tr, DEP_DELAY >= 0, 
select=c("ORIGIN","DEP_DELAY","ORIGIN_CITY_NAME"))
summary(dep)
dim(dep)
head(dep)
attach(dep)
#?paste
#combining city name with origin code
dep[,4]=paste(dep[,3],dep[,1],sep=",")
dep[,1]=dep[,4]
head(dep)
#aggregating airports based on mean of departure delay
dep_agg_mean=aggregate(dep,by=list(dep[,4]),FUN=mean)
head(dep_agg_mean)
tail(dep_agg_mean)
dim(dep_agg_mean)
dep_agg_mean=subset(dep_agg_mean,select=c(Group.1,DEP_DELAY))
head(dep_agg_mean)

#aggregating airports based on number(count) of departure delay
dep_agg_count=aggregate(dep,by=list(dep[,4]),FUN=length)
#dep_agg_count=data.frame(dep_agg_count)
dim(dep_agg_count)
head(dep_agg_count)
dep_agg_count=subset(dep_agg_count,select=c(Group.1,DEP_DELAY))
head(dep_agg_count)

#combining mean and count of departure delay based on airports
dep_agg=cbind(dep_agg_mean,dep_agg_count)
head(dep_agg)
dep_agg=dep_agg[,-3]
head(dep_agg)

#changing the column names
names(dep_agg) <- c("ORIGIN", "DEP_DELAY_MEAN","DEP_DELAY_COUNT")
head(dep_agg)
summary(dep_agg)
attach(dep_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 365
dep_delay= subset(dep_agg,DEP_DELAY_COUNT>threshold , 
select=c(ORIGIN,DEP_DELAY_MEAN,DEP_DELAY_COUNT))
head(dep_delay)
summary(dep_delay)
dim(dep_delay)
attach(dep_delay)
write.csv(dep_delay,'dep_delay.csv')

#Best departure airports
dep_delay_best= dep_delay[order(DEP_DELAY_MEAN),] 
head(dep_delay_best)
dep_delay_best_plot=dep_delay_best[1:10,]
View(dep_delay_best_plot)
attach(dep_delay_best_plot)
?barplot
barplot(DEP_DELAY_MEAN,horiz=FALSE,names.arg=ORIGIN,col='green',space=2.5,
main="Best Departure Airports",xlab="Origin Airport",
ylab="Average Departure Delay(minutes)")


#Finding worst departure airport
dep_delay_worst= dep_delay[order(-DEP_DELAY_MEAN),] 
head(dep_delay_worst)
dep_delay_worst_plot=dep_delay_worst[1:10,]
View(dep_delay_worst_plot)
attach(dep_delay_worst_plot)
barplot(DEP_DELAY_MEAN,horiz=FALSE,names.arg=ORIGIN,col='red',space=2.5,
main="Worst Departure Airports",xlab="Origin Airport",
ylab="Average Departure Delay(minutes)")

########################################################################
########################################################################
########################################################################
##Finding Best Destination Airport
arr<- subset(complete.tr, ARR_DELAY >= 0, 
select=c("DEST", "ARR_DELAY", "DEST_CITY_NAME"))
summary(arr)
dim(arr)
head(arr)
attach(arr)

#combining city name with destination code
arr[,4]=paste(arr[,3],arr[,1],sep=",")
arr[,1]=arr[,4]
head(arr)

#aggregating airports based on mean of arrival delay
arr_agg_mean=aggregate(arr,by=list(arr[,4]),FUN=mean)
head(arr_agg_mean)
tail(arr_agg_mean)
dim(arr_agg_mean)
arr_agg_mean=subset(arr_agg_mean,select=c(Group.1,ARR_DELAY))
head(arr_agg_mean)

#aggregating airports based on number(count) of arrival delay
arr_agg_count=aggregate(arr,by=list(arr[,4]),FUN=length)
dim(arr_agg_count)
head(arr_agg_count)
arr_agg_count=subset(arr_agg_count,select=c(Group.1,ARR_DELAY))
head(arr_agg_count)

#combining mean and count of arrival delay based on airports
arr_agg=cbind(arr_agg_mean,arr_agg_count)
head(arr_agg)
arr_agg=arr_agg[,-3]
head(arr_agg)


#changing the column names
names(arr_agg) <- c("DEST", "ARR_DELAY_MEAN","ARR_DELAY_COUNT")
head(arr_agg)
summary(arr_agg)
attach(arr_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 365
arr_delay= subset(arr_agg,ARR_DELAY_COUNT>threshold , 
select=c(DEST,ARR_DELAY_MEAN,ARR_DELAY_COUNT))
head(arr_delay)
summary(arr_delay)
dim(arr_delay)
attach(arr_delay)
write.csv(arr_delay,'arr_delay.csv')


#Best arrival airports
arr_delay_best= arr_delay[order(ARR_DELAY_MEAN),] 
head(arr_delay_best)
arr_delay_best_plot=arr_delay_best[1:10,]
View(arr_delay_best_plot)
attach(arr_delay_best_plot)
#?barplot
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=DEST,col='green',space=2.5,
main="Best Arrival Airports",xlab="Destination Airport",
ylab="Average Arrival Delay(minutes)")

#Finding worst arrival airport
arr_delay_worst= arr_delay[order(-ARR_DELAY_MEAN),] 
head(arr_delay_worst)
arr_delay_worst_plot=arr_delay_worst[1:10,]
View(arr_delay_worst_plot)
attach(arr_delay_worst_plot)
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=DEST,col='red',space=2.5,
main="Worst Arrival Airports",xlab="Destination Airport",
ylab="Average Arrival Delay(minutes)")



###################################################################
###################################################################
#Finding Best Airline
air<- subset(complete.tr, ARR_DELAY >= 0, 
select=c("UNIQUE_CARRIER", "ARR_DELAY"))
head(air)
a=read.csv(file.choose())
airline=merge(air,a,by="UNIQUE_CARRIER")
head(airline)
attach(airline)
airline[,4]=paste(airline[,3],airline[,1],sep=",")
head(airline)

#aggregating airline based on mean of arrival delay
airline_agg_mean=aggregate(airline,by=list(airline[,4]),FUN=mean)
head(airline_agg_mean)
tail(airline_agg_mean)
dim(airline_agg_mean)
airline_agg_mean=subset(airline_agg_mean,select=c(Group.1,ARR_DELAY))
head(airline_agg_mean)

#aggregating airlines based on number(count) of arrival delay
airline_agg_count=aggregate(airline,by=list(airline[,4]),FUN=length)
dim(airline_agg_count)
head(airline_agg_count)
airline_agg_count=subset(airline_agg_count,select=c(Group.1,ARR_DELAY))
head(airline_agg_count)

#combining mean and count of arrival delay based on airlines
airline_agg=cbind(airline_agg_mean,airline_agg_count)
head(airline_agg)
airline_agg=airline_agg[,-3]
head(airline_agg)

#changing the column names
names(airline_agg) <- c("UNIQUE_CARRIER", "ARR_DELAY_MEAN","ARR_DELAY_COUNT")
head(airline_agg)
summary(airline_agg)
attach(airline_agg)


#only considering the delays counts which are more than 365 in a year
threshold = 365
airline_delay= subset(airline_agg,ARR_DELAY_COUNT>threshold , 
select=c(UNIQUE_CARRIER,ARR_DELAY_MEAN,ARR_DELAY_COUNT))
head(airline_delay)
summary(airline_delay)
dim(airline_delay)
attach(airline_delay)
write.csv(airline_delay,'airline_delay.csv')


#Best Airline
airline_delay_best= airline_delay[order(ARR_DELAY_MEAN),] 
head(airline_delay_best)
airline_delay_best_plot=airline_delay_best[1:4,]
View(airline_delay_best_plot)
attach(airline_delay_best_plot)
#?barplot
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=UNIQUE_CARRIER,col='green',space=2.5,
main="Best Airlines",xlab="Airlines",
ylab="Average Arrival Delay(minutes)")

#Worst Airlines
airline_delay_worst= airline_delay[order(-ARR_DELAY_MEAN),] 
head(airline_delay_worst)
airline_delay_worst_plot=airline_delay_worst[1:4,]
View(airline_delay_worst_plot)
attach(airline_delay_worst_plot)
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=UNIQUE_CARRIER,col='red',space=2.5,
main="Worst Airlines",xlab="Airlines",
ylab="Average Arrival Delay(minutes)")




##########################################################################
#Finding Best Route
rt<- subset(complete.tr, ARR_DELAY >= 0, 
select=c('UNIQUE_CARRIER', 'DEST', 
'ORIGIN', 'ARR_DELAY', 'ORIGIN_CITY_NAME', 'DEST_CITY_NAME'))
head(rt)
dim(rt)
attach(rt)
rt[,7]=paste(rt[,3],rt[,5],sep="/")
rt[,8]=paste(rt[,2],rt[,6],sep="/")
head(rt)
rt[,9]=paste(rt[,7],rt[,8],sep="::")
head(rt)
dim(rt)


#aggregating airport routes based on mean of arrival delay
rt_agg_mean=aggregate(rt,by=list(rt[,9]),FUN=mean)
head(rt_agg_mean)
tail(rt_agg_mean)
dim(rt_agg_mean)
rt_agg_mean=subset(rt_agg_mean,select=c(Group.1,ARR_DELAY))
head(rt_agg_mean)
dim(rt_agg_mean)

#aggregating airport routes based on number(count) of arrival delay
rt_agg_count=aggregate(rt,by=list(rt[,9]),FUN=length)
dim(rt_agg_count)
head(rt_agg_count)
rt_agg_count=subset(rt_agg_count,select=c(Group.1,ARR_DELAY))
head(rt_agg_count)
dim(rt_agg_count)

#combining mean and count of arrival delay based on airport routes
rt_agg=cbind(rt_agg_mean,rt_agg_count)
head(rt_agg)
rt_agg=rt_agg[,-3]
head(rt_agg)

#changing the column names
names(rt_agg) <- c("ORIGIN_DEST", "ARR_DELAY_MEAN","ARR_DELAY_COUNT")
head(rt_agg)
summary(rt_agg)
attach(rt_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 365
rt_delay= subset(rt_agg,ARR_DELAY_COUNT>threshold , 
select=c(ORIGIN_DEST,ARR_DELAY_MEAN,ARR_DELAY_COUNT))
head(rt_delay)
summary(rt_delay)
dim(rt_delay)
attach(rt_delay)
write.csv(rt_delay,'airport_route.csv')


#Best Routes
rt_delay_best= rt_delay[order(ARR_DELAY_MEAN),] 
head(rt_delay_best)
rt_delay_best_plot=rt_delay_best[1:10,]
View(rt_delay_best_plot)
attach(rt_delay_best_plot)
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=ORIGIN_DEST,col='green',space=2.5,
main="Best Airport Routes",xlab="Airport Routes",
ylab="Average Arrival Delay(minutes)")

#Worst Routes
rt_delay_worst= rt_delay[order(-ARR_DELAY_MEAN),] 
head(rt_delay_worst)
rt_delay_worst_plot=rt_delay_worst[1:10,]
View(rt_delay_worst_plot)
attach(rt_delay_worst_plot)
barplot(ARR_DELAY_MEAN,horiz=FALSE,names.arg=ORIGIN_DEST,col='red',space=2.5,
main="Worst Airport Routes",xlab="Airport Routes",
ylab="Average Arrival Delay(minutes)")

############################################################################
############################################################################
#Finding Special Delays
#Carrier Delay 
#Carrier delay is within the control of the air carrier

cd<- subset(complete.tr, CARRIER_DELAY >= 0, 
select=c('UNIQUE_CARRIER', 'CARRIER_DELAY'))
head(cd)
dim(cd)
cd[,3]=(cd[,2]>0)*1
head(cd)
a=read.csv(file.choose())
cdf=merge(cd,a,by="UNIQUE_CARRIER")
head(cdf)
cdf[,5]=paste(cdf[,4],cdf[,1],sep="/")
head(cdf)


#aggregating airlines based on mean of carrier delay
cdf_agg_mean=aggregate(cdf,by=list(cdf[,5]),FUN=mean)
head(cdf_agg_mean)
tail(cdf_agg_mean)
dim(cdf_agg_mean)
cdf_agg_mean=subset(cdf_agg_mean,select=c(Group.1,V3))
head(cdf_agg_mean)
dim(cdf_agg_mean)

#aggregating airlines based on number(count) of carrier delay
cdf_agg_count=aggregate(cdf,by=list(cdf[,5]),FUN=length)
dim(cdf_agg_count)
head(cdf_agg_count)
cdf_agg_count=subset(cdf_agg_count,select=c(Group.1,V3))
head(cdf_agg_count)
dim(cdf_agg_count)

#combining mean and count of carrier delay based on airlines
cdf_agg=cbind(cdf_agg_mean,cdf_agg_count)
head(cdf_agg)
cdf_agg=cdf_agg[,-3]
head(cdf_agg)

#changing the column names
names(cdf_agg) <- c("UNIQUE_CARRIER", "CARRIER_AT_FAULT","CARRIER_DELAY_COUNT")
head(cdf_agg)
summary(cdf_agg)
attach(cdf_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 0
cdf_delay= subset(cdf_agg,CARRIER_AT_FAULT>threshold , 
select=c("UNIQUE_CARRIER", "CARRIER_AT_FAULT","CARRIER_DELAY_COUNT"))
head(cdf_delay)
summary(cdf_delay)
dim(cdf_delay)
attach(cdf_delay)
cdf_delay[,2]=(cdf_delay[,2])*100
head(cdf_delay)
write.csv(cdf_delay,'carrier_delay.csv')


#Carrier Delay Percentage
cdf_delay_worst= cdf_delay[order(-CARRIER_AT_FAULT),] 
head(cdf_delay_worst)
cdf_delay_worst_plot=cdf_delay_worst[1:5,]
View(cdf_delay_worst_plot)
attach(cdf_delay_worst_plot)
barplot(CARRIER_AT_FAULT,horiz=TRUE,names.arg=UNIQUE_CARRIER,col='red',space=2.5,
main="Carrier delay Percentage",xlab="Proportion of all dep. delays with carrier delay",
ylab="Carrier")



###########################################################################
###########################################################################

#Late Aircraft Delay 
#Arrival delay at an airport due to the late arrival of the same aircraft
#at a previous airport. The ripple effect of an earlier delay at downstream
#airports is referred to as delay propagation.

ld<- subset(complete.tr, LATE_AIRCRAFT_DELAY >= 0, 
select=c('UNIQUE_CARRIER', 'LATE_AIRCRAFT_DELAY'))
head(ld)
dim(ld)
ld[,3]=(ld[,2]>0)*100
head(ld)
a=read.csv(file.choose())  #airline_names
ldf=merge(ld,a,by="UNIQUE_CARRIER")
head(ldf)
ldf[,5]=paste(ldf[,4],ldf[,1],sep="/")
head(ldf)


#aggregating airlines based on mean of late aircraft delay
ldf_agg_mean=aggregate(ldf,by=list(ldf[,5]),FUN=mean)
head(ldf_agg_mean)
tail(ldf_agg_mean)
dim(df_agg_mean)
ldf_agg_mean=subset(ldf_agg_mean,select=c(Group.1,V3))
head(ldf_agg_mean)
dim(ldf_agg_mean)

#aggregating airlines based on number(count) of late aircraft delay
ldf_agg_count=aggregate(ldf,by=list(ldf[,5]),FUN=length)
dim(ldf_agg_count)
head(ldf_agg_count)
ldf_agg_count=subset(ldf_agg_count,select=c(Group.1,V3))
head(ldf_agg_count)
dim(df_agg_count)

#combining mean and count of late aircraft delay based on airlines
ldf_agg=cbind(ldf_agg_mean,ldf_agg_count)
head(ldf_agg)
ldf_agg=ldf_agg[,-3]
head(ldf_agg)

#changing the column names
names(ldf_agg) <- c("UNIQUE_CARRIER", "LATE_AIRCRAFT_AT_FAULT","LATE_AIRCRAFT_DELAY_COUNT")
head(ldf_agg)
summary(ldf_agg)
attach(ldf_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 0
ldf_delay= subset(ldf_agg,LATE_AIRCRAFT_AT_FAULT>threshold , 
select=c("UNIQUE_CARRIER", "LATE_AIRCRAFT_AT_FAULT",
"LATE_AIRCRAFT_DELAY_COUNT"))
head(ldf_delay)
summary(ldf_delay)
dim(ldf_delay)
attach(ldf_delay)
ldf_delay[,2]=(ldf_delay[,2])*100
head(ldf_delay)
write.csv(ldf_delay,'late_aircraft_delay.csv')


#Late Aircraft Delay Percentage
ldf_delay_worst= ldf_delay[order(-LATE_AIRCRAFT_AT_FAULT),] 
head(ldf_delay_worst)
ldf_delay_worst_plot=ldf_delay_worst[1:5,]
View(ldf_delay_worst_plot)
attach(ldf_delay_worst_plot)
barplot(LATE_AIRCRAFT_AT_FAULT,horiz=TRUE,names.arg=UNIQUE_CARRIER,col='red',space=2.5,
main="Late Aircraft Delay Percentage",xlab="Proportion of all dep. delays with late aircraft delay",
ylab="Carrier")


##########################################################################
##########################################################################
#Weather Delay 

wd<- subset(complete.tr, WEATHER_DELAY >= 0, 
select=c('ORIGIN', 'ORIGIN_CITY_NAME', 'WEATHER_DELAY'))
head(wd)
dim(wd)
wd[,4]=(wd[,3]>0)*1
wd[,5]=paste(wd[,2],wd[,1],sep="/")
head(wd)
tail(wd)
summary(wd)
dim(wd)

#aggregating airports based on mean of weather delay
wd_agg_mean=aggregate(wd,by=list(wd[,5]),FUN=mean)
head(wd_agg_mean)
tail(wd_agg_mean)
dim(wd_agg_mean)
wd_agg_mean=subset(wd_agg_mean,select=c(Group.1,V4))
head(wd_agg_mean)
dim(wd_agg_mean)

#aggregating airlines based on number(count) of late aircraft delay
wd_agg_count=aggregate(wd,by=list(wd[,5]),FUN=length)
dim(wd_agg_count)
head(wd_agg_count)
wd_agg_count=subset(wd_agg_count,select=c(Group.1,V4))
head(wd_agg_count)
dim(wd_agg_count)

#combining mean and count of weather delay based on airports
wd_agg=cbind(wd_agg_mean,wd_agg_count)
head(wd_agg)
wd_agg=wd_agg[,-3]
head(wd_agg)

#changing the column names
names(wd_agg) <- c("ORIGIN", "WEATHER_AT_FAULT","WEATHER_DELAY_COUNT")
head(wd_agg)
summary(wd_agg)
attach(wd_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 0
wd_delay= subset(wd_agg,WEATHER_AT_FAULT>threshold , 
select=c("ORIGIN", "WEATHER_AT_FAULT","WEATHER_DELAY_COUNT"))
head(wd_delay)
summary(wd_delay)
dim(ldf_delay)
attach(wd_delay)
wd_delay[,2]=(wd_delay[,2])*100
head(wd_delay)
dim(wd_delay)
write.csv(wd_delay,'weather_delay.csv')


#Weather Delay Percentage
wd_delay_worst= wd_delay[order(-WEATHER_AT_FAULT),] 
head(wd_delay_worst)
wd_delay_worst_plot=wd_delay_worst[1:5,]
View(wd_delay_worst_plot)
attach(wd_delay_worst_plot)
barplot(WEATHER_AT_FAULT,horiz=TRUE,names.arg=ORIGIN,col='red',space=2.5,
main="Weather Delay Percentage",xlab="Proportion of all dep. delays with
weather delay",ylab="Carrier")


###########################################################################
###########################################################################
#Finding best time to travel
#Finding Best hour of the day
attach(complete.tr)
b<- subset(complete.tr, ARR_DELAY >=0,
select=c('CRS_DEP_TIME', 'ARR_DELAY', 'DEP_DELAY'))
bh=subset(b, DEP_DELAY >=0,
select=c('CRS_DEP_TIME', 'ARR_DELAY', 'DEP_DELAY'))
head(bh,n=20)
summary(bh)
?format
bh[,1]=bh[,1]/100
head(bh)
bh[,1]=format(bh[,1],digits=4, decimal.mark=":")
head(bh)
names(bh)=c('CRS_DEP_HOUR', 'ARR_DELAY', 'DEP_DELAY')
head(bh)


#aggregating based on mean of arrival and departure delay
bh_agg_mean=aggregate(bh,by=list(bh[,1]),FUN=mean)
head(bh_agg_mean)
tail(bh_agg_mean)
dim(bh_agg_mean)
bh_agg_mean=subset(bh_agg_mean,select=c(Group.1,ARR_DELAY,DEP_DELAY))
head(bh_agg_mean)
dim(bh_agg_mean)

#aggregating based on number(count) of arrival and departure delay
bh_agg_count=aggregate(bh,by=list(bh[,1]),FUN=length)
dim(bh_agg_count)
head(bh_agg_count)
bh_agg_count=subset(bh_agg_count,select=c(Group.1,ARR_DELAY,DEP_DELAY))
head(bh_agg_count)
dim(bh_agg_count)

#combining mean and count of arrival and departure delay based on crs dep time
bh_agg=cbind(bh_agg_mean,bh_agg_count)
head(bh_agg)
bh_agg=bh_agg[,-4]
head(bh_agg)

#changing the column names
names(bh_agg) <- c('CRS_DEP_HOUR', 'ARR_DELAY_MEAN', 'DEP_DELAY_MEAN',
'ARR_DELAY_COUNT', 'DEP_DELAY_COUNT')
head(bh_agg)
summary(bh_agg)
attach(bh_agg)

#only considering the delays counts which are more than 365 in a year
threshold = 365
bh_delay= subset(bh_agg,ARR_DELAY_COUNT>threshold , 
select=c('CRS_DEP_HOUR', 'ARR_DELAY_MEAN', 'DEP_DELAY_MEAN',
'ARR_DELAY_COUNT', 'DEP_DELAY_COUNT'))
head(bh_delay)
summary(bh_delay)
dim(bh_delay)
attach(bh_delay)
head(bh_delay)
dim(bh_delay)
write.csv(bh_delay,'best_hour.csv')

?lines
?plot
plot(bh[,1],bh[,2],xlim=c(0,24),ylim=c(0,70),type="n",
xlab="Scheduled departure hour",ylab="Avg. delay (mins)",main="Delay by hour")
lines(ARR_DELAY_MEAN,col="red")
lines(DEP_DELAY_MEAN,col="blue")