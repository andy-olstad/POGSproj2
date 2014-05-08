#Sampling Code for 599project2
#Andy Olstad Spring 2014

library(dplyr)
library(RPostgreSQL)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
  host = endpoint,
  port = 5432,
  user = user,
  password = password)

flights <- tbl(ontime, "flights")
#pdx <- filter(flights, origin == "PDX" & year == "2013")
#look at Monday data
Monday <- filter(flights, dayofweek == 1 & year == "2013")

# PDX <- filter(flights, origin == "PDX")
# PDX_random_order <- arrange(PDX, random())
# system.time(PDX_samp <- head(PDX_random_order, n = 1000L))

# but this is actually faster
#PDX_random_order2 <- filter(pdx, random() < 0.05)
Monday_random_order <- filter(Monday, random() < 0.0001)


#pdx_local<-collect(PDX_random_order2)
Monday_local<-collect(Monday_random_order)

dim(Monday_local)
#first run (random() < 0.0001) gave n = 103
#second run gave n = 82
#then n = 90
#   hour     x count
#1     0 -15.5     6
#2     1 -13.0     4
#3     2 -16.5     2
#4     5  -9.0    56
#5     6  -7.0   339
#6     7  -5.0   330
#7     8  -5.0   333
#8     9  -6.0   257
#9    10  -4.0   321
#10   11  -3.0   297
#11   12  -4.0   308
#12   13  -3.0   308
#13   14  -2.0   270
#14   15  -1.0   304
#15   16  -2.0   259
#16   17  -1.0   317
#17   18  -2.0   291
#18   19   0.0   284
#19   20   0.0   198
#20   21   0.0   134
#21   22  -3.0    67
#22   23   2.0    26

head(Monday_local)
summary(Monday_local$arrdelay)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -42.00  -11.00   -3.00   10.34    9.00  443.00       1 


Monday_local_df<-tbl_df(Monday_local)
MondayHrs<-mutate(Monday_local, hour = trunc(crsdeptime/100L))
#sched_time_12 <- filter(sched_time, TRUNC(crsdeptime/100L) == 12) 
MondayGrouped<-group_by(MondayHrs, hour)
summarize(MondayGrouped, x = median(arrdelay,na.rm=TRUE), count = n())

#n is too small here. Repeating to get a larger sample:
Monday_random_order <- filter(Monday, random() < 0.005)
Monday_local<-collect(Monday_random_order)
dim(Monday_local)
summary(Monday_local$arrdelay)
Monday_local_df<-tbl_df(Monday_local)
MondayHrs<-mutate(Monday_local, hour = trunc(crsdeptime/100L))
MondayGrouped<-group_by(MondayHrs, hour)

Expanded_MondayGrouped<-mutate(MondayGrouped, delay = ifelse(arrdelay>0,1,0))

Summary_Monday <- summarize(Expanded_MondayGrouped, 
median_delay = median(arrdelay, na.rm=TRUE), 
count = n(),
mean_delay = mean(arrdelay,na.rm=TRUE),
count_delays = sum(delay,na.rm=TRUE))

Summary_Monday
mutate(Summary_Monday, proportion_delay = count_delays/count)

#   hour median_delay count
#1     0           -8     6
#2     1            2     3
#3     5           -8    58
#4     6           -6   334
#5     7           -5   321
#6     8           -7   291
#7     9           -3   267
#8    10           -4   280
#9    11           -3   320
#10   12           -2   299
#11   13           -1   303
#12   14           -3   275
#13   15            1   299
#14   16           -1   258
#15   17            0   337
#16   18            0   300
#17   19           -3   288
#18   20           -1   220
#19   21           -3   159
#20   22           -1    65
#21   23           -8    23
#seems very weird that no 22 or 23....


###############
#Sampling for a particular 1-hour period:
pop_summary<-read.csv(file="3_year_summary_NEW.csv")
#pop_summary: some of these are pretty tiny, we probably don't need to sample them...

#let's start by sampling noon on Tuesday:
pop_summary[which(pop_summary$dayofweek==2 & pop_summary$time==12),]
n<-pop_summary[which(pop_summary$dayofweek==2 & pop_summary$time==12),4]
prob<-2000/n

TuesNoon <- select(flights, year, month, dayofmonth, dayofweek,crsdeptime,arrdelay)
TuesNoon2 <- filter(TuesNoon, TRUNC(crsdeptime/100L) == 12L & dayofweek ==2L & year %in% c(2011L,2012L,2013L)) 

TuesNoonRandom <- filter(TuesNoon2, random() < prob)

system.time(TuesNoonLocal <- collect(TuesNoonRandom))
#L makes things fast!
TuesNoonLocal

dim(TuesNoonLocal)
summary(TuesNoonLocal$arrdelay)



######Now using that as an exemplar, let's try to loop through all of Tuesday



for(i in 0:23){
pop_summary[which(pop_summary$dayofweek==2 & pop_summary$time==i),]
n<-pop_summary[which(pop_summary$dayofweek==2 & pop_summary$time==i),4]
prob<-2000/n

Tues <- select(flights, year, month, dayofmonth, dayofweek,
                      crsdeptime,arrdelay)
Tues2 <- filter(Tues, TRUNC(crsdeptime/100L) == as.integer(i) & dayofweek ==2L & year %in% c(2011L,2012L,2013L)) 

TuesRandom <- filter(Tues2, random() < prob)

system.time(TuesLocal <- collect(TuesRandom))
#L makes things fast!

write.csv(TuesLocal,file= paste("Day","Tues","Hour",i))

#dim(TuesLocal)
#summary(TuesLocal$arrdelay)
}

#read.csv(paste("Day","Tues","Hour",i))


######Now using that as an exemplar, let's try to loop through all week


for(j in 1:7){

for(i in 0:23){
pop_summary[which(pop_summary$dayofweek==j & pop_summary$time==i),]
n<-pop_summary[which(pop_summary$dayofweek==j & pop_summary$time==i),4]
prob<-2000/n

Day <- select(flights, year, month, dayofmonth, dayofweek,
                      crsdeptime,arrdelay)
Day2 <- filter(Day, TRUNC(crsdeptime/100L) == as.integer(i) & dayofweek ==as.integer(j) & year %in% c(2011L,2012L,2013L)) 

DayRandom <- filter(Day2, random() < prob)

system.time(DayLocal <- collect(DayRandom))
#L makes things fast!

write.csv(DayLocal,file= paste("Day",j,"Hour",i))

#dim(TuesLocal)
#summary(TuesLocal$arrdelay)
}
}

#complex survey design ideas:
library(survey)
daysANDtimes<-c(1:28)
daysANDtimesSizes<-c(2:29)
for(i in 1:28){
daysANDtimesSizes[i]<-5*i
}

svydesign(~0, probs=daysANDtimesSizes, strata = daysANDtimes, variables = NULL, fpc=NULL,
data = NULL, nest = TRUE, check.strata = FALSE, weights=NULL,pps=FALSE)

