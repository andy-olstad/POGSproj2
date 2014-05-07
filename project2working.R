#ST 599 Project 2
library(dplyr)
library(RPostgreSQL)
#
#Starting with Prof. Wickham's code for accessing her database
#
# --- setting up parameters to access the data base --- #
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
  host = endpoint,
  port = 5432,
  user = user,
  password = password)
# -------- #
flights <- tbl(ontime, "flights")
#' `flights` is special object, it points to the remote database, but
#' you can work with it in dplyr like it is a data.frame.
str(flights)

#' This isn't your usual head function, but it acts the same way:
head(flights)

# or you could use tbl_df to print it pretty
tbl_df(head(flights))

#######Charlotte's Database Modification on our behalf:
#Instead of creating a new column I have created an index on: 
#TRUNC(crsarrtime/100) - the hour part of the time.
#This means queries like this:
sched_time <- select(flights, year, month, dayofmonth, 
  crsarrtime, crsdeptime,origin,arrdelay,depdelay)
sched_time_12 <- filter(sched_time, TRUNC(crsarrtime/100L) == 12) 
sched_time_12_pdx13 <- filter(sched_time_12, origin == "PDX" && year == 2013) 


explain(sched_time_12)
head(sched_time_12)
head(sched_time_12_pdx13)
#AO wow that took a long time (5 minutes)... maybe there's a better way to do it?
#AO only 2 minutes with slight rewrite
dim(sched_time_12_pdx13)

smallpdx<-collect(sched_time_12_pdx13)
head(smallpdx)
smallpdx_df<-tbl_df(smallpdx)

#AO what can we learn about these delays?
summarise(smallpdx_df, 
med_arr_delay = median(arrdelay, na.rm=TRUE),
mean_arr_delay = mean(arrdelay, na.rm=TRUE),
IQR_arr_delay = IQR(arrdelay, na.rm=TRUE))

#AO find a ratio
delay_pdx13<-summarize(filter(smallpdx, arrdelay>0),length=n())
#  length
#1   1385
total_pdx13<-den<-summarize(smallpdx,length=n())
#  length
#1   4265
delay_pdx13$length/total_pdx13$length
#so 32% of flights from PDX were delayed in 2013.

#AO: look at just PDX in September, because the load times are killing me:
year13_NA_PDX<-filter(year13_NA,origin=="PDX") 
head(year13_NA_PDX)

#will use the index (you should see "Bitmap Index Scan on arr_hour" in the 
#explain statement) and be relatively quick (the "L" on 100 is crucial for it
# to pick up the index). 
#You should also be able to use it in a group_by statement (remembering the
# L on 100).
#I'm adding the same thing for crsdeptime,


 




# JP: selecting only columns we need with the hope loading is faster
#flights_sub <- select(flights, year, dayofweek, crsarrtime, uniquecarrier, arrdelay, cancelled, diverted, origin)
flights_sub <- select(flights, year, dayofweek, crsarrtime, arrdelay, cancelled, diverted)


# JP: filtering years 2011, 2012 and 2013
#year13 <- filter(flights_sub, year == "2013")
year13 <- filter(flights_sub, year == "2013" | year == "2012" | year == "2011")
### Above code used up all memory when Tim tried to run it
# explain(year13)
# head(year13)

# JP: Removed cancelled flights becasue they have a deptime = NA
year13_NA <- filter(year13, cancelled == "0")
year13_NAA <- filter(year13_NA, diverted == "0")

# JP: group by time of day---hourly
#SG: rename hourly time
year13_TOD <- group_by(year13_NAA, time = (TRUNC(crsarrtime/100L)))
# explain(year13_TOD)

#JP: group by the day of the week
year13_TODay <- group_by(year13_TOD, dayofweek)
# explain(year13_TODay)

#SG: remove unnecessary columns
year13_TODay <- select(year13_TODay, year, dayofweek, arrdelay,time)

system.time(year13_TODay <- collect(year13_TODay))
# This took Tim about 8 minutes to run
#SG: smaller set takes ~3 mins

#SG: remove NA times
TODay_3yr <- filter(year13_TODay, time != 'NA')
#SG: how do we want to handle 24? It's part of the 0 hour,
# but it means flights left the night before, which may
# introduce some confounding variables
# N is also very low compared to all other hours...data encoding issue?


# had to group_by again after I collected the data
TODay_3yr <- group_by(TODay_3yr, time)
TODay_3yr <- group_by(TODay_3yr, dayofweek)

#JP: find the mean, median and length 
#SG: na.rm needs to be inside mean/median function call
Summary_3yr <- summarise(TODay_3yr, n_flights = n(),
                         med_delay = median(arrdelay, na.rm = TRUE),
                         mean_delay = mean(arrdelay, na.rm = TRUE))
#SG: save summaries to csv
write.csv(Summary_3yr file="SG_3yr_summary.csv")
#to read on Andy's office computer:
#Summary_3yr<-read.csv("C:\\users\\andy.olstad\\Desktop\\GitHub\\POGSproj2\\SG_3yr_summary.csv")

#JP: trying to change the column name since TRUNC is a function name I am getting errors when i try to plot
#Summary_3yr <- mutate(Summary_3yr, Time = TRUNC(crsarrtime/100L) >= 0)


# JP: save summaries file as csv so everyone can access
write.csv(Summary_3yr, file="3_year_summary.csv")
# Can find full output by clicking in Environment on the right

# JP: Plot
library(ggplot2)
qplot(dayofweek, med_delay, data = Summary_3yr, color = time)
qplot(time, med_delay, data = Summary_3yr, color = dayofweek)

plot <- ggplot() +
  layer(data = Summary_3yr,
        stat = "identity",
        geom = "line",
        mapping = aes(x = time, y = med_delay, color = dayofweek, facet_grid = dayofweek))
plot


#########################################################################################
#TODO:
# need to group on departure time, not arrival
# then look at arrival delay based on the time of day you left
# this should hopefully remove the need to deal with the 24h time snafu

# graph by day

# truncate arrival delay to remove negatives?

#can we summarize before collect?
#########################################################################################


#### CHARLOTTE'S ORIGINAL CODE BELOW #####
##' Working efficiently with a remote database is a balancing act.  
#' You want to balance the time it takes:
#' 
#' * to do work on the remote database
#' * to transfer data from the database to R
#' * to do work on local data.frames in R

#' Databases are very good at subsetting, grouped operations and 
#' basic summaries, especially if the columns of interest are indexed.
#' In flights, there are already indexes on:
#' 
#' * year
#' * year, month, dayofmonth,
#' * origin
#' * dest
#' * uniquecarrier
#' * flightnum

#' SQL is a declarative language, you tell it what you want
#' not how to get it.  `explain` will tell you the SQL code
#' that will be run, and how the database plans to do it.


# replicating a subset of hflights
hou <- filter(flights, (year == "2011" & month == 1) &  
    (origin == "HOU" | origin == "IAH"))
# nothing has executed yet
explain(hou) 
hou # now it executes 
# hou_local <- collect(hou)



# finding number of flights departing per day at PDX last year
pdx <- filter(flights, origin == "PDX" & year == "2013")
pdx_by_day <- group_by(pdx, year, month, dayofmonth)
flights_per_day <- summarise(pdx_by_day, n_flights = n())
explain(flights_per_day)

flights_per_day # looks good
flights_per_day$year # but it isn't behaving like a data.frame
# because the entire result hasn't actually been brought into R yet.
fpd <- collect(flights_per_day)

fpd$date <- with(fpd, ISOdate(year, month, dayofmonth))

library(ggplot2)
qplot(date, n_flights, data = fpd, geom = "line")


######Tim's trial code:
flights_sub <- select(flights, year, dayofweek, crsdeptime, arrdelay, cancelled, diverted)

year3 <- filter(flights_sub, year == 2013L | year == 2012L | year == 2011L, cancelled == "0", diverted == "0")

year3_TOD <- group_by(year3, dayofweek, time = (TRUNC(crsdeptime/100L)))

system.time(year3_TODay <- collect(year3_TOD))

Summary_3yr <- summarise(year3_TODay, n_flights = n(),
                          med_delay = median(arrdelay),
                          mean_delay = mean(arrdelay))

#setwd("c:\\users\\andy.olstad\\desktop\\GitHub\\POGSproj2")
write.csv(Summary_3yr, file="3_year_summary_NEW.csv")

