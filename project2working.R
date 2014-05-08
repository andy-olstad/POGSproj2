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


### I believe this was Andy's initial code here ###

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

### END OF ANDY's CODE



## THE MAIN CODING IS HERE SO FAR

### Tim's modification of Jasmine/Sarah's code, which has less commands and runs fast

# JP: selecting only columns we need with the hope loading is faster
flights_sub <- select(flights, year, dayofweek, crsdeptime, arrdelay)

# TS: Filtering the 3 years we want
year3 <- filter(flights_sub, year == 2013L | year == 2012L | year == 2011L)

# TS: Grouping first by Day of Week, and then by Hour (time) of Day
year3_TOD <- group_by(year3, dayofweek, time = (TRUNC(crsdeptime/100L)))
# explain(year3_TOD)

# TS: We can summarise()before we collect, which makes things run very fast.
# TS: Note that we are using mean_delay, and forcing all negative delays to be zero (no reward for early arrival)
# TS: Also note that SQL will automatically filter the NA's in arrdelay. However, there is one NA for 'time' which is addressed later
Summary_3yr <- summarise(year3_TOD, n_flights = n(),
                         mean_delay = mean(as.integer(arrdelay > 0)*arrdelay))
# explain(Summary_3yr)

system.time(year3_TODay <- collect(Summary_3yr))
# Good news, everyone! This collect() only took Tim about 2.5 minutes!

# Now there is
year3_Summary <- filter(year3_TODay, time >= 0 )
year3_Summary <- arrange(year3_TODay, dayofweek, time)

write.csv(year3_Summary, file="3_year_summary_NEW.csv")
# Can find full output by clicking in Environment on the right

# JP: Plot
# TS: Changed median to mean
# JP: changed to year3_Summary
library(ggplot2)
qplot(dayofweek, mean_delay, data = year3_Summary, color = time)
qplot(time, mean_delay, data = year3_Summary, color = dayofweek)

plot <- ggplot() +
  layer(data = year3_Summary,
        stat = "identity",
        geom = "line",
        mapping = aes(x = time, y = mean_delay, color = dayofweek, facet_grid = dayofweek))
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

