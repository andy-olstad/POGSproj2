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
  crsarrtime, crsdeptime)
sched_time_12 <- filter(sched_time, TRUNC(crsarrtime/100L) == 12) 

explain(sched_time_12)


#will use the index (you should see "Bitmap Index Scan on arr_hour" in the 
#explain statement) and be relatively quick (the "L" on 100 is crucial for it
# to pick up the index). 
#You should also be able to use it in a group_by statement (remembering the
# L on 100).
#I'm adding the same thing for crsdeptime,


# JP: selecting only columns we need with the hope loading is faster
flights_sub <- select(flights, year, dayofweek, deptime, uniquecarrier, depdelay, cancelled, diverted)

# JP: filtering only year 2013
year13 <- filter(flights_sub, year == "2013")
explain(year13)
head(year13)

# JP: Removed cancelled flights becasue they have a deptime = NA
year13_NA <- filter(year13, cancelled != 1)
head(year13_NA)

# JP: group by time of day 4 categories

#AO: look at just PDX in September, because the load times are killing me:
year13_NA_PDX<-filter(year13_NA,origin=="PDX") 
head(year13_NA_PDX)



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

