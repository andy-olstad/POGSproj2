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

head(Monday_local)
summary(Monday_local$arrdelay)

Monday_local_df<-tbl_df(Monday_local)
MondayHrs<-mutate(Monday_local, hour = trunc(crsarrtime/100L))
#sched_time_12 <- filter(sched_time, TRUNC(crsarrtime/100L) == 12) 
MondayGrouped<-group_by(MondayHrs, hour)
summarize(MondayGrouped, x = median(arrdelay), count = n())
#   hour   x count
#1     0  -9     1
#2     6 -13     1
#3     7   0     3
#4     8  30     6
#5     9  16     3
#6    11  NA    12
#7    12  -6     7
#8    13   4     8
#9    14  -7    10
#10   15 -10    10
#11   16 -12     5
#12   17   1     2
#13   18  NA     6
#14   19  -7     5
#15   20  18     5
#16   21  -4     8
#17   22  -3     8
#18   23 -15     3

