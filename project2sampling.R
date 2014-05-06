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
summarize(MondayGrouped, median_delay = median(arrdelay, na.rm=TRUE), count = n())

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
