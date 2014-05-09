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




## THE MAIN CODING IS HERE SO FAR

### Tim's modification of Jasmine/Sarah's code, which has less commands and runs fast

# JP: selecting only columns we need with the hope loading is faster
flights_sub <- select(flights, year, dayofweek, crsdeptime, arrdelay)

# TS: Filtering the 3 years we want
year3 <- filter(flights_sub, year == 2013L | year == 2012L | year == 2011L)

# TS: Grouping first by Day of Week, and then by Hour (time) of Day
year3_TOD <- group_by(year3, dayofweek, time = (TRUNC(crsdeptime/100L)))
# explain(year3_TOD)

# TS: We can summarise() before we collect, which makes things run very fast.
# TS: Note that we are using mean_delay, and forcing all negative delays to be zero (no reward for early arrival)
# TS: Also note that SQL will automatically filter the NA's in arrdelay. However, there is one NA for 'time' which is addressed later
Summary_3yr <- summarise(year3_TOD, n_flights = n(),
                         mean_delay = mean(as.integer(arrdelay > 0)*arrdelay),
                         prop_delay = mean(as.integer(arrdelay > 0)))
# explain(Summary_3yr)

system.time(year3_TODay <- collect(Summary_3yr))
# Good news, everyone! This collect() only took Tim about 2.5 minutes!

# Now there is
year3_Summary <- filter(year3_TODay, time >= 0 )
year3_Summary <- arrange(year3_Summary, dayofweek, time)

write.csv(year3_Summary, file="3_year_summary_NEW.csv")
# Can find full output by clicking in Environment on the right

# SG: read in summary csv
year3_Summary <- read.csv("3_year_summary_NEW.csv", header = T, stringsAsFactors = F)

# JP: Plot
# TS: Changed median to mean
# JP: changed to year3_Summary
library(ggplot2)
qplot(dayofweek, mean_delay, data = year3_Summary, color = time)
qplot(time, mean_delay, data = year3_Summary, color = dayofweek)

# SG: changed to line plot
plot <- ggplot() +
  layer(data = year3_Summary,
        stat = "identity",
        geom = "line",
        mapping = aes(x = time, 
                      y = mean_delay, 
                      color = as.factor(dayofweek))) +
  xlab("Time of departure") +
  ylab("Mean delay (minutes)") +
  scale_color_discrete(name = "Day of week",
                      breaks = c("1","2","3","4","5","6","7"),
                      labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                 "Friday", "Saturday", "Sunday"))

plot


#########################################################################################
#TODO:
# need to group on departure time, not arrival - done
# then look at arrival delay based on the time of day you left 
# this should hopefully remove the need to deal with the 24h time snafu

# graph by day - done

# truncate arrival delay to remove negatives? - done

#can we summarize before collect? - done
#########################################################################################

