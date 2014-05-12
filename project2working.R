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

# New Data set for geom_ribbon
new <- group_by(year3_Summary, time, add=FALSE)
ribbon_data <- summarise
write.csv(ribbon_data, "ribbon_graph_data.csv")

write.csv(year3_Summary, file="3_year_summary_NEW.csv")
# Can find full output by clicking in Environment on the right

# SG: read in summary csv
year3_Summary <- read.csv("3_year_summary_NEW.csv", header = T, stringsAsFactors = F)
# AO: renamed as year3_Summary_NEW because that's what the ggplot code calls
year3_Summary_NEW<-year3_Summary

# JP: Plot
# TS: Changed median to mean
# JP: changed to year3_Summary
library(ggplot2)
qplot(dayofweek, mean_delay, data = year3_Summary_NEW, color = time)
qplot(time, mean_delay, data = year3_Summary_NEW, color = dayofweek)

# SG: changed to line plot
plot <- ggplot() +
  geom_line(aes(x= year3_Summary_NEW$time, 
                y = year3_Summary_NEW$mean_delay, 
                color = factor(year3_Summary_NEW$dayofweek), 
                group=factor(year3_Summary_NEW$dayofweek)),
            size = 1) +
  xlab("Time of departure") +
  ylab("Mean delay (minutes)") +
  theme_bw(18) +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"),
                       guide = guide_legend(title.theme = element_text(size=18, angle = 0),
                                            label.theme = element_text(size = 15, angle = 0)))

plot

# Tim working on the above plot
# Success! This gives an "outline" of the population data that we can use for the superimposing
ribbon_graph_data <- read.csv("ribbon_graph_data.csv", header = T, stringsAsFactors = F)
# renamed to plot2 separate from above
plot2 <- ggplot(data = ribbon_graph_data, mapping = aes(x = time)) + 
  geom_ribbon(aes(ymin=min_delay, ymax=max_delay), fill="#999999") + 
  xlab("Time of departure") +
  ylab("Mean delay (minutes)")

plot2
# Try using geom_ribbon on the min and max maen delay per hour
# First we will have to extract the min/max per day per hour into another data set
# geom_ribbon(aes(ymin=, ymax=), colour=)


############Sampling Approach#######################

###Get populaton size numbers
pop_summary<-read.csv(file="3_year_summary_NEW.csv")


###Use a nested loop to sample hours within days
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

###Use another nested loop to summarize this sample data:

data<-read.csv(paste("Day",5,"Hour",13))
#summary(data$arrdelay)
median<-median(data$arrdelay,na.rm=TRUE)
avg<-mean(data$arrdelay,na.rm=TRUE)

#confidence interval (cf cyclismo.org)
s<-sd(data$arrdelay,na.rm=TRUE)
n<-nrow(data[which(!is.na(data[,7])),])
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- avg-error
right <- avg+error

#confidence interval after scrubbing negatives:
adjusted_mean_delay<-sum(data$arrdelay[which(data$arrdelay>0)],rm.na=TRUE)/n
s_adj<-sd(data$arrdelay[which(data$arrdelay>0)],na.rm=TRUE)
error_adj <- qt(0.975,df=n-1)*s_adj/sqrt(n)
left_adj <- adjusted_mean_delay-error_adj
right_adj <- adjusted_mean_delay+error_adj

proportion_delay<-nrow(data[which(data[,7]>0),])/n
iqr<-IQR(data$arrdelay,na.rm=TRUE)
N<-pop_summary[which(pop_summary$dayofweek==5 & pop_summary$time==13),4]


#build a data frame to store this information:
sample_summary_df = data.frame(matrix(vector(), 168, 13, dimnames=list(c(), c("Day","Hour","Mean", "Median", "IQR", "LowerCL","UpperCL", "n_sample", "N_pop", "Proportion_Delay","Mean_adj","LowerCL_adj","UpperCL_adj"))), stringsAsFactors=F)

#hand populate that dataframe
sample_summary_df[133,1]<-5
sample_summary_df[133,2]<-13
sample_summary_df[133,3]<-avg
sample_summary_df[133,4]<-median
sample_summary_df[133,5]<-iqr
sample_summary_df[133,6]<-left
sample_summary_df[133,7]<-right
sample_summary_df[133,8]<-n
sample_summary_df[133,9]<-N
sample_summary_df[133,10]<-proportion_delay
sample_summary_df[133,11]<-adjusted_mean_delay
sample_summary_df[133,12]<-left_adj
sample_summary_df[133,13]<-right_adj

#now let's loop it!
for(i in 1:168){
d<-ceiling(i/24)
h<- (i-1) %% 24
data<-read.csv(paste("Day",d,"Hour",h))
#summary(data$arrdelay)
median<-median(data$arrdelay,na.rm=TRUE)
avg<-mean(data$arrdelay,na.rm=TRUE)

#confidence interval (cf cyclismo.org)
s<-sd(data$arrdelay,na.rm=TRUE)
n<-nrow(data[which(!is.na(data[,7])),])
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- avg-error
right <- avg+error

#confidence interval after scrubbing negatives:
adjusted_mean_delay<-sum(data$arrdelay[which(data$arrdelay>0)],rm.na=TRUE)/n
s_adj<-sd(data$arrdelay[which(data$arrdelay>0)],na.rm=TRUE)
error_adj <- qt(0.975,df=n-1)*s_adj/sqrt(n)
left_adj <- adjusted_mean_delay-error_adj
right_adj <- adjusted_mean_delay+error_adj

proportion_delay<-nrow(data[which(data[,7]>0),])/n
iqr<-IQR(data$arrdelay,na.rm=TRUE)
N<-pop_summary[which(pop_summary$dayofweek==d & pop_summary$time==h),4]

sample_summary_df[i,1]<-d
sample_summary_df[i,2]<-h
sample_summary_df[i,3]<-avg
sample_summary_df[i,4]<-median
sample_summary_df[i,5]<-iqr
sample_summary_df[i,6]<-left
sample_summary_df[i,7]<-right
sample_summary_df[i,8]<-n
sample_summary_df[i,9]<-N
sample_summary_df[i,10]<-proportion_delay
sample_summary_df[i,11]<-adjusted_mean_delay
sample_summary_df[i,12]<-left_adj
sample_summary_df[i,13]<-right_adj
}

###write file output
write.csv(sample_summary_df,"SamplingResults")
# read file output
sampling_results <- read.csv("SamplingResults")

#plot sampling results using Sarah's code above
plot3 <- ggplot() +
  layer(data = sampling_results,
        stat = "identity",
        geom = "line",
        mapping = aes(x = Hour, 
                      y = Mean_adj, 
                      color = as.factor(Day))) + 
  xlab("Time of departure") +
  ylab("Mean delay (minutes)") +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))

plot3

# SG: working on plot2 and plot3 overlay
plot4 <- ggplot() +
  geom_ribbon(aes(ymin=ribbon_graph_data$min_delay, 
                  ymax=ribbon_graph_data$max_delay,
                  x = ribbon_graph_data$time), 
              fill="#B8B8B8") + 
  geom_line(aes(x= sampling_results$Hour, 
                y = sampling_results$Mean_adj, 
                color = factor(sampling_results$Day), 
                group=factor(sampling_results$Day)),
            size = 1) +
  xlab("Time of departure") +
  ylab("Mean delay (minutes)") +
  theme_bw(18) +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"),
                       guide = guide_legend(title.theme = element_text(size=18, angle = 0),
                                            label.theme = element_text(size = 15, angle = 0)))    
plot4
# SG: it works!

###graphing a few exploratory plots

##Overlay mean with adjusted mean:
plot(sampling_results$Hour,sampling_results$Mean,ylim=c(-5,22))
points(sampling_results$Hour,sampling_results$Mean_adj,col="red")
diff<-sampling_results$Mean-sampling_results$Mean_adj
plot(sampling_results$Hour,diff)

##graph proporiton delayed
plot(sampling_results$Hour,sampling_results$Proportion_Delay)

##overlay mean with rescaled proportoin
plot(sampling_results$Hour,sampling_results$Mean/max(sampling_results$Mean),col="red",pch=2)
points(sampling_results$Hour,4*sampling_results$Proportion_Delay-1.2)

#Looks Familiar!!!


##try for graph by hour
plot(sampling_results$Day,sampling_results$Mean)


