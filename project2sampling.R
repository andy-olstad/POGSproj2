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

pop_summary<-read.csv(file="3_year_summary_NEW.csv")

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

########Now capturing data
####Lower & Upper Confidence Bounds
####Mean, Median
####IQR, Q1, Q3



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
sample_summary_df = data.frame(matrix(vector(), 168, 14, dimnames=list(c(), c("Day","Hour","Mean", "Median", "IQR", "LowerCL","UpperCL", "n_sample", "N_pop", "Proportion_Delay","Mean_adj","LowerCL_adj","UpperCL_adj","Long_Delay"))), stringsAsFactors=F)

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
sample_summary_df[133,13]<-long_delay

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

long_delay<-nrow(data[which(data[,7]>15),])/n

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
sample_summary_df[i,14]<-long_delay

}

#write file output
write.csv(sample_summary_df,"SamplingResults")

#read it back later
sample_summary_df<-read.csv(file="SamplingResults")

#graphing

plot(sample_summary_df$Hour,sample_summary_df$Mean,ylim=c(-5,22))
points(sample_summary_df$Hour,sample_summary_df$Mean_adj,col="red")
diff<-sample_summary_df$Mean-sample_summary_df$Mean_adj
plot(sample_summary_df$Hour,diff)

plot(sample_summary_df$Hour,sample_summary_df$Proportion_Delay)
plot(sample_summary_df$Hour,sample_summary_df$Long_Delay)


plot(sample_summary_df$Hour,sample_summary_df$Mean/max(sample_summary_df$Mean),col="red",pch=2)
points(sample_summary_df$Hour,4*sample_summary_df$Proportion_Delay-1.2)


plot(sample_summary_df$Hour,sample_summary_df$Mean)


#Looks Familiar!!!




###########Can we do an ANOVA test on the sample data?

Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
}

for(j in 1:24){

h<-j-1

big<-read.csv(paste("Day",1,"Hour",h))
#now let's loop it!
for(i in 2:7){
data<-read.csv(paste("Day",i,"Hour",h))
big<-rbind(big,data)
}


###Perform an ANOVA test to see if there's any day effect
fit<-aov(big$arrdelay ~ as.factor(big$dayofweek))
print(summary(fit))
#p-value: 
drop1(fit,~.,test="F") # type III SS and F Tests

#Pause
}

#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    20783    3464    4.03 0.000488 ***
#Residuals                13912 11958276     860                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#114 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)
#as.factor(big$dayofweek)     6     8560    1427   1.301  0.253
#Residuals                12424 13624130    1097               
#88 observations deleted due to missingness
#                           Df  Sum Sq Mean Sq F value Pr(>F)
#as.factor(big$dayofweek)    6   15808    2635   1.773  0.101
#Residuals                2381 3538362    1486               
#30 observations deleted due to missingness
#                           Df  Sum Sq Mean Sq F value  Pr(>F)   
#as.factor(big$dayofweek)    6   29973    4995   3.124 0.00475 **
#Residuals                1869 2988374    1599                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#33 observations deleted due to missingness
#                           Df  Sum Sq Mean Sq F value Pr(>F)
#as.factor(big$dayofweek)    6    7241  1206.8   1.362  0.226
#Residuals                1810 1603783   886.1               
#27 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)
#as.factor(big$dayofweek)     6     2547   424.6   0.425  0.862
#Residuals                13587 13557726   997.8               
#243 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)
#as.factor(big$dayofweek)     6    10233    1706   1.571  0.151
#Residuals                13814 14995019    1086               
#264 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    33932    5655   6.285 1.31e-06 ***
#Residuals                13780 12399577     900                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#230 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)   
#as.factor(big$dayofweek)     6    17310  2885.0   2.908 0.0078 **
#Residuals                13715 13606411   992.1                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#223 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    24299    4050   4.253 0.000276 ***
#Residuals                13766 13106652     952   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#205 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)  
#as.factor(big$dayofweek)     6    16857    2810    2.64 0.0147 *
#Residuals                13671 14546690    1064                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#193 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    36305    6051   5.457 1.19e-05 ***
#Residuals                13755 15253120    1109                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#204 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value  Pr(>F)    
#as.factor(big$dayofweek)     6    49795    8299   6.912 2.4e-07 ***
#Residuals                13741 16498591    1201                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#236 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    44982    7497    6.02 2.66e-06 ***
#Residuals                13836 17230471    1245                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#236 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    51248    8541   6.323 1.18e-06 ***
#Residuals                13888 18760748    1351                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#276 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    50806    8468   6.224 1.54e-06 ***
#Residuals                13786 18756631    1361                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#242 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    60972   10162   6.414 9.24e-07 ***
#Residuals                13735 21762114    1584                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#289 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    50508    8418    4.71 8.48e-05 ***
#Residuals                13773 24614130    1787                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#270 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6   128663   21444   12.42 5.29e-14 ***
#Residuals                13596 23475416    1727                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#283 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)    
#as.factor(big$dayofweek)     6   144503   24084   14.57 <2e-16 ***
#Residuals                13754 22738128    1653                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#283 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)    
#as.factor(big$dayofweek)     6   230019   38336   22.84 <2e-16 ***
#Residuals                13706 23007364    1679                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#277 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value Pr(>F)    
#as.factor(big$dayofweek)     6   125029   20838   14.94 <2e-16 ***
#Residuals                13656 19050426    1395                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#265 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value  Pr(>F)    
#as.factor(big$dayofweek)     6    79975   13329   11.34 1.1e-12 ***
#Residuals                13669 16063164    1175                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#252 observations deleted due to missingness
#                            Df   Sum Sq Mean Sq F value   Pr(>F)    
#as.factor(big$dayofweek)     6    52123    8687   5.943 3.27e-06 ***
#Residuals                13550 19805536    1462                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#162 observations deleted due to missingness


###get some sizes on those confidence intervals
margin<-mutate(sampling_results,
ME=(UpperCL_adj - LowerCL_adj)/2)
summary(margin$ME)


##complex survey design ideas:
#library(survey)
#daysANDtimes<-c(1:28)
#daysANDtimesSizes<-c(2:29)
#for(i in 1:28){
#daysANDtimesSizes[i]<-5*i
#}
#filter(tbl_df(data),!is.na(arrdelay))
#
#
#svydesign(~0, probs=daysANDtimesSizes, strata = daysANDtimes, variables = NULL, fpc=NULL,
#data = NULL, nest = TRUE, check.strata = FALSE, weights=NULL,pps=FALSE)


### Look at that 3am spike

h<- 3
big<-read.csv(paste("Day",1,"Hour",h))
#now let's loop it!
for(i in 2:7){
data<-read.csv(paste("Day",i,"Hour",h))
big<-rbind(big,data)
}


head(big)
big$arrdelay
summary(big$arrdelay)
summary(big$crsdeptime)
length(big$arrdelay)
#confirming that we have the whole population:
pop_summary[which(pop_summary$time==3),]
sum(pop_summary$n_flights[which(pop_summary$time==3)])
#yep, that's all 1909

#plot delay vs. time
plot(big$crsdeptime,big$arrdelay, xlab="Departure Times",ylab="Arrival Delay",
 main = "3am departures: 1909 Flights in 3 years")
summary(big$arrdelay)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -45.00  -13.00   -2.00   10.43   16.00  349.00      33 

#look for flights delayed more than 1 hour)
long_delay<-filter(big,arrdelay>=60)

plot(long_delay$crsdeptime,long_delay$arrdelay)
spike_pop<-pop_summary[which(pop_summary$time==3),]
sum(spike_pop$n_flights)


##compare to 2am:

h<- 2
big<-read.csv(paste("Day",1,"Hour",h))
#now let's loop it!
for(i in 2:7){
data<-read.csv(paste("Day",i,"Hour",h))
big<-rbind(big,data)
}


head(big)
big$arrdelay
summary(big$arrdelay)
summary(big$crsdeptime)
length(big$arrdelay)
#confirming that we have the whole population:
pop_summary[which(pop_summary$time==2),]
sum(pop_summary$n_flights[which(pop_summary$time==2)])

#plot delay vs. time
plot(big$crsdeptime,big$arrdelay, xlab="Departure Times",ylab="Arrival Delay",
 main = "2am departures: 2418 Flights in 3 years")
summary(big$arrdelay)

##compare to 4am:

h<- 4
big<-read.csv(paste("Day",1,"Hour",h))
#now let's loop it!
for(i in 2:7){
data<-read.csv(paste("Day",i,"Hour",h))
big<-rbind(big,data)
}


head(big)
big$arrdelay
summary(big$arrdelay)
summary(big$crsdeptime)
length(big$arrdelay)
#confirming that we have the whole population:
pop_summary[which(pop_summary$time==4),]
sum(pop_summary$n_flights[which(pop_summary$time==4)])

#plot delay vs. time
plot(big$crsdeptime,big$arrdelay, xlab="Departure Times",ylab="Arrival Delay",
 main = "4am departures: 1844 Flights in 3 years")
summary(big$arrdelay)


#####try using small replicates to show the same shape
library(ggplot2)
sample_results<-read.csv(file="SamplingResults")

days<-c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday")


#plot sampling results using Sarah's code above
plot6 <- ggplot() +
  layer(data = sample_results,
        stat = "identity",
        geom = "line",
        mapping = aes(x =Hour, 
                      y = Mean_adj, 
                      color = as.factor(Day))) + 
  xlab("Day of Departure") +
  ylab("Mean delay (minutes)") +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))

plot6

#Adjusted Mean Delays
plot7 <- ggplot() +
  layer(data = sample_results,
        stat = "identity",
        geom = "line",
        mapping = aes(x =Hour, 
                      y = Mean_adj, 
                      color = as.factor(Day))) + 
  xlab("Day of Departure") +
  ylab("Mean delay (minutes)") +
  labs(title = "Adjusted Mean Delays") +
  theme_bw(18) +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))

plot7

#Raw Means
plot8 <- ggplot() +
  layer(data = sample_results,
        stat = "identity",
        geom = "line",
        mapping = aes(x =Hour, 
                      y = Mean, 
                      color = as.factor(Day))) + 
  xlab("Day of Departure") +
  ylab("Mean delay (minutes)") +
  theme_bw(18) +
  labs(title = "Mean Delay (counting negative delays)") +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))

plot8

plot9 <- ggplot() +
  layer(data = sample_results,
        stat = "identity",
        geom = "line",
        mapping = aes(x =Hour, 
                      y = Proportion_Delay, 
                      color = as.factor(Day))) + 
  xlab("Day of Departure") +
  theme_bw(18) +
  ylab("Mean delay (minutes)") +
  labs(title = "Proportion of Delays") +
  scale_color_discrete(name = "Day",
                       breaks = c("1","2","3","4","5","6","7"),
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))

plot9



