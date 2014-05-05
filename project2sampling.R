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
pdx <- filter(flights, origin == "PDX" & year == "2013")

# PDX <- filter(flights, origin == "PDX")
# PDX_random_order <- arrange(PDX, random())
# system.time(PDX_samp <- head(PDX_random_order, n = 1000L))

# but this is actually faster
PDX_random_order2 <- filter(pdx, random() < 0.05)

pdx_local<-collect(PDX_random_order2)
dim(pdx_local)

head(pdx_local)
summary(pdx_local$arrdelay)

pdx_local_df<-tbl_df(pdx_local)
mutate(pdx_local_df, hour=

