## ---------------------------------------------------------------------------
## 03_imputing_missing_values.R - peer assessment assignment 01
## Copy right (C) all rights reserved. Masaru Nakagawa 2014.
## ---------------------------------------------------------------------------

dat <- read.csv(file = "./activity.csv") # with stringsAsFactors = FALSE

# Q1 total number of missing value

sum(is.na(dat))

sum(is.na(dat$steps))
sum(is.na(dat$interval))
sum(is.na(dat$date))

# Q2

dat.d <- dat[is.na(dat$steps), ]$date
dat.i <- dat[is.na(dat$steps), ]$interval

table(droplevels(dat.d)) #

## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288 

tb <- table(dat.i)

sum(tb != tb[[1]]) # --> 0, meaning every missing value is unique == 8


ave.steps.by.intv <- aggregate(steps ~ interval, data = dat, function(x) mean(x, na.rm = TRUE))

ans = list()
av.filled = data.frame()

for (ymd in lv) {
    v <- rep(ymd, 288)
    tmp <-  cbind(v, ave.steps.by.intv)
    ans[[ymd]] <- tmp
    av.filled <- rbind(av.filled, tmp)
}
 


merge(x = 
