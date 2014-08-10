## ---------------------------------------------------------------------------
## 03_imputing_missing_values.R - peer assessment assignment 01
## Copy right (C) all rights reserved. Masaru Nakagawa 2014.
## ---------------------------------------------------------------------------

dat <- read.csv(file = "./activity.csv") # with stringsAsFactors = FALSE

## Q1 total number of missing value

sum(is.na(dat))

sum(is.na(dat$steps))
sum(is.na(dat$interval))
sum(is.na(dat$date))

## Q2

dat.d <- dat[is.na(dat$steps), ]$date
dat.i <- dat[is.na(dat$steps), ]$interval

table(droplevels(dat.d)) #

## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288

tb <- table(dat.i)

sum(tb != tb[[1]]) # --> 0, meaning every missing value is unique == 8


## Q3

lv <- levels(droplevels(dat.d))
ave.steps.by.intv <- aggregate(steps ~ interval, data = dat,
                               function(x) mean(x, na.rm = TRUE))

target.df <- dat
new.val.df <- ave.steps.by.intv

for (ymd in lv) { # NA's date: "2012-10-01", "2012-10-08"..
    
    tmp.df <- target.df[target.df$date == ymd, ]

    ## print(nrow(tmp.df)) ## --> 288 

    for (i in new.val.df$interval) { # 0, 5, 10, ... 2355

        new.v <- new.val.df[ new.val.df$interval == i, ]$steps
        target.df[target.df$date == ymd & target.df$interval == i, ]$steps <- new.v
        ## print(paste("orig -> :", orig.v))
        ## print(paste("new  -> :", new.v))
    }
}


##
ave.steps.by.intv.adj <- aggregate(steps ~ interval, data = target.df,
                                   function(x) mean(x, na.rm = TRUE))

plot(ave.steps.by.intv.adj, type = 'l')
plot(ave.steps.by.intv, type = 'l')
        



