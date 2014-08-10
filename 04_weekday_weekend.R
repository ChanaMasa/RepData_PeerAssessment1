## ---------------------------------------------------------------------------
## 04_weekday_weekend.R - peer assessment assignment 01
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

## Q4
target.df$date.p <- as.POSIXlt(as.Date(as.character(target.df$date)))
target.df$is.weekend <- target.df$date.p$wday == 0 | target.df$date.p$wday == 6

sp.wd <- split(target.df, target.df$is.weekend)


ave.steps.by.intv.wd <- aggregate(steps ~ interval, data = sp.wd[[1]],
                                  function(x) mean(x, na.rm = TRUE))

ave.steps.by.intv.we <- aggregate(steps ~ interval, data = sp.wd[[2]],
                                  function(x) mean(x, na.rm = TRUE))

op <- par()
par(mfrow = c(2, 1))

plot(ave.steps.by.intv.wd, type = 'l')
plot(ave.steps.by.intv.we, type = 'l')

par(op)

##

wd <- data.frame(week = rep("weekday", nrow(ave.steps.by.intv.wd)), ave.steps.by.intv.wd)
we <- data.frame(week = rep("weekend", nrow(ave.steps.by.intv.we)), ave.steps.by.intv.we)

ave.steps.by.intv.g <- rbind(wd, we)

library(ggplot2)

g <- ggplot(data = ave.steps.by.intv.g, aes(x = interval, y = steps, group = week, color = week))
g <- g + geom_line()
g <- g + facet_grid(week ~ .)
print(g)

