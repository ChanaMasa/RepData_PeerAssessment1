## ---------------------------------------------------------------------------
## pa01-1.R - peer assessment assignment 01
## Copy right (C) all rights reserved. Masaru Nakagawa 2014.
## ---------------------------------------------------------------------------

dat <- read.csv(file = "./activity.csv") # with stringsAsFactors = FALSE

str(dat)

ave.steps.by.intv <- aggregate(steps ~ interval, data = dat, function(x) mean(x, na.rm = TRUE))

plot(ave.steps.by.intv, type = 'l')

ave.steps.by.intv[ave.steps.by.intv$steps ==  max(ave.steps.by.intv$steps), ]

##     interval    steps
## 104      835 206.1698


