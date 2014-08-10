## ---------------------------------------------------------------------------
## 01_mean_total_num.R - peer assessment assignment 01
## Copy right (C) all rights reserved. Masaru Nakagawa 2014.
## ---------------------------------------------------------------------------

dat <- read.csv(file = "./activity.csv") # with stringsAsFactors = TRUE

sum.steps <- with(dat, tapply(steps, list(date), function(x) sum(x, na.rm = TRUE) ))

mean(sum.steps)
median(sum.steps)

