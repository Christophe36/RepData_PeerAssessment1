PA1\_template
================

#### Including code

``` r
library(spacetime)

# Import Data set Activity
Activity <- read.csv("activity.csv")

#What is mean total number of steps taken per day?

#Total number of steps taken per day calculation
Summ_Steps_day <- aggregate(steps~date,data = Activity,FUN = sum)
" histogramm of total number of steps per day"
```

    ## [1] " histogramm of total number of steps per day"

``` r
par(cex = .8)
plot(as.Date(Summ_Steps_day$date,),
     Summ_Steps_day$steps,
     "h",
     main = " Total number of steps taken per day",
     xlab = "Days",
     ylab = " Number of steps "
)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-1.png)<!-- -->

``` r
#3.mean and median of the total number of steps taken per day
#   mean
Mean_Steps_day <- aggregate( steps~date,
                             data = Activity,
                             FUN = mean
                             )
 # median
Median_Steps_day <- aggregate(steps~date,data = Activity,FUN = median)
                              
# What is the average daily activity pattern?
Activity.ts <- ts(data = Activity,
                          start = c(2012,4)
                         )
Steps_day.ts <- aggregate( steps~date,data = Activity.ts, FUN = sum)
par( cex = .8)
with(Steps_day.ts, 
     plot(date,
          steps,
          type = "l",
          main = " Average daily activity pattern",
          xlab = " Days" ,
          ylab = " Number of steps")
)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-2.png)<!-- -->

``` r
#2. Which 5-minute interval, on average across all the days in the dataset,
#contains the maximum number of steps?

Z <- aggregate(data = Activity,steps~interval,
               FUN = mean
               )

Max_num_steps_interval <- Z$interval[Z$steps == max(Z$steps)]

# IMPUTING MISSING VALUE
# 1.    Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NA s)

Total_NA = sum(sapply(Activity,is.na))

# 2.    Devise a strategy for filling in all of the missing values in the dataset.

# approximation of NA value with spline fitting
 
# 3.Create a new dataset that is equal to the original dataset but with 
 # the missing data filled in.
# check NA existence in each column
 
mean(is.na(Activity$steps))
```

    ## [1] 0.1311475

``` r
mean(is.na(Activity$date))
```

    ## [1] 0

``` r
mean(is.na(Activity$interval))
```

    ## [1] 0

``` r
# fill NA in steps column

steps_new <- na.spline(Activity$steps)
#creating new data set     
Activity_new <- Activity
Activity_new$steps <- steps_new

#4. Make a histogram of the total number of steps taken each day

Summ_Steps_day_new <- aggregate(steps~date,
                                data = Activity_new,
                                FUN = sum)
# plot 

par(cex = .8)
plot(as.Date(Summ_Steps_day_new$date,),
     Summ_Steps_day_new$steps,
     "h",
     main = "New Total number of steps taken per day",
     xlab = "Days",
     ylab = " Number of steps "
)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-3.png)<!-- -->

``` r
#   mean_new

Mean_Steps_day_new <- aggregate( steps~date,
                             data = Activity_new,
                             FUN = mean
)
# median_new

Median_Steps_day_new <- aggregate(steps~date,
                                  data = Activity_new,
                                  FUN = median)

# New average of daily activity pattern?

Activity_new.ts <- ts(data = Activity_new,
                  start = c(2012,4)
)
Steps_day_new.ts <- aggregate(steps~date,data = Activity_new.ts, FUN = sum)
par( cex = .8)
with(Steps_day_new.ts, 
     plot(date,
          steps,
          type = "l",
          main = " New Average daily activity pattern",
          xlab = " Days" ,
          ylab = " Number of steps")
)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-4.png)<!-- -->

``` r
# Are there differences in activity patterns between weekdays and weekends?

# 1. adding activity_new data set new column which holds weekdays and weekends
# NOTE : DAYS WILL BE MENTION IN FRENCH BECAUSE OF GEOGRAPHIC ZONE

week_days <- weekdays(as.Date((Activity_new$date)),
                       abbreviate = TRUE
)
weekday = week_days[week_days == c("lun.","mar.","mer.","jeu.","ven.")]
weekend = week_days[week_days == c("sam.", "dim.")]
Activity_new$week_days <- week_days
AG <- aggregate(steps~interval, 
                data = Activity_new,
                FUN = mean,
                na.rm= TRUE)


# 2. PLOT 
# week days average steps by 5 minutes interval plot

par(mfrow = c(2,1), cex = .5)
with(AG,plot(interval[week_days == weekday],
                       steps[week_days == weekday],
                       type = "l",
                       xlim = c(0,500),
                       ylim = c(0,5),
             main = "Weekday",
             xlab = "interval",
             ylab = "steps")
     )
#  week average plot by 5 minutes interval 

with(AG,plot(interval,steps,
             type = "l",
             xlim = c(0,500),
             ylim = c(0,5),
             main = "week")
     )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-5.png)<!-- -->
