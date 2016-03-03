# Reproducible Research: Peer Assessment 1


```r
echo = TRUE    # Always make code visible
options(scipen = 1) # Turn off scientific notations for numbers
```

## Loading and preprocessing the data

```r
data = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
total.steps = tapply(data$steps,data$date, FUN = sum, na.rm = TRUE)
qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(total.steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(total.steps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
averages = aggregate(x = list(steps = data$steps), by = list(interval=data$interval), FUN = mean, na.rm = TRUE)
ggplot(data  = averages, aes(x = interval, y = steps)) + 
    geom_line() +
    xlab("5 - minute interval") + 
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

Inputing missing values

There are many spots with missing value, denoted as NA. 

```r
missing = is.na(data$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

Then all of the missing value are replaced by mean value of the 5 minute interval

```r
fill.value = function(steps, interval){
  filled = NA
  if (!is.na(steps))
    filled = c(steps)
  else 
    filled = (averages[averages$interval == interval, "steps"])
}
filled.data = data
filled.data$steps = mapply(fill.value, filled.data$steps, filled.data$interval)
```

use the filled data set, make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps


```r
total.steps = tapply(filled.data$steps, filled.data$date, FUN = sum)
qplot(total.steps, binwidth = 1000, xlab = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in values, find the day of the week for each measurement in the dataset


```r
weekday.or.weekend = function(date){
  day = weekdays(date)
  if (day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else 
    stop("invalid date")
}

filled.data$date = as.Date(filled.data$date)
filled.data$day = sapply(filled.data$date, FUN = weekday.or.weekend)
```


make a panel plot containing plots of average number of steps taken on weekdays and weekends.


```r
averages = aggregate(steps ~ interval + day, data = filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5 - minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->





