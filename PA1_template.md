# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Additional libraries needed for this analysis.


```r
library(data.table)
library(ggplot2)
library(dplyr)
```

Extract and load the data file.


```r
unzip('activity.zip')
df.data <- fread( "activity.csv", na.strings="NA", data.table=FALSE)
str(df.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert the date column from a "char" to a "date" data type for this analysis.


```r
df.data$date <- as.Date( df.data$date )
str(df.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps per day, ignoring missing values.
   There are 61 total days of data in the dataset, 10/01/2012 - 11/30/2012. 
   Ignoring missing values reduces the analysis to 53 days.


```r
steps_per_day <- aggregate( steps~date, df.data, sum )
str(steps_per_day)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

2. Create a histogram of the number of steps per day.


```r
hist( steps_per_day$steps, 
      main = "Total Number of Steps Taken Per Day", 
      col = "burlywood2",
      breaks = 25,
      xlab = NULL)
```

![](PA1_template_files/figure-html/histogram_ignore_NA-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.


```r
mean_steps_per_day <- mean( steps_per_day$steps )
median_steps_per_day <- median( steps_per_day$steps )
```


```
The mean is 10766.19
```

```
The median is 10765.00
```

## What is the average daily activity pattern?

1. Calulate the average steps for each 5 minute interval during the day across all days in the data sample, ignoring missing values.


```r
interval_average <- aggregate( steps~interval, df.data, mean )
str(interval_average)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

2. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot.ts(interval_average$interval, 
        interval_average$steps, 
        type = "l",
        main = "Average Number of Steps Taken Per 5 Minute Interval", 
        col = "BLUE"
        ,xlab = "Interval (0 - 2355)", 
        ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/Time_series_plot_average_steps -1.png) 

3. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_step_interval <- interval_average$interval[ which.max( interval_average$steps ) ]
```


```
Interval 835 contains the maximum number of steps.
```

## Imputing missing values

1. The total number of rows with at least 1 NA value are:


```r
cat( sprintf( "   %d\n", sum(!complete.cases(df.data) )))
```

```
   2304
```

2. The strategy used to fill in the NA values will be to use the mean for that 5-minute interval.
   The process will be:
   a) Merge the inital data frame "df.data" with the "interval_average" data frame adding a new
      column of daily_average_steps for each interval. 
   b) The NA step values will then be replaced with the daily_average_steps value for the respective interval.


```r
df.imputed <- merge(df.data, rename( interval_average, daily_average_steps = steps ), by.x = "interval")
df.imputed[ is.na( df.imputed$steps ),'steps'] <- df.imputed[ is.na(df.imputed$steps),'daily_average_steps']
str(df.imputed)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ interval           : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps              : num  1.72 0 0 0 0 ...
##  $ date               : Date, format: "2012-10-01" "2012-11-23" ...
##  $ daily_average_steps: num  1.72 1.72 1.72 1.72 1.72 ...
```

3. Create an updated histogram of the total steps per day.


```r
updated_steps_per_day <- aggregate( steps~date, df.imputed, sum )
str(updated_steps_per_day)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: num  10766 126 11352 12116 13294 ...
```


```r
hist( updated_steps_per_day$steps, 
      main = "Total Number of Steps Taken Per Day", 
      col = "coral1" ,
      breaks = 20,
      xlab = NULL)
```

![](PA1_template_files/figure-html/histogram_all-1.png) 

4. Re-calculate and report the mean and median of the total number of steps taken per day.


```r
new_mean_steps_per_day <- mean( updated_steps_per_day$steps )
new_median_steps_per_day <- median( updated_steps_per_day$steps )
```


```
The mean is 10766.19
```

```
The median is 10766.19
```

4. Dataset comparison - ignoring NA vs imputed NA.


```
Mean ignoring NA 10766.19 - Mean imputed NA 10766.19
```

```
Median ignoring NA  10765.00 - Median imputed NA 10766.19
```

The mean values are identical.

The median values vary by (1.19).

The difference between ignoring NA values and compensating for them appears to be insignificant.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dt.wkday_wkend <- data.table( df.imputed )
dt.wkday_wkend$work_or_play <- wday( dt.wkday_wkend$date )
dt.wkday_wkend$work_or_play <- 
   sapply( dt.wkday_wkend$work_or_play, 
           function(x){
             if( (x > 1) & (x < 7) ){
                x = "weekday"
             }else{
                X = "weekend"
             }
           } 
         )

dt.wkday_wkend$work_or_play <- factor( dt.wkday_wkend$work_or_play )
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps_wkday_wkend <- aggregate( steps ~ interval + work_or_play, dt.wkday_wkend, mean)

g <- ggplot( steps_wkday_wkend, aes(interval, steps) )

g + geom_line( color="darkblue" ) + 
  facet_grid( work_or_play ~. ) + 
  labs( x = "interval (0 - 2355)", 
        y = "Number of Steps", 
        title = "Average Number of Steps Taken Per 5 Minute Interval")
```

![](PA1_template_files/figure-html/panel_plot_by_weekday_weekend -1.png) 
