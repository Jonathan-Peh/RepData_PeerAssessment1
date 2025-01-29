---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

First, we unzip the provided ZIP file for the data file

``` r
unzip("Reproducible Research/repdata_data_activity.zip",exdir = "Reproducible Research")
```
Then, we read the data from the file into R

``` r
data = read.csv("Reproducible Research/activity.csv")
```
Question 1: What is mean total number of steps taken per day?
=============================================================
Calculate the total number of steps taken per day

``` r
#removing NA
noNA = data[!is.na(data$steps),]
totalstepsperday = tapply(noNA$steps,noNA$date,sum)
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

``` r
hist(totalstepsperday, xlab = "Total Number of Steps per day", main = "Distribution of Steps Per Day",ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

``` r
print(paste("mean of total numer of steps per day is",mean(totalstepsperday)))
print(paste("median of total numer of steps per day is",median(totalstepsperday)))
```

```
## [1] "mean of total numer of steps per day is 10766.1886792453"
## [1] "median of total numer of steps per day is 10765"
```

What is the average daily activity pattern?
===========================================
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
averagestepsperinterval = tapply(noNA$steps,noNA$interval,mean)
plot(unlist(dimnames(averagestepsperinterval)),averagestepsperinterval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
print(paste("the 5-minute interval with the maximum number of steps is",max(averagestepsperinterval)))
```

```
## [1] "the 5-minute interval with the maximum number of steps is 206.169811320755"
```

Imputing missing values
=======================
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
print(paste("total number of missing values is", sum(is.na(data$steps))))
```

```
## [1] "total number of missing values is 2304"
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.
We will be choosing the mean of that 5-minute interval to fill in missing values

``` r
for (i in 1:nrow(data)){
  if (is.na(data$steps[i])){
    data$steps[i] = averagestepsperinterval[which(unlist(dimnames(averagestepsperinterval)) == data$interval[i])]
  }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
totalstepsperday_imputed = tapply(data$steps,data$date,sum)
hist(totalstepsperday_imputed, xlab = "Total Number of Steps per day", main = "Distribution of Steps Per Day (with Imputed Data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
print(paste("new mean is", mean(totalstepsperday_imputed)))
print(paste("new median is", median(totalstepsperday_imputed)))
print("mean does not change but median increases very slightly.")
plot(as.Date(unlist(dimnames(totalstepsperday))), totalstepsperday,col=rgb(0,0,1,0.5),pch=16,xlab ="",ylab="Total Number of Steps per day",ylim=c(0,30000))
legend("topright",legend = c("non-imputed","imputed"),col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),pch=16)
points(as.Date(unlist(dimnames(totalstepsperday_imputed))),totalstepsperday_imputed,col=rgb(1,0,0,0.5),pch=c(16,16))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```
## [1] "new mean is 10766.1886792453"
## [1] "new median is 10766.1886792453"
## [1] "mean does not change but median increases very slightly."
```
As seen on the graph, imputed data did not affect existing values, but it did add values for days without data

Are there differences in activity patterns between weekdays and weekends?
====
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
data$date = as.Date(data$date)
WeekdayOrWeekend = weekdays(data$date)
for (i in 1:length(WeekdayOrWeekend)){
  if (WeekdayOrWeekend[i] == "Saturday"|WeekdayOrWeekend[i] == "Sunday"){
    WeekdayOrWeekend[i] = "weekend"
  }
  else{
    WeekdayOrWeekend[i] = "weekday"
  }
}
data$WeekdayOrWeekend = factor(WeekdayOrWeekend)
head(data)
```

```
##       steps       date interval WeekdayOrWeekend
## 1 1.7169811 2012-10-01        0          weekday
## 2 0.3396226 2012-10-01        5          weekday
## 3 0.1320755 2012-10-01       10          weekday
## 4 0.1509434 2012-10-01       15          weekday
## 5 0.0754717 2012-10-01       20          weekday
## 6 2.0943396 2012-10-01       25          weekday
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
library(lattice)
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 4.4.2
```

``` r
final = melt(tapply(data$steps,list(data$interval,data$WeekdayOrWeekend),mean))
xyplot(value ~ Var1|Var2, data = final,type="l",ylab = "Average No. of Steps",xlab = "Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
