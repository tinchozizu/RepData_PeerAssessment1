# Reproducible Research: Peer Assessment 1, Martin Salvo
## Loading and preprocessing the data

```r
ActivityData <- read.csv("activity.csv")
ActivityData2 <- read.csv("activity.csv")
ActivityClean <- ActivityData[which(ActivityData$steps!= "NA"), ]
```
## What is mean total number of steps taken per day?

```r
stepsperdate <- aggregate(steps ~ date, data=ActivityClean, FUN=sum)
meansteps <- mean(stepsperdate$steps)
mediansteps <- median(stepsperdate$steps)
barplot(stepsperdate$steps , main="Total Steps per Day", names.arg= stepsperdate$date)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean steps taken per day is 1.0766189\times 10^{4} and the median 10765.

## What is the average daily activity pattern?

```r
mean.step.5min <- aggregate(steps ~ interval, data=ActivityClean, FUN="mean")
plot(steps~interval,data=mean.step.5min,type="l", main="Steps Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max.step <- mean.step.5min[which.max(mean.step.5min$steps),]$interval
```
The max step inteval contains 835

## Imputing missing values

```r
NAsCount <- sum(is.na(ActivityData2$steps))
count = 0
##NAs are replaced with the Interval Mean of all days
for(i in 1:nrow(ActivityData)){
        if(is.na(ActivityData[i,]$steps)){
        ActivityData[i,]$steps<- mean(mean.step.5min$steps)
        count=count+1
    }
} 
```
Here is the histogram of all intervals adjusted


```r
AdjActivity <-aggregate(steps~date,data=ActivityData,sum)
barplot(AdjActivity$steps , main="Histogram of Adjusted Total",)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
Adjmean <- mean(AdjActivity$steps)
Adjmedian <- median(AdjActivity$steps)
```

The adjusted mean and median are, 1.0766189\times 10^{4} and 1.0766189\times 10^{4}, respectively.The are not significant differences, between clean data and adjusted data. 

## Are there differences in activity patterns between weekdays and weekends?

There are differences between weekdays and weekends??

```r
ActivityClean$day=ifelse(as.POSIXlt(as.Date(ActivityClean$date))$wday%%6==0,
                          "weekend","weekday")
# Grading levels: weekend and weekday 
ActivityClean$day=factor(ActivityClean$day,levels=c("weekday","weekend"))
#Making panel plot
weekdayData=aggregate(steps~interval+day, ActivityClean ,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=weekdayData ,aspect=2/1,type="l", main= "Pattern regarding weekday and weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

