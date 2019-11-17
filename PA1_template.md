Reproducible Research Week 2 Assignment
==============================================================================================

First, I download the data and load it into R.


```r
#setwd("./R/Coursera_Data_Science_Specialization/Reproducible_Research/week2")

if (!file.exists("activity.zip")){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, destfile="./activity.zip")
  unzip("activity.zip")
}

data<-read.csv("activity.csv", header=TRUE, sep=",", col.names=c("steps", "date", "interval"))
```

## Part 1

What is the mean total number of steps taken per day?

### Question 1
Calculate the total number of steps taken per day.


```r
data$date<-as.Date(data$date, format="%Y-%m-%d")
data2<-tapply(data$steps, as.factor(data$date), sum)
data3<-as.data.frame.table(data2, ncols=2)
colnames(data3)<-c("date", "steps")
data3
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

### Question 2
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(data3$steps, xlab="Number of Steps", ylab="Frequency", main="Frequency of Total Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Question 3
Calculate and report the mean and median of the total number of steps taken per day


```r
totalmean<-mean(data3$steps, na.rm=TRUE)
print(totalmean)
```

```
## [1] 10766.19
```

```r
totalmedian<-median(data3$steps, na.rm=TRUE)
print(totalmedian)
```

```
## [1] 10765
```

## Part 2
What is the average daily activity pattern?

### Question 1
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data4<-tapply(data$steps, as.factor(data$interval), mean, na.rm=TRUE)
data5<-as.data.frame.table(data4, ncols=2)
colnames(data5)<-c("interval", "average_steps")

library(ggplot2)
sp<-ggplot(data=data5, aes(x=interval, y=average_steps, group=1))+
  geom_point() + geom_line()
sp+ scale_x_discrete(name="5-min interval", breaks = seq(0, 2355, 200)) +
 scale_y_continuous(name="Average Number of Steps Taken Averaged Across All Days") +
  ggtitle("Average # of Steps Taken per Day for Each Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

### Question 2

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
themax<-data5[which.max(data5$average_steps),]
print(themax[1,1])
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```
 The interval which the most steps each day ( on average) is the 835th interval.
 
## Part 3

Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### Question 1

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
thesum<-sum(is.na(data$steps))
print(thesum)
```

```
## [1] 2304
```

### Question 2
Will replace the NA's with the mean for that interval averaged across all days.

### Question 3
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
library(dplyr)
data$steps<-ifelse(is.na(data$steps), data5$average_steps, data$steps)
```

### Question 4

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
data7<-tapply(data$steps, as.factor(data$date), sum)
data8<-as.data.frame.table(data7, ncols=2)
colnames(data8)<-c("date", "steps")
hist(data8$steps, xlab="Number of Steps", ylab="Frequency", main="Frequency of Total Number of Steps Per Day", breaks=61)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
mean(data8$steps)
```

```
## [1] 10766.19
```

```r
#answer is 10766.19
median(data8$steps)
```

```
## [1] 10766.19
```

```r
#answer is 10766.19
```

When the NA missing values are imputed with the mean for that interval averaged across all days there is very little impact on the mean and median. The mean with the NA values was 10766.19 and the mean with imputed NA values is 10766.19. The median with the NA values removed was 10765 and the median with imputed NA values is 10766.19.

## Part 4

Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### Question 1 
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data$date<-as.Date(data$date, format="%Y-%m-%d")
data$weekday<-weekdays(data$date)
```

### Question 2

Make a panel plot containing a time series plot type="l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#create two data frames one which has weekday days and one which has weekend days
seperated<-split(data, data$weekday)
MTWTF<-rbind(seperated$Monday, seperated$Tuesday, seperated$Wednesday, seperated$Thursday, seperated$Friday)
SS<-rbind(seperated$Saturday, seperated$Sunday)

# for each interval number average the steps taken in the two seperate data frames

MTWTFmean<-tapply(MTWTF$steps, as.factor(MTWTF$interval), mean, na.rm=TRUE)
MTWTFmean1<-as.data.frame.table(MTWTFmean, ncols=2)
colnames(MTWTFmean1)<-c("interval", "average_steps")

SSmean<-tapply(SS$steps, as.factor(SS$interval), mean, na.rm=TRUE)
SSmean1<-as.data.frame.table(SSmean, ncols=2)
colnames(SSmean1)<-c("interval", "average_steps")

plot1<-ggplot(data=MTWTFmean1, aes(x=interval, y=average_steps, group=1))+ geom_point() + geom_line() + 
  scale_x_discrete(name="5-min interval", breaks = seq(0, 2355, 200))+
  ggtitle("Weekday")

plot2<-ggplot(data=SSmean1, aes(x=interval, y=average_steps, group=1))+ geom_point() + geom_line() +
scale_x_discrete(name="5-min interval", breaks = seq(0, 2355, 200))+
  ggtitle("Weekend")

library(gridExtra)
grid.arrange(plot1, plot2, ncol=1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
