# Reproducible Research: Peer Assessment 1
Khurram Majeed  

### Set options and libraries

```r
## Always make code visible
echo = TRUE;
## Turn off scientific notations for numbers
options(scipen = 1)  
## Set the decimals to 2
options(digits = 2)

## Load the library to create plots
library(ggplot2)
```

## Loading and preprocessing the data

```r
## Unzip the dataset archive
unzip("activity.zip")

## Read the CSV, set the class of each variable
data = read.csv("activity.csv", 
                colClasses = c("integer", "Date", "factor"))

## Add month column for later use in histogram
data$month <- as.numeric(format(data$date, "%m"))

## Remove the missing values from the dataset
actDataComplete = data[complete.cases(data),] 

## Set name of each row in the complete dataset to be the row number
rownames(actDataComplete) <- 1:nrow(actDataComplete)

## Checking the summary of new data
summary(actDataComplete)
```

```
##      steps          date               interval         month     
##  Min.   :  0   Min.   :2012-10-02   0      :   53   Min.   :10.0  
##  1st Qu.:  0   1st Qu.:2012-10-16   10     :   53   1st Qu.:10.0  
##  Median :  0   Median :2012-10-29   100    :   53   Median :10.0  
##  Mean   : 37   Mean   :2012-10-30   1000   :   53   Mean   :10.5  
##  3rd Qu.: 12   3rd Qu.:2012-11-16   1005   :   53   3rd Qu.:11.0  
##  Max.   :806   Max.   :2012-11-29   1010   :   53   Max.   :11.0  
##                                     (Other):14946
```

```r
## Checking the structure of new data
str(actDataComplete)
```

```
## 'data.frame':	15264 obs. of  4 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: Factor w/ 288 levels "0","10","100",..: 1 226 2 73 136 195 198 209 212 223 ...
##  $ month   : num  10 10 10 10 10 10 10 10 10 10 ...
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
ggplot(actDataComplete, aes(date, steps)) + 
  geom_bar(stat = "identity", colour = "violetred", fill = "violetred", width = 0.7) + 
  facet_grid(. ~ month, scales = "free") + 
  labs(title = "Histogram of total number of steps per day during October & November of 2012", 
       x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
## Get the total number of steps taken each day
stepsPerDay <- aggregate(steps ~ date, data=actDataComplete, sum)$steps;
stepsPerDay
```

```
##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426
## [12] 15098 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355
## [23]  2492  6778 10119 11458  5018  9819 15414 10600 10571 10439  8334
## [34] 12883  3219 12608 10765  7336    41  5441 14339 15110  8841  4472
## [45] 12787 20427 21194 14478 11834 11162 13646 10183  7047
```

```r
## Get the mean of total number of steps taken per day
mean(stepsPerDay)
```

```
## [1] 10766
```

```r
## Get the median of total number of steps taken per day
median(stepsPerDay)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Create a time series plot of 5-minute interval average number of steps taken across all days  

```r
## Calculate average steps taken for every 5-minute interval
avgStepsAll = aggregate(actDataComplete$steps,
                        list(interval = as.numeric(as.character(actDataComplete$interval))), 
                         FUN = "mean")
## Assign name to the means column 
names(avgStepsAll)[2] = "avgSteps"

## Create a line plot for intervals and average steps 
ggplot(avgStepsAll,aes(interval, avgSteps)) + 
  geom_line(color = "violetred", size = 0.75) + 
  labs(title = "Time Series Plot of average steps taken in every 5-minute interval", 
       x = "5-minute intervals", 
       y = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval contains maximum number of steps on average?

```r
## Gets the interval and number of steps
avgStepsAll[avgStepsAll$avgSteps == max(avgStepsAll$avgSteps), ]
```

```
##     interval avgSteps
## 104      835      206
```

## Imputing missing values
1. Calculate the total number of observations with missing values in dataset

```r
sum(is.na(data))
```

```
## [1] 2304
```

## Are there differences in activity patterns between weekdays and weekends?
