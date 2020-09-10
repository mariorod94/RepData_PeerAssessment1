---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## **Loading and preprocessing the data**  

Loading and seeing data


```r
library(readr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
head(data)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(data)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

Format date and interval variables


```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
# data$interval <- factor(data$interval)
```


## **What is mean total number of steps taken per day?**  

Steps per day


```r
steps <- data %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(steps)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <dbl>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Histogram of steps per day


```r
hist(steps$steps, main = "Total Steps Per Day", xlab = "Steps", 
     col = "dark blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
Mean steps by day


```r
mean(steps$steps)
```

```
## [1] 9354.23
```

Median of steps by day


```r
median(steps$steps)
```

```
## [1] 10395
```


## **What is the average daily activity pattern?**

Average steps across all days

```r
steps_interval <- data %>% 
  group_by(interval) %>% 
  summarise(meansteps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(steps_interval)
```

```
## # A tibble: 6 x 2
##   interval meansteps
##      <dbl>     <dbl>
## 1        0    1.72  
## 2        5    0.340 
## 3       10    0.132 
## 4       15    0.151 
## 5       20    0.0755
## 6       25    2.09
```

Plot of average steps per day by interval


```r
plot(steps_interval$meansteps ~ steps_interval$interval, col = "dark blue",
     type = "l", main = "Mean Steps By Time Interval", xlab = "5min Interval",
     ylab = "Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Interval containing the most steps on average

```r
most_steps <- steps_interval$interval[which.max(steps_interval$meansteps)]
print(most_steps)
```

```
## [1] 835
```

Average steps for the previous interval

```r
average_most <- max(steps_interval$meansteps)
print(average_most)
```

```
## [1] 206.1698
```


## **Imputing missing values**

Total number of NA's

```r
print(sum(is.na(data$steps)))
```

```
## [1] 2304
```

NA's Strategy:  
Replacing steps NA's with mean steps for the interval.  
  
Creating a new dataset without NA values

```r
data_nona <- data

for (i in 1:nrow(data)) {
  if(is.na(data$steps[i])) {
    data_nona$steps[i] <- steps_interval$meansteps[data_nona$interval[i] == 
                                                     steps_interval$interval]
  }
}

head(data_nona)
```

```
## # A tibble: 6 x 3
##    steps date       interval
##    <dbl> <date>        <dbl>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

New histogram of total steps taken per day


```r
steps_day2 <- data_nona %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_day2$steps, main = "New Total Steps Per Day", xlab = "Steps", 
     col = "dark blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

New mean steps by day


```r
mean(steps_day2$steps)
```

```
## [1] 10766.19
```

New median of steps by day


```r
median(steps_day2$steps)
```

```
## [1] 10766.19
```

The new average is different because on the original NA values were removed. After filling NA
values, the average increased.


## **Are there differences in activity patterns between weekdays and weekends?**

Creating factor variable for weekday and weekend and filter each activity


```r
data2 <- data_nona
data2$date <- as.Date(data2$date)
data2$day <- ifelse(weekdays(data2$date) %in% c("Saturday", "Sunday"), 
                    "weekend", "weekday")

weekday <- filter(data2, day == "weekday") %>% 
  group_by(interval) %>% 
  summarise(steps = mean(steps)) %>% 
  mutate(day = as.factor("weekday"))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
weekend <- filter(data2, day == "weekend") %>% 
  group_by(interval) %>% 
  summarise(steps = mean(steps)) %>% 
  mutate(day = as.factor("weekend"))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

Creating new dataset


```r
data_days <- rbind(weekday, weekend)
data_days$day <- as.factor(data_days$day)
```

Plotting weekday and weekend activity


```r
library(ggplot2)
ggplot(data_days, aes(interval, steps)) +
  geom_line() +
  facet_grid(day~.) +
  ggtitle("Average steps: Weekday vs Weekend") +
  xlab("Interval") +
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
  
There are little differences in steps patterns by interval on weekdays and weekend.
