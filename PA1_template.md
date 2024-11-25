---
title: "Ex2"
author: "Aliya"
date: "2024-11-25"
output: html_document
---




``` r
# Load required library
library(ggplot2)

# Step 1: Load and Preprocess the Data
data <- read.csv("C:/Users/Наргиза/Desktop/activity.csv")
data$date <- as.Date(data$date)

# Step 2: Analyze Total Steps Taken Per Day
# Calculate total steps per day
daily_steps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)

# Generate a histogram of daily steps
hist(daily_steps$steps, 
     main = "Total Steps Per Day", 
     xlab = "Steps", 
     col = "blue")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
# Calculate mean and median of total steps per day
mean_steps <- mean(daily_steps$steps)
median_steps <- median(daily_steps$steps)
cat("Mean steps per day:", mean_steps, "\n")
```

```
## Mean steps per day: 10766.19
```

``` r
cat("Median steps per day:", median_steps, "\n")
```

```
## Median steps per day: 10765
```

``` r
# Step 3: Average Daily Activity Pattern
# Calculate average steps per interval
interval_steps <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)

# Plot the time series of average steps
plot(interval_steps$interval, interval_steps$steps, type = "l", col = "red", 
     xlab = "Interval", ylab = "Average Steps", 
     main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
# Find the interval with the maximum average steps
max_interval <- interval_steps[which.max(interval_steps$steps), ]
cat("Interval with max average steps:", max_interval$interval, "\n")
```

```
## Interval with max average steps: 835
```

``` r
# Step 4: Handle Missing Data
# Count missing values
total_na <- sum(is.na(data$steps))
cat("Total missing values:", total_na, "\n")
```

```
## Total missing values: 2304
```

``` r
# Impute missing values using interval averages
data_imputed <- data
data_imputed$steps[is.na(data_imputed$steps)] <- 
  interval_steps$steps[match(data_imputed$interval, interval_steps$interval)]
```

```
## Warning in data_imputed$steps[is.na(data_imputed$steps)] <-
## interval_steps$steps[match(data_imputed$interval, : number of items to replace is not a
## multiple of replacement length
```

``` r
# Histogram of total daily steps after imputation
daily_steps_imputed <- aggregate(steps ~ date, data = data_imputed, sum)
hist(daily_steps_imputed$steps, 
     main = "Total Steps Per Day (Imputed)", 
     xlab = "Steps", 
     col = "green")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

``` r
# Compare mean and median of total steps
mean_steps_imputed <- mean(daily_steps_imputed$steps)
median_steps_imputed <- median(daily_steps_imputed$steps)
cat("Mean steps per day (imputed):", mean_steps_imputed, "\n")
```

```
## Mean steps per day (imputed): 10766.19
```

``` r
cat("Median steps per day (imputed):", median_steps_imputed, "\n")
```

```
## Median steps per day (imputed): 10766.19
```

``` r
# Step 5: Weekday vs Weekend Activity
# Add a new column for weekday/weekend
data_imputed$day_type <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), 
                                "weekend", "weekday")
data_imputed$day_type <- factor(data_imputed$day_type, levels = c("weekday", "weekend"))

# Calculate average steps for each interval split by weekday/weekend
interval_steps_daytype <- aggregate(steps ~ interval + day_type, data = data_imputed, mean)

# Plot the weekday vs weekend activity
ggplot(interval_steps_daytype, aes(x = interval, y = steps, color = day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Weekday vs Weekend Activity", x = "Interval", y = "Average Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

