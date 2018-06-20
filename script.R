library(dplyr)
library(lattice)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
## Loading and preprocessing the data

unzip("activity.zip")
df <- read.csv("activity.csv")
df$date <- as.Date(df$date, format = "%Y-%m-%d")


## What is mean total number of steps taken per day?
par(mfrow = c(1,1), mar = c(4.1, 4.1, 1.1, 1.1))
per_day <- df %>%
    group_by(date) %>% 
    summarise(`Number of steps` = sum(steps, na.rm = T))

## Histogram
with(per_day, hist(`Number of steps`, 
                   main = "", breaks = 12, col = "green"))
abline(v = mean(per_day$`Number of steps`, na.rm = T),
       lty = 2, col = "red", lwd = 2)
abline(v = median(per_day$`Number of steps`, na.rm = T),
       lty = 3, col = "blue", lwd = 2)

## Time series plot 
per_int <- df %>% 
    rename(`5-minute interval` = interval) %>% 
    group_by(`5-minute interval`) %>%
    summarise(`Number of steps` = mean(steps, na.rm = T))

xyplot(`Number of steps` ~ `5-minute interval`, data = per_int, type = "l")


## Imputing missing values using the mean for that 5-minute interval
sum(is.na(df$steps))
df1 <- df %>%
    group_by(interval) %>% 
    mutate(steps = unlist(tapply(steps, interval, 
                                 function(x) ifelse(is.na(x), 
                                                    mean(x, na.rm = T), x)))) 

per_day_imp <- df1 %>% 
    group_by(date) %>% 
    summarise(`Number of steps` = sum(steps))


par(mfrow = c(1, 2))
with(per_day, hist(`Number of steps`, 
                   main = "Before imputation", breaks = 12, col = "green"))
abline(v = mean(per_day$`Number of steps`, na.rm = T),
       lty = 2, col = "red", lwd = 2)
abline(v = median(per_day$`Number of steps`, na.rm = T),
       lty = 3, col = "blue", lwd = 2)

with(per_day_imp, hist(`Number of steps`, 
                       main = "After imputation", breaks = 12, col = "wheat"))
abline(v = mean(per_day_imp$`Number of steps`),
       lty = 2, col = "red", lwd = 2)
abline(v = median(per_day_imp$`Number of steps`),
       lty = 3, col = "blue", lwd = 2)

## Are there differences in activity patterns between weekdays and weekends?
df1$week <- ifelse(weekdays(df$date) %in% unique(weekdays(df$date))[1:5], "Weekday", "Weekend")
per_week <- df1 %>% 
    rename(`5-minute interval` = interval) %>% 
    group_by(week, `5-minute interval`) %>% 
    summarise(`Number of steps` = mean(steps))

xyplot(`Number of steps` ~ `5-minute interval` | week, 
       data = per_week, type = "l", layout = c(1,2))


format()