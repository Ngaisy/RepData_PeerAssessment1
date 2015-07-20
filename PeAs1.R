## Get to the proper directory on the computer
dir = "/Users/Shawn/Documents/BigData/Coursea/5.Reproducible Research/"
setwd(dir)

activity <- read.csv("activity.csv")
## sum of steps per day, mean and median of the total steps 
library(ggplot2)
tot_steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
hist(tot_steps, xlab="total number of steps taken each day")
mean(tot_steps, na.rm = TRUE)
median(tot_steps, na.rm = TRUE)

## A time series plot of  the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)
Aversteps <- aggregate(x = list(steps = activity$steps),
                  by = list(interval = activity$interval), 
                  FUN = mean,
                  na.rm = TRUE)

d <- ggplot(data = Aversteps, aes(x = interval, y = steps))
plot <- d + geom_line() +
       ggtitle("Time series plot: 5-minute interval vs averaged across all days ") + 
       xlab("5-minute interval") + 
       ylab("averaged across all days")
## Aversteps[Aversteps$steps == max(Aversteps$steps), ]?
max.steps <- Aversteps[Aversteps$steps == max(Aversteps$steps), ][1]

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
sum(is.na(activity))

## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, 
## or the mean for that 5-minute interval, etc.
newactivity <- activity 
for(i in 1:nrow(newactivity)){
  if(is.na(newactivity$steps[i] == TRUE))
    newactivity$steps[i] <- Aversteps[which(newactivity$interval[i] == activity$interval[i]),]$steps
}
## Check the number of NA in new data set
sum(is.na(newactivity))

plot_newactivity_steps <- ggplot(newactivity, aes(date, steps)) + geom_bar(stat = "identity",
                                                 colour = "grey",
                                                 fill = "grey",
                                                 width = 0.7) 

plot_newactivity_steps

new_tot_steps <- aggregate(newactivity$steps, 
                           list(Date = newactivity$date), 
                           FUN = sum)
colnames(new_tot_steps) <- c("date","steps")

## mean and median of the edited total steps per day
mean_new_tot_steps <- mean(new_tot_steps$steps)
median_new_tot_steps <- median(new_tot_steps$steps)
## mean and median of the original total steps per day
mean_ori_tot_steps <- mean(tot_steps, na.rm = TRUE)
median_ori_tot_steps <- median(tot_steps, na.rm = TRUE)
## Comparision 
diff_mean = mean_new_tot_steps - mean_ori_tot_steps
diff_median = median_new_tot_steps - median_ori_tot_steps

## weekday and weekend 

weekday_or_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
newactivity$date <- as.Date(newactivity$date)
newactivity$day <- sapply(newactivity$date, FUN=weekday_or_weekend)


averages <- aggregate(steps ~ interval + day, data=newactivity, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

