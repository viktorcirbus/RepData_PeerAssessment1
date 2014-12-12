
#Loading and preprocessing the data

activity = read.csv("~/GitHub/RepData_PeerAssessment1/data/activity.csv", stringsAsFactors=FALSE)
activity$date=as.Date(activity$date)

#What is mean total number of steps taken per day?

# 1.  Make a histogram of the total number of steps taken each day

activity.sums = aggregate(activity[c("steps")], 
                           by = activity[c("date")],
                           FUN=sum, na.rm=TRUE)

hist(activity.sums$steps, breaks = 50, main="histogram of the total number of steps taken each day",xlab="Total number of steps taken each day")


# 2.  Calculate and report the mean and median total number of steps taken per day


#Mean number of steps:

mean(activity.sums$steps, na.rm = TRUE)

#Median number of steps:

median(activity.sums$steps, na.rm = TRUE)


# What is the average daily activity pattern?

#1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


activity.intervals = aggregate(activity[c("steps")], 
                           by = activity[c("interval")],
                           FUN=mean, na.rm=TRUE)

plot(activity.intervals$interval,activity.intervals$steps,type="l")

#2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

activity.intervals[activity.intervals$steps == max(activity.intervals$steps), ]$interval

# Imputing missing values

# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


length(which(is.na(activity$steps)))



# 2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Mean for that 5-minute interval used for imputation

for(i in 1:nrow(activity)){
  activity[i,c("imputed")]=ifelse(is.na(activity[i,c("steps")]),activity.intervals[activity.intervals$interval==activity[i,c("interval")],]$steps,activity[i,c("steps")])
}


#3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity2=activity[,c("imputed","date","interval")]
colnames(activity2)[1] = "steps"


#4.  

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
#total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

activity2.sums = aggregate(activity2[c("steps")], 
                          by = activity2[c("date")],
                          FUN=sum)

hist(activity2.sums$steps, breaks = 50, main="histogram of the total number of steps taken each day after imputation",xlab="Total number of steps taken each day after imputation")

#Mean number of steps:

mean(activity2.sums$steps)

#Median number of steps:

median(activity2.sums$steps)

#Both the mean and the median are higher after using the imputed values in our calculation in comparison to the original data set.


#Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
#1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#indicating whether a given date is a weekday or weekend day.


for(i in 1:nrow(activity2)){
  activity2[i,c("datetype")]=ifelse(as.POSIXlt(activity2[i,c("date")])$wday == 6,"weekend",ifelse(as.POSIXlt(activity2[i,c("date")])$wday == 7,"weekend","weekday"))
}

activity2$datetype=as.factor(activity2$datetype)


#2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,
#averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see 
#an example of what this plot should look like using simulated data.

activity2.datetype = aggregate(activity2[c("steps")], 
                           by = activity2[c("interval","datetype")],
                           FUN=mean)

ggplot(data = activity2.datetype, aes(x = interval, y = steps, color = datetype)) + 
  geom_line() + facet_wrap(~datetype, ncol = 1) + ylab("average number of steps taken")


