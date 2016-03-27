---
title: "Reproducible Research"
output: 
  html_document: 
    fig_caption: yes
    highlight: pygments
    theme: cerulean
---

Load required libraries

```{r}
library(ggplot2)
library(lattice)
library(plyr)

```

Read the activity data set from your working directory and assign it to rawdata

```{r}
rawdata <- read.csv("activity.csv")

```

Transform the date variable of the rawdata vector to a date value understandable by R


```{r}
rawdata$date <- as.POSIXct(rawdata$date, format = "%Y-%m-%d")

```

Add a weekday variable to the raw data vector using the date variable

```{r}
rawdata <- data.frame(date=rawdata$date, wkday=tolower(weekdays(rawdata$date)),steps=rawdata$steps, interval=rawdata$interval)

```

Separate weekends from weekdays in the rawdata dataset

```{r}
rawdata <-cbind(rawdata, daytype=ifelse(rawdata$wkday=="saturday" | rawdata$wkday=="sunday", "weekend", "weekday"))

```


Total the number of steps taken each day and remove the NA values and rename the columns

```{r}
steps <- aggregate(rawdata$steps, by=list(rawdata$date), FUN=sum, na.rm=TRUE)

names(steps) <- c("Dates", "Total")

```

Create a histogram of the total number of steps taken each day

```{r}
ggplot(data = steps, aes(x = Dates, y = Total,)) +geom_bar(fill = "blue", stat = "identity") + labs(title = "Number of Steps Taken Each Day")

```

Calculate the mean and median for number of steps taken each day. The mean number of steps taken each days is 9354.23 and the median number of steps taken each day is 10395.

```{r}
mean(steps$Total)
median(steps$Total)

```

Calculate the average daily activity pattern using the average number of steps taken in five minute intervals averaged across all days

Find the mean of steps across all days for all intervals and rename the variables.

```{r}
dyact <- aggregate(rawdata$steps, by=list(rawdata$interval),FUN=mean, na.rm=TRUE)

names(dyact) <- c("Interval", "Mean")

```

Create the plot of average number of steps by interval across all days

```{r}
plot(dyact$Interval, dyact$Mean, type="l", col="red", lwd=3, xlab = "Interval in Minutes", ylab = "Average Number of Steps", main = "Average Number of Steps by Interval Across All Days" )

```

Find which five minute has the highest average number of steps across all days. Interval 835 has the maximum number of steps across all days.

```{r}
max_pos <- which.max(dyact$Mean)
maxint <- dyact[max_pos,1]

```

Find the number of missing values in the dataset. only the steps variable has NA values so that is looked at and the total number of rows with missing data is 2304

```{r}
NAttl <- sum(is.na(rawdata$steps))

```

Create an imputation strategy to fill the missing data. First we find the NA rows then calculate the mean of the steps variable 

```{r}
NApos <- which(is.na(rawdata$steps))
medvec <- rep(mean(rawdata$steps, na.rm=TRUE), times=length(NApos))

```

Now the NA values in the rawdata dataset are replaced with the imputed values

```{r}
rawdata[NApos, "steps"] <- medvec

```

Compare the imputed dataset with the original results to see if the mean and median number of total steps has changed.


Create a new comparative dataset and rename the columns.

```{r}
summ <- aggregate(rawdata$steps, by=list(rawdata$date), FUN=sum)
names(summ) <- c("Date", "Total")

```

Plot the number of steps taken each day with the new imputed data

```{r}
ggplot(data = summ, aes(x = Date, y = Total,)) +geom_bar(fill = "blue", stat = "identity") + labs(title = "Number of Steps Taken Each Day with Imputed Data")

```

Find the median and mean of the Total variable in new dataset with imputed values

```{r}
mean(summ$Total)
median(summ$Total)

```

Imputing the NA values in the original data has resulted in a greater mean and median value.

Using the wekkday/weekend variable already created we'll see if there is a difference in activity between weekends and weekdays. First we create a new data set aggregated from the rawdata dataset then rename the variables and finally plot the results.

```{r}
wkd <- aggregate(rawdata$steps, by = list(rawdata$daytype, rawdata$interval, rawdata$wkday), FUN=mean)

names(wkd) <- c("DayType", "Interval", "WeekDay", "Mean")


xyplot(Mean ~ Interval | DayType, wkd, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))

```



