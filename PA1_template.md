Loading and preprocessing the data
----------------------------------

Show any code that is needed to

-   Load the data (i.e. read.csv())
-   Process/transform the data (if necessary) into a format suitable for
    your analysis

<!-- -->

    ##load libraries
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(readr)
    library(ggplot2)
    library(tidyr)
    library(chron)

    ##read data
    data <- read_csv("E:/Coursera Data Science/Course 5/Week 2/Git Files/RepData_PeerAssessment1/activity.zip") %>% as_tibble

    ## Parsed with column specification:
    ## cols(
    ##   steps = col_integer(),
    ##   date = col_date(format = ""),
    ##   interval = col_integer()
    ## )

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in
the dataset.

-   Calculate the total number of steps taken per day
-   If you do not understand the difference between a histogram and a
    barplot, research the difference between them. \* Make a histogram
    of the total number of steps taken each day
-   Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    ##What is mean total number of steps taken per day?
    totalSteps <- data %>% 
            na.omit() %>%
            group_by(date) %>% 
            summarise(totalsteps = sum(steps))


    ##histogram of the total number of steps taken each day
    g <- ggplot(totalSteps, aes(totalsteps)) +
            geom_histogram(fill = "#4e81d3", binwidth = 500) + 
            xlab("Total daily steps") + 
            ggtitle("Count of occurances of total daily steps")
    print(g)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ##Calculate and report the mean and median of the total number of steps taken per day
    mean(totalSteps$totalsteps)

    ## [1] 10766.19

    median(totalSteps$totalsteps)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all days (y-axis).

-   Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    #calc average steps per day per interval
    dayAvg <- data %>% 
            na.omit() %>%
            group_by(interval) %>% 
            summarise(avg = mean(steps))

    #plot time series as line graph
    g <- ggplot(dayAvg, aes(interval, avg)) +
            geom_line(colour = "#5e20ba") +
            xlab("Interval (5 min increments)") +
            ylab("Number of steps") + 
            ggtitle("Average no.steps taken, averaged across days, per 5 min. interval")
    print(g)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    filter(dayAvg, avg == max(avg))[,1]

    ## # A tibble: 1 x 1
    ##   interval
    ##      <int>
    ## 1      835

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values.

The presence of missing days may introduce bias into some calculations
or summaries of the data.

-   Calculate and report the total number of missing values in the
    dataset.
-   Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.
-   Create a new dataset that is equal to the original dataset but with
    the missing data filled in.
-   Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    ##Calculate and report the total number of missing values in the dataset
    sum(is.na(data)) #total

    ## [1] 2304

    sapply(data, function(x) sum(is.na(x))) #breakdown by column

    ##    steps     date interval 
    ##     2304        0        0

    ##Impute missing values using interval mean from all days
    join <- full_join(data, dayAvg, by = "interval")
    join$steps[is.na(join$steps)] <- join$avg[is.na(join$steps)]


    ##What is adjusted total number of steps taken per day?
    adjTotSteps <- join %>% 
            group_by(date) %>% 
            summarise(totalsteps = sum(steps))

    ##plot histogram
    g <- ggplot(adjTotSteps, aes(totalsteps)) +
            geom_histogram(fill = "#539621", binwidth = 500) + 
            xlab("Total daily steps") + 
            ggtitle("Count of occurances of total daily steps, with imputation")
    print(g)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ##adjusted mean and median and difference from originals
    mean(adjTotSteps$totalsteps)

    ## [1] 10766.19

    median(adjTotSteps$totalsteps)

    ## [1] 10766.19

    mean(adjTotSteps$totalsteps) - mean(totalSteps$totalsteps)

    ## [1] 0

    median(adjTotSteps$totalsteps) - median(totalSteps$totalsteps)

    ## [1] 1.188679

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Use the dataset with the filled-in missing values for this part.

-   Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.
-   Make a panel plot containing a time series plot of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all weekday days or weekend days (y-axis). See the README
    file in the GitHub repository to see an example of what this plot
    should look like using simulated data.

<!-- -->

    ##Weekend/weekday split and convert output to character
    join$wknd <- join$date %>%
            is.weekend() 

    join$wknd <- ifelse(join$wknd == FALSE, "Weekday", "Weekend")


    ##updated mean per interval with NAs removed
    JoinDayAvg <- join %>% 
            group_by(interval, wknd) %>% 
            summarise(avg = mean(steps))

    #Plot weekend v weekday
    g <- ggplot(JoinDayAvg, aes(interval, avg)) +
            geom_line(aes(colour = wknd), show.legend = FALSE) +
            facet_grid(rows = vars(wknd)) +
            xlab("Interval (5 min increments)") +
            ylab("Number of steps") + 
            ggtitle("Mean no.steps taken, averaged across days, per 5 min. interval, with imputation")
    print(g)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
