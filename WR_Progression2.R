#### WR Progression ####
## July 2025 ##

# housekeeping
setwd("/Users/orly/Desktop/Swim Stats/WR Progression")

# libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

# load data
wr = read.csv("WRProgression.csv")
head(wr)

# new columns to split up the distance and the stroke
wr = wr %>%
  separate("Event", 
           sep = " ", 
           into = c("Distance", "Stroke"), 
           remove = FALSE)
head(wr)

# format of date column
class(wr$Date)
# it says character so we have to do something about that
head(wr$Date)
# upon further investigation - we have to note that the dates are also from 1900!
# the function above assumes it's from 2000
# might have to make this some manual effort...
# new column to convert it to date format so we can do calculations
wr$Date2 = as.Date(wr$Date, format = "%m/%d/%Y")
head(wr$Date2)
# test it by calculating time between the dates
timebetween = wr$Date2[2] - wr$Date2[1]
timebetween
# cool!

# we are also going to take out "NA" rankings
wr_sub = wr %>%
  drop_na(Rank)

# ok now we are ready for some time analysis

#### time between analysis ####
# calculate days between WRs for each event type
wr_diff = wr_sub %>%
  arrange(Event, Date2) %>%
  group_by(Event, Sex) %>%
  mutate(DaysPassed = as.numeric(Date2 - lag(Date2))) %>%
  ungroup()
head(wr_diff)

# anything with rank 1 should have no days passed
check_rankings = sum(wr_diff[wr_diff$Rank == 1,]$DaysPassed)
check_rankings
# cool

#### average days passed ####
# by stroke
avg_days_stroke = wr_diff %>%
  group_by(Stroke) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_stroke$Avg = round(avg_days_stroke$Avg, 2)
avg_days_stroke

# plot it
avg_days_stroke_plot = plot_ly(
  data = avg_days_stroke,
  x = ~Stroke,
  y = ~Avg,
  name = "Average Days Between Wr By Stroke",
  type = "bar"
)
avg_days_stroke_plot

# by event
avg_days_event = wr_diff %>%
  group_by(Event) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_event$Avg = round(avg_days_event$Avg, 2)
avg_days_event

# most days passed

# current longest standing record

# least days passed

# current latest world record

# Katie, Summer, Ariarne - the 400 Freestyle Preview
# for any stroke/event - total WRs
# for any stroke/event - average re-breaking (ie, breaking their own record)
# for 400 fr - who has broken it the most
# for 400 fr - how much did they break it by


