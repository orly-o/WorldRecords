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
## by stroke
avg_days_stroke = wr_diff %>%
  group_by(Stroke) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_stroke$Avg = round(avg_days_stroke$Avg, 2)
avg_days_stroke

# plot it
# different color for the highest one
stroke_colors = ifelse(avg_days_stroke$Stroke == "Free", "#28BB8E", "#0284c7")
stroke_text = ifelse(avg_days_stroke$Stroke == "Free", avg_days_stroke$Avg, "")

avg_days_stroke_plot = plot_ly(
  data = avg_days_stroke,
  x = ~Stroke,
  y = ~Avg,
  type = "bar",
  text = stroke_text,
  marker = list(
    color = stroke_colors
  )
) %>%
  layout(
    title = "Avg Days Passed Between WRs by Stroke",
    xaxis = list(
      title = "Stroke"
    ),
    yaxis = list(
      title = "Days"
    )
  )
avg_days_stroke_plot

# break it up by sex
# avg_days_stroke_sex = wr_diff %>%
#   group_by(Stroke, Sex) %>%
#   summarise(Avg_Days = round(mean(DaysPassed, na.rm = TRUE), 2), 
#             .groups = "drop") %>%
#   pivot_wider(
#     names_from = Sex,
#     values_from = Avg_Days,
#     names_prefix = "Avg_"
#   )
# avg_days_stroke_sex

avg_days_stroke_F = wr_diff %>%
  filter(Sex == "F") %>%
  group_by(Stroke) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_stroke_F$Avg = round(avg_days_stroke_F$Avg, 2)
avg_days_stroke_F

avg_days_stroke_M = wr_diff %>%
  filter(Sex == "M") %>%
  group_by(Stroke) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_stroke_M$Avg = round(avg_days_stroke_M$Avg, 2)
avg_days_stroke_M

# # plot it
# stroke_colors_F = ifelse(avg_days_stroke_sex$Stroke == "Free", "#28BB8E", "#0284c7")
# stroke_colors_M = ifelse(avg_days_stroke_sex$Stroke == "Free", "#E5E4E5", "#014F86")
# stroke_text_F = ifelse(avg_days_stroke_sex$Stroke == "Free", avg_days_stroke_sex$Avg_F, "")
# stroke_text_M = ifelse(avg_days_stroke_sex$Stroke == "Free", avg_days_stroke_sex$Avg_M, "")
# 
# avg_days_stroke_sex_plot = plot_ly(
#   data = avg_days_stroke_sex,
#   x = ~Stroke,
#   y = ~Avg_F,
#   type = "bar",
#   text = stroke_text_F,
#   marker = list(
#     color = stroke_colors_F
#   ),
#   name = "Female"
# ) %>%
#   add_trace(
#     y = ~Avg_M,
#     marker = list(
#       color = stroke_colors_M
#     ),
#     text = stroke_text_M,
#     name = "Male"
#   ) %>%
#   layout(
#     title = "Avg Days Passed Between WRs by Stroke and Sex",
#     xaxis = list(
#       title = "Stroke"
#     ),
#     yaxis = list(
#       title = "Days"
#     ),
#     showlegend = TRUE
#   )
# avg_days_stroke_sex_plot

## by event
avg_days_event = wr_diff %>%
  group_by(Event) %>%
  summarize(Avg = mean(DaysPassed, na.rm = TRUE)) %>%
  as.data.frame()
avg_days_event$Avg = round(avg_days_event$Avg, 2)
avg_days_event

# plot it
# set the axis order
xaxis_order = c("50 Free", "100 Free", "200 Free", "400 Free", "800 Free", "1500 Free",
                "50 Backstroke", "100 Backstroke", "200 Backstroke",
                "50 Breaststroke", "100 Breaststroke", "200 Breaststroke",
                "50 Butterfly", "100 Butterfly", "200 Butterfly",
                "200 IM", "400 IM")
event_colors = ifelse(avg_days_event$Event == "1500 Free", "#28BB8E", "#0284c7")
event_text = ifelse(avg_days_event$Event == "1500 Free", avg_days_event$Avg, "")

avg_days_event_plot = plot_ly(
  data = avg_days_event,
  x = ~Event,
  y = ~Avg,
  type = "bar",
  text = event_text,
  marker = list(
    color = event_colors
  )
) %>%
  layout(
    title = "Avg Days Passed Between WRs by Event",
    xaxis = list(
      title = "Event",
      categoryorder = "array",
      categoryarray = xaxis_order
    ),
    yaxis = list(
      title = "Days"
    )
  )
avg_days_event_plot

# what about just for the women's 400 Free? profile of days passed
days_passed_400FrW_min = wr_diff %>%
  filter(Event == "400 Free", Sex == "F") %>%
  filter(DaysPassed == min(DaysPassed, na.rm = TRUE))

days_passed_400FrW_max = wr_diff %>%
  filter(Event == "400 Free", Sex == "F") %>%
  filter(DaysPassed == max(DaysPassed, na.rm = TRUE))

days_passed_400FrW_avg = wr_diff %>%
  filter(Event == "400 Free", Sex == "F") %>%
  summarize(DaysPassed = mean(DaysPassed, na.rm = TRUE))

days_passed_400FrW_min
days_passed_400FrW_max
days_passed_400FrW_avg


#### most days passed ####

## longest time passed before breaking a record
longest_record = wr_diff %>%
  filter(!is.na(DaysPassed)) %>% 
  slice_max(DaysPassed, n = 1)
longest_record

## WRs that have most days and least days since previous one
# take last ranking instance of each event
# this didn't work at first because the rank column is a character
# let's make a new column to change to numbers, ignoring NAs (don't need the first instance)
wr_diff$Rank2 = as.numeric(na.omit(wr_diff$Rank))

# switch in the new Rank column
latest_event = wr_diff %>%
  # filter(!is.na(Rank2)) %>%
  group_by(Event, Sex) %>%
  filter(Rank2 == max(Rank2)) %>%
  ungroup()

# pull record that was recently broken longest after previous one
recent_longest = latest_event %>%
  filter(DaysPassed == max(DaysPassed))
View(recent_longest)

# pull record that was recently broken closest after previous one
recent_shortest = latest_event %>%
  filter(DaysPassed == min(DaysPassed))
View(recent_shortest)
# so these were set 0 days later which means it was the same day
# either in finals after prelims or semifinals after prelims

## Katie, Summer, Ariarne - the 400 Freestyle Preview
# for any stroke/event - total WRs
# for any stroke/event - average re-breaking (ie, breaking their own record)
# for 400 fr - who has broken it the most
# for 400 fr - how much did they break it by


