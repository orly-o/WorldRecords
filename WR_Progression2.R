#### WR Progression ####
## May 2025 ##

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

#### first looks, manipulate as needed ####
# main descriptive things
nrow(wr)
ncol(wr)

# new columns to split up the distance and the stroke
# gosh i missed piping
wr = wr %>%
  separate("Event", 
           sep = " ", 
           into = c("Distance", "Stroke"), 
           remove = FALSE)
head(wr)

# separate out male and female datasets
wrm = wr[wr$Sex == "M",]
wrf = wr[wr$Sex == "F",]

#### early analysis ####
# how many swimmers are there?
males = n_distinct(wrm$Name)
males
females = n_distinct(wrf$Name)
females

# count of WRs by swimmer
# and graphs! yay graphs!
# men
wrcount_m = wrm %>%
  count(Name, sort = TRUE)
colnames(wrcount_m) = c("Name", "WR_Count")
head(wrcount_m, 5)

# graph it (top 10 maybe)
wrcount_m_plot = plot_ly(
  data = wrcount_m %>% slice_max(order_by = WR_Count, n = 20),
  x = ~Name,
  y = ~WR_Count,
  type = "bar"
) %>%
  layout(
    title = "Top 20 Swimmers by WR Count (M)",
    xaxis = list(
      title = "Swimmer",
      categoryorder = "total descending"),
    yaxis = list(
      title = "WR Count"
    )
)
wrcount_m_plot

# women
wrcount_f = wrf %>%
  count(Name, sort = TRUE)
colnames(wrcount_f) = c("Name", "WR_Count")
head(wrcount_f, 5)

wrcount_f_plot = plot_ly(
  data = wrcount_f %>% slice_max(order_by = WR_Count, n = 20),
  x = ~Name,
  y = ~WR_Count,
  type = "bar"
) %>%
  layout(
    title = "Top 20 Swimmers by WR Count (F)",
    xaxis = list(
      title = "Swimmer",
      categoryorder = "total descending"),
    yaxis = list(
      title = "WR Count"
    )
  )
wrcount_f_plot








