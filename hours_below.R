# Function calculating rough estimate of hours a day spent below 45 degrees F 
#   based on a cosine function.
#
# Input:  data frame with tmin and tmax variables
# Output: Number of hours spent below 45 degrees
# Implementation: Include source("hours_below.R") at beginning of main code

hours_below <- function(df) {
  df %>% 
    mutate(daily_hours_below = -1, 
           daily_hours_below = ifelse(tmax <= 45, 24, daily_hours_below), 
           daily_hours_below = ifelse(tmin >= 45, 0, daily_hours_below), 
           daily_hours_below = ifelse(
             daily_hours_below < 0, 
             ((pi - acos((45 - (tmax + tmin)/2)/(tmax - (tmax + tmin)/2)))/pi)*24, 
             daily_hours_below)) %>% 
    summarise(cum_hours_below = sum(daily_hours_below))
}