# Function calculating rough estimate of hours a day spent below 45 degrees F 
#   based on a cosine function.
#
# Input:  min: The day's minimum temperature
#         max: The day's maximum temperature
# Output: Number of hours spent below 45 degrees
# Implementation: Include source("hours_below.R") at beginning of main code

hours_below <- function(min, max) {
  if (min > 45) {
    return(0)
  } else if (max < 45) {
    return(24)
  }
  x <- acos((45 - mean(c(min, max)))/(max - mean(c(min, max))))
  return(((pi - x)/pi)*24)
}

hours_below2 <- function(df) {
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
