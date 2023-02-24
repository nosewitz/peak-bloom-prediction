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