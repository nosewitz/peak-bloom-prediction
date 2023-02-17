library(tidyverse)

doy_last_frost <- function(tmax, doy_max = 100) {
  dof <- which(tmax[1:doy_max] < 0)
  if(length(dof) == 0) {1} else {max(dof) + 1}
}

doy_prediction <- function(temp, tmax)
  doy_last_frost(tmax) + which.max(cumsum(pmax(temp[(doy_last_frost(tmax) + 1):365], 0, na.rm = TRUE)^2) > 4264)

tmax_dc <- read_csv("https://www.ncei.noaa.gov/erddap/griddap/nmme_ccsm4_tasmax_day_r01_by_time_LAT_LON.csv?TREFMXAV%5B(2023-01-01T12:00:00Z):1:(2023-06-01T12:00:00Z)%5D%5B(39):1:(39)%5D%5B(77):1:(77)%5D", skip = 1)
tmin_dc <- read_csv("https://www.ncei.noaa.gov/erddap/griddap/nmme_ccsm4_tasmin_day_r01_by_time_LAT_LON.csv?TREFMNAV%5B(2023-01-01T12:00:00Z):1:(2023-06-01T12:00:00Z)%5D%5B(39):1:(39)%5D%5B(77):1:(77)%5D", skip = 1)

temp_dc <-
  tmax_dc %>%
  transmute(date = as.Date(UTC),
            tmax = Kelvin - 273.15) %>%
  left_join(tmin_dc %>%
              transmute(date = as.Date(UTC),
                        tmin = Kelvin - 273.15)) %>%
  mutate(temp = (tmax + tmin) / 2,
         year = format(date, "%Y"))

temp_dc %>%
  summarize(date_dc = doy_prediction(temp, tmax) + as.Date("2023-01-01")) %>%
  pull(date_dc)