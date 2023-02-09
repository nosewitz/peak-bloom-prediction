library("tidyverse")

# using nest to extract R-squared from multiple regressions
y <- rep(1:2, 50)
x <- rnorm(100)
z <- y + x + rnorm(100, sd = .1)

tibble(x, y, z) %>%
  group_by(y) %>%
  nest() %>%
  mutate(models = map(data, function(df) lm(z ~ x, data = df)),
         rsq = map(models, function(mod) summary(mod)$r.squared)) %>%
  unnest(rsq)

# using nest to calculate the cumulative temperature from January 1st to a bloom date 
library("rnoaa")
temp <- ghcnd_search(stationid = "USC00186350",
                     #refresh = TRUE,
                     var = c("tmax", "tmin"),
                     date_min = "1900-01-01",
                     date_max = "2023-01-01")

temp <-
  left_join(temp$tmax, temp$tmin, by = c("id", "date")) %>%
  mutate(temp = (tmax + tmin) / 20) %>%
  select(date, temp)

bloom_data <-
  tibble(year = 1948:2022,
         doy  = rpois(75, 100))

temp %>%
  mutate(temp = ifelse(is.na(temp), 0, temp),
         year = parse_number(format(date, "%Y"))) %>%
  group_by(year) %>%
  nest() %>%
  left_join(bloom_data) %>%
  mutate(temp_sum = map(data, function(df) cumsum(df$temp)[doy]),
         temp2_sum = map(data, function(df) cumsum(df$temp^2)[doy]),
         temp3_sum = map(data, function(df) cumsum(df$temp^3)[doy]),
         row_num  = map(data, nrow)) %>%
  unnest(c(temp_sum, temp2_sum, temp3_sum, row_num)) %>%
  filter(row_num > 364)
