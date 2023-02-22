source("prof_email.R")
library(leaps)

x<-temp %>%
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

mod1 <- lm(x$doy ~ x$temp_sum)
mod2 <- lm(x$doy ~ x$temp2_sum)
mod3 <- lm(x$doy ~ x$temp3_sum)
modall <- lm(x$doy ~ x$temp_sum*x$temp2_sum*x$temp3_sum)
mod12 <- lm(x$doy ~ x$temp_sum*x$temp2_sum)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod12)
summary(modall)
predictions1 <- predict(mod1)
predictions2 <- predict(mod2)
predictions3 <- predict(mod3)
predictions12 <- predict(mod12)
predictionsall <- predict(modall)

mse <- function(real, predicted) {
  return(sum((real - predicted)^2)/length(real))
}


mse(x$doy, predictions1)
mse(x$doy, predictions2)
mse(x$doy, predictions3)
mse(x$doy, predictions12)
mse(x$doy, predictionsall)

bloom.regfit.full <- regsubsets(doy ~ temp_sum*temp2_sum*temp3_sum, x)
bloom.summary <- summary(bloom.regfit.full)
bloom.summary

