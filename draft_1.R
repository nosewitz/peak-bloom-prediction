


a <- 3
b <- 5
temp <- function(t) a * sin(t) + b


qplot(seq(0, 2 * pi, .01), sapply(seq(0, 2 * pi, .01), arccos_fun)) +
  geom_vline(xintercept = c(pi / 2, 3 * pi / 2),
             linetype = 2) +
  geom_hline(yintercept = 4)


tmax = a + b
tmin = -a + b

b = (tmax + tmin) / 2
a = (tmax - tmin) / 2

t : a * sin(t) + b = 4


# Define the maximum and minimum temperatures for the day
max_temp <- 55
min_temp <- 40

# Define the time period for the day (in hours)
time_period <- 24

# Define the threshold temperature below which we want to estimate the time spent
threshold_temp <- seq(from = min_temp, to = max_temp, length.out = 100)

# Define the sine function that we will use to estimate the time spent below the threshold temperature
cos_fun <- function(x) {
  (max_temp - mean(c(max_temp, min_temp)))*cos(x) - (threshold_temp - (mean(c(max_temp, min_temp))))
}

arccos_fun <- function(min, max, threshold) {
  acos((threshold - mean(c(min, max)))/(max - mean(c(min, max))))
}

((pi - (arccos_fun(min_temp, max_temp, threshold_temp)))/pi)*24

lin_fun <- function(x) {
  (x*(min_temp - max_temp)/pi) + (max_temp - threshold_temp)
}
((pi - (pi*(threshold_temp - max_temp)/(min_temp - max_temp)))/pi)*24


x <- seq(0, pi, .1)


plot(x, cos_fun(x))
abline(-4, 0)
plot(x, lin_fun(x))
plot(1:100, (((pi - (arccos_fun(min_temp, max_temp, threshold_temp)))/pi)*24)/(((pi - (pi*(threshold_temp - max_temp)/(min_temp - max_temp)))/pi)*24))
threshold_temp[12]
cos_fun(0)

# Use the sine function to estimate the time spent below the threshold temperature
time_spent_below_threshold <- time_period * integrate(sin_fun, 0, 2*pi)$value


# Print the estimated time spent below the threshold temperature
cat("The estimated time spent below", threshold_temp, "degrees is", round(time_spent_below_threshold, 2), "hours.")


# Define the maximum and minimum temperatures for the day
max_temp <- 70
min_temp <- 40

# Define the time period for the day (in hours)
time_period <- 24

# Define the threshold temperature below which we want to estimate the time spent
threshold_temp <- 45

# Define the sine function that we will use to estimate the time spent below the threshold temperature
sin_fun <- function(x) {
  (sin((pi/12)*x)+1)/2
}

# Use the sine function to estimate the time spent below the threshold temperature
time_spent_below_threshold <- time_period * integrate(sin_fun, 0, 2*(pi/12))$value

# Print the estimated time spent below the threshold temperature
cat("The estimated time spent below", threshold_temp, "degrees is", round(time_spent_below_threshold, 2), "hours.")


acos()