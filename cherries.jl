import Pkg;
Pkg.activate(".");
Pkg.instantiate()


using RCall
using DataFrames
using CSV
using Chain
using StatsBase
using StatsPlots
using GLM
using Metrics

const fpath = "peak-bloom-prediction" 

# append each .csv into 1 DataFrame. 
# Do this by reading the directory, "data", then appending it with "data\\"
data = CSV.read("data\\".*readdir("data")[6:end], DataFrame)

# restrict looking into four cities 
keyLocs(x::String) =  any( occursin.(x, ["washingtondc", "kyoto", "liestal", "vancouver"]))
cities = filter(:location => keyLocs, data) 

theme(:dark)
# Shows all days are somewhat trending down.
@df cities scatter(:year,
                   :bloom_doy,
                   group = :location,
                   smooth = true,
                   legend = :topright)


x = groupby(cities, :location)

p1 = scatter(x[1].year, x[1].bloom_doy, smooth = true, color = :blue, linewidth=3, label = "Kyoto")
p2 = scatter(x[2].year, x[2].bloom_doy, smooth = true, color = :red, linewidth=3, label = "Liestal")
p3 = scatter(x[3].year, x[3].bloom_doy, smooth = true, color = :pink, linewidth=3, label = "Vancouver")
p4 =scatter(x[4].year, x[4].bloom_doy, smooth = true, color = :green, linewidth=3, label = "Washington D.C.")


plot(p1,p2,p3,p4, xlims = (1935, 2030), ylims = (70, 130) )


# Adding covariates
#  (Washington D.C.),  (Liestal),  (Kyoto), and 
# stationcodes = ["USC00186350","GME00127786","JA000047759","CA001108395"]

R"""

library(rnoaa)
library(tidyverse)

get_temperature <- function (stationid) {
     ghcnd_search(stationid = stationid, var = c("tmax"),
                  date_min = "1950-01-01", date_max = "2023-01-31")[[1]] %>%
     mutate(year = as.integer(format(date, "%Y")),
            month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
            season = cut(month, breaks = c(0, 2, 5, 8, 11),
                         include.lowest = TRUE,
                         labels = c("Winter", "Spring", "Summer", "Fall")),
            year = if_else(month == 0, year + 1L, year)) %>%
     group_by(year, season) %>%
     summarize(tmax_avg = mean(tmax, na.rm = TRUE))
   }

   historic_temperatures <-
   tibble(location = "washingtondc", get_temperature("USC00186350")) |>
   bind_rows(tibble(location = "liestal", get_temperature("GME00127786"))) |>
   bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"))) |>
   bind_rows(tibble(location = "vancouver", get_temperature("CA001108395")))
"""


bloom_data = @chain begin cities
   select(_, :location, :year, :bloom_doy)
   rename(_, :bloom_doy => :doy) 
end

@rput bloom_data


@rget historic_temperatures

@df historic_temperatures scatter(:year, :tmax_avg, group = :location) 





plot(historic_temperatures.year, historic_temperatures.tmax_avg)

seasons = unique(historic_temperatures.season)
locs = ["kyoto",	"liestal",	"washingtondc",	"vancouver"]


f(df, season, loc) = subset(df, :location => ByRow(n -> n == loc), :season => ByRow(n -> n ==season) )

sbs = [ f(historic_temperatures, season, loc) for season in seasons, loc in locs ]

plts = [ plot(d.year, d.tmax_avg, color = i % 4) for (i,d) in enumerate(sbs)] 

plot(plts..., xlims = (1950, 2030), ylims = (-10, 200), ) 


### Cumulative sum models
### Run professor code:
### 





### Linear Model
### Estimate weather



winter_spring = filter( :season => x-> any( x .== ("Winter", "Spring")), historic_temperatures)

ols = lm(@formula(tmax_avg ~ season*year + location), winter_spring)
ols2 = lm(@formula(tmax_avg ~ season*year + location), historic_temperatures)

ftest(ols.model)

forecast = @chain begin 
    Iterators.product(["Winter", "Spring"], locs, 1950:2032)
    collect 
    DataFrame
    rename(_, :1 => :season, :2 => :location, :3 => :year)
    #subset(_, :year => ByRow( <=(2022)))
end

forecast.predicted_tmax_avg = predict(ols2, forecast)
prs = unstack(forecast, :season, :predicted_tmax_avg)

@chain begin prs
  leftjoin(_, cities, on = ["location", "year"] )
  subset(_, :year => ByRow( <=(2022)))
  lm(@formula(bloom_doy ~ Winter*Spring), _)
  predict(_, unstack(forecast, :season, :predicted_tmax_avg) )
  #prs.np = convert.(Int, round.(_))
end

preds =  @chain begin prs
  leftjoin(_, cities, on = ["location", "year"] )
  subset(_, :year => ByRow( x -> 2023 > x > 2010 ) )
end


@chain begin preds
  select(_, :year, :location, :np)
  unstack(_, :location, :np)
end


# EVALUATE USING MAE, MSE, etc using Metrics.jl







@df preds plot(:year, :np, group = :location)