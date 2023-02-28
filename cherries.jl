import Pkg;
Pkg.activate(".");
Pkg.instantiate()


using RCall
using DataFrames, CSV
using Chain, Tidier
using StatsBase, Metrics, GLM
using StatsPlots
using Metrics


# append each .csv into 1 DataFrame. 
# Do this by reading the directory, "data", then appending it with "data\\"
data = CSV.read("data\\".*readdir("data")[6:end], DataFrame)

# restrict looking into four cities 
keyLocs(x::String) =  x in ["washingtondc", "kyoto", "liestal", "vancouver"]
cities = filter(:location => keyLocs, data) 

theme(:dark)
# Shows all days are somewhat trending down.
@df cities scatter(:year,
                   :bloom_doy,
                   group = :location,
                   smooth = true,
                   legend = :topright)


x = @chain cities begin 
  @filter(year>1940)
  @group_by(location)
end

p1 = scatter(x[1].year, x[1].bloom_doy, smooth = true, color = :blue, linewidth=3, label = "Kyoto")
p2 = scatter(x[2].year, x[2].bloom_doy, smooth = true, color = :red, linewidth=3, label = "Liestal")
p3 = scatter(x[3].year, x[3].bloom_doy, smooth = true, color = :pink, linewidth=3, label = "Washington D.C.")



plot(p1,p2,p3, xlims = (1935, 2030), ylims = (70, 130) )


# Adding covariates
#  (Washington D.C.),  (Liestal),  (Kyoto), and Vancouver
stationcodes = ["USC00186350","GME00127786","JA000047759","CA001108395"]


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



# Growing degree days approach
R"""

search_minmax <- function(id) { 
  ghcnd_search(stationid = id,
  var = c("tmax", "tmin"),
  date_min = "1900-01-01",
  date_max = "2023-01-01")
}

adjust <- function(table) { 
  left_join(table$tmax, table$tmin, by = c("id", "date")) |>
  mutate(temp = (tmax + tmin) / 20,
        year = as.integer(format(date, "%Y")),
        month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
        season = cut(month, breaks = c(0, 2, 5, 8, 11),
        include.lowest = TRUE,
        labels = c("Winter", "Spring", "Summer", "Fall")),
        year = if_else(month == 0, year + 1L, year)) |>
  select(year, date, temp)
}

temps <-
tibble(location = "washingtondc", search_minmax("USW00013743") |> adjust() ) |>
bind_rows(tibble(location = "liestal", search_minmax("SZ000001940") |> adjust() )) |>
bind_rows(tibble(location = "kyoto", search_minmax("JA000047759") |> adjust() )) |>
bind_rows(tibble(location = "vancouver", search_minmax("CA001108447") |> adjust() ))
"""


# Bloom data is used for joining
bloom_data = @chain begin cities
  @filter(year > 1940)
  @select(location, year, bloom_doy)
  @rename(doy = bloom_doy) 
end

# Put into R context
@rput bloom_data

# Store results of cumulative sums inside dataframe
R"""
GDD <- temps %>%
     mutate(temp = ifelse(is.na(temp), 0, temp)) %>%
     group_by(year,location) %>%
     nest() %>%
     left_join(bloom_data) %>%
     mutate(temp_sum = map(data, function(df) cumsum(df$temp)[doy]),
            temp2_sum = map(data, function(df) cumsum(df$temp^2)[doy]),
                     temp3_sum = map(data, function(df) cumsum(df$temp^3)[doy]),
            row_num  = map(data, nrow)) %>%
     unnest(c(temp_sum, temp2_sum, temp3_sum, row_num)) %>%
     filter(row_num > 364)
"""


# Retrieve data from approaches
@rget historic_temperatures
@rget GDD






@df historic_temperatures scatter(:year, :tmax_avg, group = :location) 

@df GDD scatter(:year, :temp2_sum, group = :location, smooth = true)


seasons = unique(historic_temperatures.season)
locs = ["kyoto",	"liestal",	"washingtondc"]

function format_preds(data, vn_data)
  st = unstack(data, :year, :location, :np)

  return @chain vn_data begin                                                                                                                                                                                                      
    unstack(:location, :np)                                                                                                                                                                                          
    hcat(st, makeunique=true)
    @select(year, kyoto, liestal, washingtondc, vancouver)
  end
end

function forecast_data()
  vn = DataFrame(location = repeat(["vancouver"], 10), year = 2023:2032)
  dm = DataFrame( location = repeat(locs, 10), year = repeat(2023:2032, inner = 3)) 
  return (dm,vn)
end
### Cumulative sum models
### Run professor code:
### 

# DF to estimate temp_sums required for extrapolating to 2023-2032



dm, vn = forecast_data()

est1 = lm(@formula(temp_sum ~ location*year), GDD)
est2 = lm(@formula(temp2_sum ~ location*year), GDD)
est3 = lm(@formula(temp3_sum ~ location*year), GDD)

est11 = lm(@formula(temp_sum ~ year), GDD)
est22 = lm(@formula(temp2_sum ~ year), GDD)
est33 = lm(@formula(temp3_sum ~ year), GDD)


dm.temp_sum =  predict(est1, dm)
dm.temp2_sum =  predict(est2, dm)
dm.temp3_sum =  predict(est3, dm)

vn.temp_sum = predict(est11, vn)
vn.temp2_sum = predict(est22, vn)
vn.temp3_sum = predict(est33, vn)


m1 = lm(@formula( doy ~ temp_sum + location), GDD) 
m2 = lm(@formula( doy ~ temp_sum * location), GDD) 
#m4 = lm(@formula( doy ~ temp_sum*lat*long), GDD) |> r2


m5 = lm(@formula(doy ~  location*year + temp_sum + temp2_sum), GDD) 
m6 = lm(@formula(doy ~  location*year + temp_sum + temp2_sum + temp3_sum), GDD) 

m7 = lm(@formula(doy ~  year + temp_sum + temp2_sum + temp3_sum), GDD) 

dm.np = predict(m6, dm) .|> round .|> x -> convert(Int, x)
vn.np = predict(m7,vn) .|>  round .|> x -> convert(Int, x)




format_preds(dm, vn)

ls = groupby(GDD, :location)

for (i,l) in enumerate(ls)

  abserr = select(l, Not([:row_num, :data])) |>
  data ->  predict(m6, data) |>
  pred -> mae(l.doy, pred) 
  println( "$(l.location[1]): $(round(abserr, digits = 3))")
end


### Chill hours model
###
R"""source("rol_chill.R")""" 
@rget chills 

DD = @chain bloom_data begin 
  rightjoin(_, chills, on=[:year ,:location])
  @filter(year>1940)
  #filter( :location => !=("vancouver"), _)
end


chill_regressor = lm( @formula(chill_hours ~ location*year), DD) # 0.43 r2
chill_regressor2 = lm( @formula(chill_hours ~ year ), DD)  

ids = findall(!ismissing, DD.doy)

c1 = lm(@formula( doy ~ chill_hours*location + year), DD[ids, :]) #|> adjr2  0.23
c12 = lm(@formula( doy ~ chill_hours*year), DD[ids, :])   


fore,vn = forecast_data()

fore.chill_hours = predict(chill_regressor, fore)
fore.np = predict(c1, fore) .|> round .|> x -> trunc(Int, x) 

vn.chill_hours = predict(chill_regressor2, vn)
vn.np = predict(c12, vn)

format_preds(fore,vn)

cop = @chain bloom_data begin 
  rightjoin(_, chills, on=[:year ,:location])
  @filter(year>1940)
  filter(:location => !=("vancouver"), _)
end

for (i,l) in enumerate(groupby(cop,  :location))

  ids = completecases(l)
  abserr = select(l, Not(:data)) |>
  data ->  predict(c1, data[ids, :]) |>
  pred -> mae(l.doy[ids], pred) 
  println( "$(l.location[1]) MAE: $(round(abserr, digits = 3))")
end

### Chill hours + GDD

DF = @chain DD begin
  leftjoin(GDD, on=[:location, :year], makeunique=true)
  select(Not( [:doy_1, :data_1, :data, :row_num] )  )
end

 lm(@formula( doy ~ location + chill_hours*temp_sum*year + temp2_sum), DF) |> adjr2



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


# This model yields R2 adj of 0.23
@chain begin prs
  leftjoin(_, cities, on = ["location", "year"] )
  subset(_, :year => ByRow( <=(2022)))
  lm(@formula(bloom_doy ~ Winter*Spring), _)
  predict(_, unstack(forecast, :season, :predicted_tmax_avg) )
  prs.np = convert.(Int, round.(_))
end

preds =  @chain begin prs
  leftjoin(_, cities, on = ["location", "year"] )
  subset(_, :year => ByRow( x -> 2023 > x > 2010 ) )
end


@chain begin preds
  select(_, :year, :location, :np)
  unstack(_, :location, :np)
end

