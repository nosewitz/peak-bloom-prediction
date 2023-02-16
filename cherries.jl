import Pkg;
#Pkg.activate("peak-bloom-prediction\\");
Pkg.instantiate()


using RCall
using DataFrames
using CSV
using Chain
using StatsBase
using StatsPlots

const fpath = "peak-bloom-prediction" 

# append each .csv into 1 DataFrame. 
# Do this by reading the directory, "data", then appending it with "data\\"
data = CSV.read("data\\".*readdir("data")[6:end], DataFrame)

# restrict looking into four cities 
keyLocs(x::String) =  any( occursin.(x, ["washingtondc", "Japan/Kyoto", "Switzerland/Liestal", "vancouver"]))
cities = filter(:location => keyLocs, data) 

theme(:lime)
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