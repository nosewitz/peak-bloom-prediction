import Pkg;
Pkg.activate("peak-bloom-prediction\\");
Pkg.instantiate()


using RCall
using DataFrames
using CSV
using Chain
using StatsBase
using StatPlots

const fpath = "peak-bloom-prediction" 

# append each .csv into 1 DataFrame. 
# Do this by reading the directory, "data", then appending it with "data\\"
data = CSV.read("$fpath\\data\\".*readdir("$fpath\\data")[6:end], DataFrame)

# restrict looking into four cities 
keyLocs(x::String) =  any( occursin.(x, ["washingtondc", "Japan/Kyoto", "Switzerland/Liestal", "vancouver"]))
cities = filter(:location => keyLocs, data) 

# Shows all days are somewhat trending down.
@df cities scatter(:year,
                   :bloom_doy,
                   group = :location,
                   smooth = true,
                   legend = :topright)

                   