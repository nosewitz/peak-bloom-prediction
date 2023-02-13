using Pkg; Pkg.activate("."); 
# Pkg.add(["RCall", "DataFrames", "CSV"])

using RCall
using DataFrames
using CSV
using Chain
using StatsBase





# append each .csv into 1 DataFrame. 
# Do this by reading the directory, "data", then appending it with "data\\"
data = CSV.read("data\\".*readdir("data")[6:end], DataFrame)

keyLocs(x::String) =  any( occursin.(x, ["washingtondc", "Japan/Kyoto", "Switzerland/Liestal", "vancouver"]))
cities = filter(:location => keyLocs, data) 
