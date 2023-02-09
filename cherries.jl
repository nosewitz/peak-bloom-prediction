using Pkg; Pkg.activate("."); 
# Pkg.add(["RCall", "DataFrames", "CSV"])

using RCall
using DataFrames
using CSV

# append each .csv by reading the directory, "data" and appending it with data\\, create 1 dataframe from all results
CSV.read.("data\\".*readdir("data")[6:end], DataFrame)


