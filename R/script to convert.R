#install.packages("httr")
require(httr)

source("R/togeojson.R")

#file to convert
file <- "DNR_Trails_KMZ/chugachstatepark.kmz"

#convert kmz to geojson
togeojson(file, "R/Chugach_State_Park_Trails.geojson")
