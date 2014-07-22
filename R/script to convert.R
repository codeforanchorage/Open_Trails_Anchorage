#install.packages("httr")
require(httr)

source("R/togeojson.R")

#file to convert
file <- "Muni_Trails_Shapefile/trails.shp"

#convert kmz to geojson
togeojson(file, "R/Anchorage_Muni_Trails.geojson")


