require(httr)

source("togeojson.R")

# file to convert

file <- "../data/Muni_Trails_Shapefile/trails.shp"

# convert kmz to geojson
togeojson(file, "../data/Muni_Trails_Shapefile/Anchorage_Muni_Trails.geojson")

