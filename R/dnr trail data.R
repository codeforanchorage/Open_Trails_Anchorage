library(maptools)
require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

#tkml <- getKMLcoordinates(kmlfile="DNR_Trails_KMZ/doc.kml", ignoreAltitude=T)

setwd(paste(getwd(), "DNR_Trails_KMZ", sep = "/"))

#ogrListLayers("doc.kml")

Trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
Trailheads <- readOGR(dsn = "doc.kml", layer = "Trailheads")
Labels <- readOGR(dsn = "doc.kml", layer = "Labels")

#trail names
levels(Trails@data$Name)

