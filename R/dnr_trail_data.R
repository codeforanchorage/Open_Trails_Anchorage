require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

# TODO: is this still needed?
# tkml <- getKMLcoordinates(kmlfile="DNR_Trails_KMZ/doc.kml", ignoreAltitude=T)

setwd("../data/DNR_Trails_KMZ")

Trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
Trailheads <- readOGR(dsn = "doc.kml", layer = "Trailheads")
Labels <- readOGR(dsn = "doc.kml", layer = "Labels")

#trail names
levels(Trails@data$Name)

Trails@data$id = rownames(Trails@data)
Trails.points = fortify(Trails, region="id")
Trails.df = join(Trails.points, Trails@data, by="id")

head(Trails.df)
# TODO: are we missing some data here?
# head(anc_trails.df)

# TODO: is this still needed?
# rbind.fill(df1, df2)

