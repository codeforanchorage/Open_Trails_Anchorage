require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

#tkml <- getKMLcoordinates(kmlfile="DNR_Trails_KMZ/doc.kml", ignoreAltitude=T)

setwd(paste(getwd(), "DNR_Trails_KMZ", sep = "/"))
setwd("C:/Users/chthompson/Desktop/Open_Trails_Anchorage/DNR_Trails_KMZ")
#see more layers in the kml file for chugach state par
#ogrListLayers("doc.kml")

Trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
Trailheads <- readOGR(dsn = "doc.kml", layer = "Trailheads")
Labels <- readOGR(dsn = "doc.kml", layer = "Labels")

#trail names
levels(Trails@data$Name)

Trails@data$id = rownames(Trails@data)
Trails.points = fortify(Trails, region="id")
Trails.df = join(Trails.points, Trails@data, by="id")

head(Trails.df)
head(anc_trails.df)


rbind.fill(df1, df2)
