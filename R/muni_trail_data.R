require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

setwd("../data/Muni_Trails_Shapefile")
ogrListLayers(".")

anc_trails = readOGR(dsn=".", layer="trails")
anc_trails@data$id = rownames(anc_trails@data)
anc_trails.points = fortify(anc_trails, region="id")
anc_trails.df = join(anc_trails.points, anc_trails@data, by="id")

anc_trails@data$id = rownames(anc_trails@data)
anc_trails.points = fortify(anc_trails, region="id")
anc_trails.df = join(anc_trails.points, anc_trails@data, by="id")
ggplot(anc_trails.df) + 
    aes(long,lat,group=group) + 
    geom_polygon() +
    geom_path(color="white") +
    coord_equal() +
    scale_fill_brewer("anc_trails Ecoregion")

# trail names
levels(anc_trails@data$TRAIL_NAME)

# data from named trails
anc_trails@data[!is.na(anc_trails@data$TRAIL_NAME),]

