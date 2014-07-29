require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("ggmap")

setwd("../data/DNR_Trails_KMZ")
chugach_trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
chugach_trails@data$id = rownames(chugach_trails@data)
chugach_trails.points = fortify(chugach_trails, region="id")
chugach_trails.df = join(chugach_trails.points, chugach_trails@data, by="id")

setwd("../Muni_Trails_Shapefile")
anc_trails <- readOGR(dsn=".", layer="trails")
anc_trails <- spTransform(anc_trails, CRS("+init=epsg:4326")) 
anc_trails@data$id = rownames(anc_trails@data)
anc_trails.points = fortify(anc_trails, region="id")
anc_trails.df = join(anc_trails.points, anc_trails@data, by="id")

head(chugach_trails.df)
head(anc_trails.df)

# remove empty description field
chugach_trails.df <- chugach_trails.df[,1:7]
colnames(chugach_trails.df)[7] <- "TRAIL_NAME"
all_trails <- rbind.fill(chugach_trails.df, anc_trails.df)
NA_trails <- all_trails[which((is.na(all_trails$TRAIL_NAME))),]
named_trails <- all_trails[which((!is.na(all_trails$TRAIL_NAME))),]

# browse trails
for(i in seq(levels(named_trails$TRAIL_NAME))) {
one_trail <- named_trails[which(named_trails$TRAIL_NAME == levels(named_trails$TRAIL_NAME)[i]),]
map_center <- c(min(one_trail$long) - 0.02, min(one_trail$lat) - 0.02, max(one_trail$long) + 0.02, max(one_trail$lat) + 0.02)
anc_map <- get_map(map_center)
p <- ggmap(anc_map) +
    geom_point(data = one_trail, aes(x = long, y = lat, color = TRAIL_NAME)) +
    ggtitle(levels(named_trails$TRAIL_NAME)[i]) +
    theme(legend.position="none")
ggsave(paste(i, ".png", sep = ""))
}

