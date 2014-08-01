require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("ggmap")

# EXCUSE ME GOOD SIR OR MADAM
# Use Rstudio to load the R Project file.  It will bring the working directory to the directory the file is in and load any .Rdata files for the project. 

# The layer arguement in readOGR doesn't like directory paths or extensions. So the
# working directory needs to be changed. 
oldWD <- getwd()
setwd(paste0(oldWD, "/data/DNR_Trails_KMZ"))
chugach_trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
chugach_trails@data$id = rownames(chugach_trails@data)
chugach_trails.points = fortify(chugach_trails, region="id")
chugach_trails.df = join(chugach_trails.points, chugach_trails@data, by="id")

setwd(paste0(oldWD, "/data/Muni_Trails_Shapefile"))
anc_trails <- readOGR(dsn=".", layer="trails")
anc_trails <- spTransform(anc_trails, CRS("+init=epsg:4326")) 
anc_trails@data$id = rownames(anc_trails@data)
anc_trails.points = fortify(anc_trails, region="id")
anc_trails.df = join(anc_trails.points, anc_trails@data, by="id")

head(chugach_trails.df)
head(anc_trails.df)

# remove empty description field
chugach_trails.df <- chugach_trails.df[, colnames(chugach_trails.df) != "Description"]
colnames(chugach_trails.df)[7] <- "TRAIL_NAME"
all_trails <- rbind.fill(chugach_trails.df, anc_trails.df)
unnamed_trails <- all_trails[all_trails$TRAIL_NAME &in& c("Unnamed", "Unnnamed"),]
NA_trails <- all_trails[which((is.na(all_trails$TRAIL_NAME))),]
NA_trails <- rbind(NA_trails, unnamed_trails)
named_trails <- all_trails[which((!is.na(all_trails$TRAIL_NAME))),]
named_trails <- named_trails[!named_trails$TRAIL_NAME %in% c("Unnamed", "Unnnamed"),]
                                                        
# Remove trails that are on roads (excluding highways). This is
# just a judgement call that people don't care about trails that
# follow city roads. 
notes <- read.csv("doc/notes on the data to clean.csv")
trails_to_remove <- notes[notes$first_notes == "weak trail",]$x
named_trails <- droplevels(named_trails[!named_trails$TRAIL_NAME %in% trails_to_remove,])
# Remove minitrails for now
trails_to_remove <- notes[notes$first_notes == "minitrail",]$x
named_trails <- droplevels(named_trails[!named_trails$TRAIL_NAME %in% trails_to_remove,])
# Look at trails that don't fit in the box of googlemaps 
use_bounding_box <- notes[notes$first_notes == "fix my plot to browse to zoom out",]$x
bounding_box <- droplevels(named_trails[named_trails$TRAIL_NAME %in% use_bounding_box,])

# look up close at the trail
for(i in seq(levels(bounding_box$TRAIL_NAME))) {
    one_trail <- bounding_box[which(bounding_box$TRAIL_NAME == levels(bounding_box$TRAIL_NAME)[i]),]
    map_center <- c(min(one_trail$long) - 0.04, min(one_trail$lat) - 0.04, max(one_trail$long) + 0.04, max(one_trail$lat) + 0.04)
    anc_map <- get_map(map_center, source = "osm")
    p <- ggmap(anc_map) +
        geom_point(data = one_trail, aes(x = long, y = lat, color = TRAIL_NAME), size = 1) +
        geom_line(data = one_trail, aes(x = long, y = lat, color = TRAIL_NAME, size = 1)) +
        ggtitle(levels(bounding_box$TRAIL_NAME)[i]) +
        theme(legend.position="none")
    ggsave(paste(i, ".png", sep = ""))
}
write.csv(levels(bounding_box$TRAIL_NAME), file = "long trails to give a system.csv")

# Group up the $SYSTEM_NAM into new $trail_system
trail_system <- rep(NA, dim(named_trails)[1])
trail_system[named_trails$SYSTEM_NAM %in% c("Kincaid Other Trails", "Kincaid Single Track", "Kincaid Ski Trails")] <- "Kincaid"
trail_system[named_trails$SYSTEM_NAM %in% c("Girdwood Trails")] <- "Girdwood"
trail_system[named_trails$SYSTEM_NAM %in% c("Johnson Trail")] <- "Seward Highway"
trail_system[named_trails$SYSTEM_NAM %in% c("Rabbit Creek Greenbelt", "Ruth Arcand Trails")] <- "South Anchorage Hillside"
trail_system[named_trails$SYSTEM_NAM %in% c("Powerline")] <- "Glen Alps"
trail_system[named_trails$SYSTEM_NAM %in% c("Gasline Trail", "Abbot Trail", "Abbott Loop Trail", "Powerline", "Hillside Ski Trails", "Hillside Trail System", "Mt Bike Single Track")] <- "Bicentenial Park"
trail_system[named_trails$SYSTEM_NAM %in% c("Mirror Lake Ski Trails", "Beach Lake Sled Dog", "Beech Lake Ski Trails")] <- "Peters Creek"
trail_system[named_trails$SYSTEM_NAM %in% c("Russian Jack Ski Trails")] <- "UAA, APU, and Airport Heights"
trail_system[named_trails$SYSTEM_NAM %in% c("Campbell Creek Trail")] <- "Campbell Creek"
trail_system[named_trails$SYSTEM_NAM %in% c("Connors Bog Skijor")] <- "Connor's Bog"
trail_system[named_trails$SYSTEM_NAM %in% c("Ship Creek Trail", "Coastal Trail")] <- "Coastal Trail"
trail_system[named_trails$SYSTEM_NAM %in% c("Glenn Hwy Trail")] <- "Glenn Hwy"
trail_system[named_trails$SYSTEM_NAM %in% c("Centennial Park Trails")] <- "Centenial Parks"
trail_system[named_trails$SYSTEM_NAM %in% c("Bartlett Ski Trails")] <- "Bartlett"
trail_system[named_trails$SYSTEM_NAM %in% c("Chester Creek Trail")] <- "Chester Creek"
named_trails <- cbind(named_trails, trail_system)
# Did I miss any (besides Tour de Anchorage)? Nope
levels(droplevels(named_trails[is.na(named_trails$trail_system),]$SYSTEM_NAM))
# Look at trails that aren't in a system yet
no_category_yet <- droplevels(named_trails[is.na(trail_system),])
levels(no_category_yet$TRAIL_NAME)


# look up close at the trail
for(i in seq(levels(no_category_yet$TRAIL_NAME))) {
one_trail <- named_trails[which(no_category_yet$TRAIL_NAME == levels(named_trails$TRAIL_NAME)[i]),]
map_center <- c(min(one_trail$long) - 0.02, min(one_trail$lat) - 0.02, max(one_trail$long) + 0.02, max(one_trail$lat) + 0.02)
anc_map <- get_map(map_center)
p <- ggmap(anc_map) +
    geom_point(data = one_trail, aes(x = long, y = lat, color = TRAIL_NAME), size = .01) +
    geom_line(data = one_trail, aes(x = long, y = lat, color = TRAIL_NAME, size = .01)) +
    ggtitle(levels(named_trails$TRAIL_NAME)[i]) +
    theme(legend.position="none")
ggsave(paste(i, ".png", sep = ""))
Sys.sleep(4)
}

# look where in Anchorage the systems all ready exist
anc_map <- get_map(c(lat = 61.2, lon = -149.76), zoom = 9)
for(i in seq(levels(named_trails$TRAIL_NAME))) {
ggmap(anc_map) +
    geom_line(data = named_trails[named_trails$SYSTEM_NAM == levels(named_trails$SYSTEM_NAM)[i],],
              aes(x = long, y = lat, color = TRAIL_NAME, group = TRAIL_NAME), size = .5) +
    ggtitle(levels(named_trails$SYSTEM_NAM)[i]) +
ggsave(paste("whichTrailSystem", i, ".png", sep = ""))
}
