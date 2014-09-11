library("RJSONIO")
require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("ggmap")
library("stringr")

# Use Rstudio to load the R Project file.  It will bring the working directory to the directory the file is in and load any .Rdata files for the project. 

# The layer arguement in readOGR doesn't like directory paths or extensions. So the
# working directory needs to be changed. 
oldWD <- getwd()
setwd(paste0(oldWD, "/data/DNR_Trails_KMZ"))
chugach_trails     <- readOGR(dsn = "doc.kml", layer = "Trails")
colnames(chugach_trails@data)[1] <- "TRAIL_NAME"

setwd(paste0(oldWD, "/data/Muni_Trails_Shapefile"))
anc_trails <- readOGR(dsn=".", layer="trails")
anc_trails <- spTransform(anc_trails, CRS("+init=epsg:4326")) 

setwd(oldWD)

#trail variables are in dat as a dataframe. trail segments are in lines as a list. Both need to be manipulated for cleaning 
dat <- rbind.fill(data.frame(TRAIL_NAME = chugach_trails@data$TRAIL_NAME), anc_trails@data)
dat <- cbind(dat, new_id = seq(dim(dat)[1]))

lines <- c(chugach_trails@lines, anc_trails@lines)

##########DATA CLEANING############

remove trail names with "Unnamed" or NA
cleaning_vector <-  is.na(dat$TRAIL_NAME) | dat$TRAIL_NAME == "Unnamed" 

dat  <- dat[!cleaning_vector,]
lines <- lines[!cleaning_vector]

remove all trails on roads
road_trails <- str_detect(dat$TRAIL_NAME, "Ave Trail") | 
    str_detect(dat$TRAIL_NAME, "Street Trail") |
    str_detect(dat$TRAIL_NAME, "Bvld.") |
    str_detect(dat$TRAIL_NAME, "Ave. Trail") |
    str_detect(dat$TRAIL_NAME, "Road") |
    str_detect(dat$TRAIL_NAME, "Rd.") |
    str_detect(dat$TRAIL_NAME, "ROAD") |
    str_detect(dat$TRAIL_NAME, "St.") |
    str_detect(dat$TRAIL_NAME, "Jewell Lake") |
    str_detect(dat$TRAIL_NAME, "Dimond (South Side)") |
    str_detect(dat$TRAIL_NAME, "Dimond (North Side)") |
    str_detect(dat$TRAIL_NAME, "Macinnes St. Trail") |
    str_detect(dat$TRAIL_NAME, "Lake Otis Pkwy. Trail") |
    str_detect(dat$TRAIL_NAME, "St Trail") |
    str_detect(dat$TRAIL_NAME, "Pkwy") |
    str_detect(dat$TRAIL_NAME, "Dr.") 


dat  <- dat[!road_trails,]
lines <- lines[!road_trails]

###################################

dat$MANAGEMENT <- as.character(dat$MANAGEMENT)
dat$MANAGEMENT[is.na(dat$MANAGEMENT)] <- "NA"
dat$MANAGEMENT <- factor(dat$MANAGEMENT)
managers <- levels(dat$MANAGEMENT)
stewardID <- seq(managers)


trail_segments <- list(type = "FeatureCollection",
                       features = x <- vector(mode = "list", length = dim(dat)[1]))


for(i in seq(dim(dat)[1])) {
    
    trail_segments$features[[i]] <- list(geometry = list(type = "MultiLineString",
                                                         coordinates = lines[[i]]@Lines[[1]]@coords),
                                         id = as.character(dat$new_id[i]),
                                         properties = list(steward = dat$MANAGEMENT[i],
                                                           source = "TestOrganization",
                                                           trail1 = as.character(dat$TRAIL_NAME[i]), 
                                                           motor_vehicles = "no",
                                                           foot = "yes",
                                                           bicycle = "yes",
                                                           horse = "no",
                                                           ski = "yes",
                                                           wheelchair = "no",
                                                           osm_tags = NA),
                                         type = "Feature"

    )
}
# write trail_segments.geojson
fileConn<-file("output files/trailsy standard/trail_segments.geojson")
writeLines(toJSON(trail_segments, digits = 9), fileConn)
close(fileConn)
# Two segments are left out without cleaning. One is left out with cleaning. 
for(i in seq(length(lines))) {
    if(length(lines[[i]]@Lines) != 1)
    {print(i)} 
}

named_trails <- data.frame(id = dat$ROUTEID,
                           name = dat$TRAIL_NAME, 
                           steward = "NPS",
                           source = "NPS",
                           length = 1.4,
                           description = "TestDescription",
                           part_of = dat$SYSTEM_NAM)
# write named_trails.csv
write.csv(named_trails, file = "output files/trailsy standard/named_trails.csv")


trailheads <- list(type = "FeatureCollection",
                   features = x <- vector(mode = "list", length = dim(dat)[1]))


for(i in seq(dim(dat)[1])) {
    
    trailheads$features[[i]] <- list(type = "Feature",
                                     geometry = list(type = "Point",
                                                      coordinates = lines[[i]]@Lines[[1]]@coords[1,]),
                                     properties = list(name = as.character(dat$TRAIL_NAME[i]),
                                                       trail1 = as.character(dat$TRAIL_NAME[i]),
                                                                       id = dat$ROUTEID[i],
                                                                       trail_ids = dat$new_id[i],
                                                                       steward_id = as.character(dat$MANAGEMENT[i]),
                                                                       address = character(),
                                                                       parking = character(),
                                                                       drinkwater = character(),
                                                                       source = "TestOrganization",
                                                                       restrooms = character(),
                                                                       kiosk = character(),
                                                                       osm_tags = character())
                                     )
    
}
#validate "Point"
for(i in seq(dim(dat)[1])) {
    if(length(lines[[i]]@Lines[[1]]@coords[1,]) != 2 )
    {print(i)}
}


# write trail_segments.geojson
fileConn<-file("output files/trailsy standard/trailheads.geojson")
writeLines(toJSON(trailheads, digits = 9), fileConn)
close(fileConn)

# Create stewards.csv
stewards <- data.frame(name = managers,
                       id = stewardID,
                       url = "",
                       phone = "",
                       address = "",
                       publisher = "yes",
                       license = "none"
)
# write stewards.csv
write.csv(stewards, file = "output files/trailsy standard/stewards.csv")

zip_dir <- c("output files/trailsy standard")
zip("output files/open_trails_anchorage_trails.zip", files = paste(zip_dir, list.files(zip_dir), sep = "/"))
