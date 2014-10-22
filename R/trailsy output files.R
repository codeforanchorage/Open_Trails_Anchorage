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
    
    setwd(paste0(oldWD, "/data/Mat_Su_Shapefile"))
    matsu_trails <- readOGR(dsn=".", layer="MSB_Trails_Legal_Aug2014")
    matsu_trails <- spTransform(matsu_trails, CRS("+init=epsg:4326")) 
    colnames(matsu_trails@data) <- c("OBJECTID", "Shape_Leng", "TRAIL_NAME", "NAME_2", "TYPE", "USE", 
                                     "SEASON", "PLAN", "PLAN_ID", "LGL_ESMT", "ESMT_DOC", "SOURCE", 
                                     "SURVEY_DAT", "SURVEY_MET", "QC", "QC_2", "LENGTH_MI", "LENGTH_FT", 
                                     "NOTES")
    
    setwd(oldWD)
    
    #trail variables are in dat as a dataframe. trail segments are in lines as a list. Both need to be manipulated for cleaning 
    dat <- rbind.fill(data.frame(TRAIL_NAME = chugach_trails@data$TRAIL_NAME), anc_trails@data, matsu_trails@data)
    dat <- cbind(dat, new_id = seq(dim(dat)[1]))
    
    lines <- c(chugach_trails@lines, anc_trails@lines, matsu_trails@lines)
    
    ##########DATA CLEANING############
    
    #remove trail names with "Unnamed" or NA
    cleaning_vector <-  is.na(dat$TRAIL_NAME) | dat$TRAIL_NAME == "Unnamed" 
    
    dat  <- dat[!cleaning_vector,]
    lines <- lines[!cleaning_vector]
    
    #remove all trails on roads
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
        str_detect(dat$TRAIL_NAME, "Eklutna ATV Access") |
        str_detect(dat$TRAIL_NAME, "Peak Spor RR") |
        str_detect(dat$TRAIL_NAME, "Fish Creek") |
        str_detect(dat$TRAIL_NAME, "Mt. View Sch. Trail") |
        str_detect(dat$TRAIL_NAME, "Mchugh Creek Pedestrian") |
        str_detect(dat$TRAIL_NAME, "Bragaw") |
        str_detect(dat$TRAIL_NAME, "Beaver Pl. Trail") |
        str_detect(dat$TRAIL_NAME, "Seward Hwy. Trail") |
        str_detect(dat$TRAIL_NAME, "RABBIT CREEK PARK TRAIL") |
        str_detect(dat$TRAIL_NAME, "Seward Hwy. Trail") |
        str_detect(dat$TRAIL_NAME, "Gruening Sch. Trail") |
        str_detect(dat$TRAIL_NAME, "Dimond (North Side)") |
        str_detect(dat$TRAIL_NAME, "Beaver Pl. Trail") |
        str_detect(dat$TRAIL_NAME, "Dr.") 

    dat  <- dat[!road_trails,]
    lines <- lines[!road_trails]    
    
    #more_bad_trails are trails that didn't match the pattern above that are still roads or 
    #elementary school trails or duplicates or just not worthy.
    more_bad_trails <- c("Bragaw", "SLEDDING HILL", "Loop", "Old Sweard Hwy. Trail", "Mt. View Sch. Trail", "Wonder Park Sch. Trail",
                         "Galdys Wood Trail", "Dimond (North Side)", "Dimond (South Side)", "Seward Hwy Trail", "Rondy Cut Off Trail", 
                         "14 mile loop", "Raspberry Parking Lot Connector", "New Seward Hwy. Trail", "Shortcut", "Ski Trail", "RABBIT CREEK PARK TRAIL", 
                         "Alder Trail", "Cannonrd", "Middle Fork", "School", "Glenn Alps View Loop", "Glenn Alps Powerline B", "Glenn Alps Powerline A", 
                         "Flattop Peak Alternate", "Flattop Option", "Ballfield", "Margeurite Hills", "Tranagain Arm Interpretive", "Baldy Traverse", "Trail 7", 
                         "Track 3", "Soccer Field", "N Connect", "Logjam Trail", "S Connect", "School Track", "Go Again")
    
    
    
    %in% dat$TRAIL_NAME
    
    

    
    #dat <- dat[c(102),]
    #lines <- lines[c(102)]
    ###################################
    
    dat$MANAGEMENT <- as.character(dat$MANAGEMENT)
    dat$MANAGEMENT[is.na(dat$MANAGEMENT)] <- "NA"
    dat$MANAGEMENT <- factor(dat$MANAGEMENT)
    managers <- levels(dat$MANAGEMENT)
    stewardID <- seq(managers)
    
    
    trails <- levels(factor(dat$TRAIL_NAME))
    
    
    
    trail_segments <- list(features = x <- vector(mode = "list", length = length(trails)),
                           type = "FeatureCollection")
    
    for(i in seq(trails)) {

        lines_rows <- which(dat$TRAIL_NAME == trails[i])
        
        turtles <- lapply(lines[lines_rows], function(x) { x@Lines[[1]]@coords})    
        
        trail_segments$features[[i]] <- list(geometry = list(coordinates = list(turtles),
                                                                       type = "MultiLineString"),
                                             id = i,
                                             properties = list(id = i,
                                                               source_id = 3,
                                                               steward_id = 3,
                                                               length = 1.4, 
                                                               trail1 = trails[i],
                                                               trail2 = NULL,
                                                               trail3 = NULL,
                                                               trail4 = NULL,
                                                               trail5 = NULL,
                                                               trail6 = NULL,
                                                               accessible = NULL, 
                                                               roadbike = NULL, 
                                                               hike = "y", 
                                                               mtnbike = "y", 
                                                               equestrian = NULL, 
                                                               xcntryski = NULL,
                                                               conditions = NULL, 
                                                               trlsurface = NULL,
                                                               dogs = NULL, 
                                                               source = "TestOrganization", 
                                                               source_fullname = "Made up for testing",
                                                               source_phone = "999-999-9999", 
                                                               source_url = "http://www.google.com",
                                                               steward = "TestOrganization",
                                                               steward_fullname = as.character(dat$MANAGEMENT[i]),
                                                               steward_phone = "999-999-9999", 
                                                               steward_url = "http://www.google.com"),
                                             type = "Feature")
    }
    # write trail_segments.geojson
    fileConn <- file("output files/trailsy standard/trailsegments.json")
    writeLines(toJSON(trail_segments, digits = 9), fileConn)
    close(fileConn)
    # Two segments are left out without cleaning. One is left out with cleaning. 
    for(i in seq(length(lines))) {
        if(length(lines[[i]]@Lines) != 1)
        {print(i)} 
    }
    
    ninja <- dat[!duplicated(dat$TRAIL_NAME),]
    
    
    named_trails <- data.frame(id = seq(trails),
                               name = ninja$TRAIL_NAME, 
                               steward = "TestOrganization",
                               source =  "TestOrganization",
                               length = 1.4,
                               description = "TestDescription",
                               part_of = "dat$SYSTEM_NAME")
    # write named_trails.csv
    write.csv(named_trails, file = "output files/trailsy standard/named_trails.csv", row.names = F)
    
    
    trailheads <- list(type = "FeatureCollection",
                       features = x <- vector(mode = "list", length = length(trails)))
    
    
    for(i in seq(trails)) {
        
        lines_rows <- which(dat$TRAIL_NAME == trails[i])
        
        turtles <- lapply(lines[lines_rows], function(x) { x@Lines[[1]]@coords})    
        
        trailheads$features[[i]] <- list(type = "Feature",
                                         geometry = list(type = "Point",
                                                         coordinates = turtles[[1]][1,]),
                                         properties = list(id = i,
                                                           name = trails[i],
                                                           source_id = 3,
                                                           steward_id = 3,
                                                           length = 1.4, 
                                                           trail1 = trails[i],
                                                           accessible = NULL, 
                                                           roadbike = NULL, 
                                                           hike = "y", 
                                                           mtnbike = NULL, 
                                                           equestrian = NULL, 
                                                           xcntryski = NULL,
                                                           conditions = NULL, 
                                                           trlsurface = NULL,
                                                           dogs = NULL, 
                                                           source  = "TestOrganization", 
                                                           steward = "TestOrganization",
                                                           source_fullname = "Made up for testing",
                                                           source_phone = "999-999-9999", 
                                                           source_url = "http://www.google.com",
                                                           steward_fullname = as.character(dat$MANAGEMENT[i]),
                                                           steward_phone = "999-999-9999", 
                                                           steward_url = "http://www.google.com"),
                                                           id = i)
        
    }
    #validate "Point"
    for(i in seq(dim(dat)[1])) {
        if(length(lines[[i]]@Lines[[1]]@coords[1,]) != 2 )
        {print(i)}
    }
    
    
    # write trail_segments.geojson
    fileConn<-file("output files/trailsy standard/trailheads.json")
    writeLines(toJSON(trailheads, digits = 9), fileConn)
    close(fileConn)
    
    *# Create stewards.csv
    stewards <- data.frame(name = "managers",
                           id = 3,
                           url = "",
                           phone = "",
                           address = "",
                           publisher = "yes",
                           license = "none"
    )
    # write stewards.csv
    write.csv(stewards, file = "output files/trailsy standard/stewards.csv", row.names = FALSE)
    
    zip_dir <- c("output files/trailsy standard")
    zip("output files/trailsy_files.zip", files = paste(zip_dir, list.files(zip_dir), sep = "/"))

