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
    
    #setwd(paste0(oldWD, "/data/Mat_Su_Shapefile"))
    #matsu_trails <- readOGR(dsn=".", layer="MSB_Trails_Legal_Aug2014")
    #matsu_trails <- spTransform(matsu_trails, CRS("+init=epsg:4326")) 
    #colnames(matsu_trails@data) <- c("OBJECTID", "Shape_Leng", "TRAIL_NAME", "NAME_2", "TYPE", "USE", 
    #                                 "SEASON", "PLAN", "PLAN_ID", "LGL_ESMT", "ESMT_DOC", "SOURCE", 
    #                                 "SURVEY_DAT", "SURVEY_MET", "QC", "QC_2", "LENGTH_MI", "LENGTH_FT", 
    #                                 "NOTES")
    
    setwd(oldWD)
    
    #trail variables are in dat as a dataframe. trail segments are in lines as a list. Both need to be manipulated for cleaning 
    dat <- rbind.fill(data.frame(TRAIL_NAME = chugach_trails@data$TRAIL_NAME), anc_trails@data)# , matsu_trails@data)
    dat <- cbind(dat, new_id = seq(dim(dat)[1]))
    
    lines <- c(chugach_trails@lines, anc_trails@lines#, matsu_trails@lines)
    #########ADD FIELDS################
    levels(dat$LIGHTING) <- c("n", "y")
    dat[(is.na(dat$LIGHTING)),]$LIGHTING <- "n"
    
    
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
    bad_trails <- c("Bragaw", "SLEDDING HILL", "Loop", "Old Sweard Hwy. Trail", "Mt. View Sch. Trail", "Wonder Park Sch. Trail",
                         "Galdys Wood Trail", "Dimond (North Side)", "Dimond (South Side)", "Seward Hwy Trail", "Rondy Cut Off Trail", 
                         "14 mile loop", "Raspberry Parking Lot Connector", "New Seward Hwy. Trail", "Shortcut", "Ski Trail", "RABBIT CREEK PARK TRAIL", 
                         "Alder Trail", "Cannonrd", "Middle Fork", "School", "Glenn Alps View Loop", "Glenn Alps Powerline B", "Glenn Alps Powerline A", 
                         "Flattop Peak Alternate", "Flattop Option", "Ballfield", "Margeurite Hills", "Tranagain Arm Interpretive", "Baldy Traverse", "Trail 7", 
                         "Track 3", "Soccer Field", "N Connect", "Logjam Trail", "S Connect", "Gladys Wood Trail", "School Track", "Go Again")
    
    bad_trails <- dat$TRAIL_NAME %in% bad_trails 
    dat <- dat[!bad_trails,]
    lines <- lines[!bad_trails]
    
    #dat <- dat[c(102),]
    #lines <- lines[c(102)]
    
   
    #Rename trails that are small or conitinous of other trails. 
    #This is mainly just to categorize trails more easily. 
    dat <- dat[!is.na(dat$TRAIL_NAME),]
    lines <- lines[!is.na(dat$TRAIL_NAME)]
    dat$TRAIL_NAME <- as.character(dat$TRAIL_NAME)
    dat[dat$TRAIL_NAME == "Spur Oscar Anderson",]$TRAIL_NAME <- "Coastal Trail"
    dat[dat$TRAIL_NAME == "North Coastal Trail",]$TRAIL_NAME <- "Coastal Trail"
    dat[dat$TRAIL_NAME == "Ship Creek Trail",]$TRAIL_NAME <- "Ship Creek"
    dat[dat$TRAIL_NAME == "Tikishla Park Trail",]$TRAIL_NAME <- "Chester Creek Trail"
    dat[dat$TRAIL_NAME == "Peanut Farm",]$TRAIL_NAME <- "Campbell Creek Trail"
    dat[dat$TRAIL_NAME == "to main",]$TRAIL_NAME <- "Spruce Loop"
    dat[dat$TRAIL_NAME == "Main",]$TRAIL_NAME <- "Connors Lake Trail"
    dat[dat$TRAIL_NAME == "Along Lake",]$TRAIL_NAME <- "Connors Lake Loop"
    dat[dat$TRAIL_NAME == "Lakeside Loop",]$TRAIL_NAME <- "Connors Lake Loop"
    dat[dat$TRAIL_NAME == "To Lake",]$TRAIL_NAME <- "Connors Lake Loop"
    dat[dat$TRAIL_NAME == "Mahaffey/APU",]$TRAIL_NAME <- "Mahaffey Loop"
    dat[dat$TRAIL_NAME == "Tour Of Anchorage",]$TRAIL_NAME <- "Chester Creek Trail"
    dat[dat$TRAIL_NAME == "Old Three Mile Trail",]$TRAIL_NAME <- "Outgoing Trail"
    dat[dat$TRAIL_NAME == "Culvert Trail",]$TRAIL_NAME <- "Outgoing Trail"
    dat[dat$TRAIL_NAME == "Inner Lake Loop",]$TRAIL_NAME <- "Little Campbell Lake Loop"
    dat[dat$TRAIL_NAME == "Lake Loop",]$TRAIL_NAME <- "Little Campbell Lake Loop"
    #dat[dat$TRAIL_NAME == "North Gasline/Powerline Trail",]$TRAIL_NAME <- "Powerline"
    dat[dat$TRAIL_NAME == "Multi-use To Bivouc",]$TRAIL_NAME <- "Gasline"
    dat[dat$TRAIL_NAME == "Spencer Short Cut",]$TRAIL_NAME <- "Spencer's Loop"
    dat[dat$TRAIL_NAME == "Upper Gasline Trail",]$TRAIL_NAME <- "Gasline Trail"
    dat[dat$TRAIL_NAME == "North Fork",]$TRAIL_NAME <- "Long Lake"
    dat[dat$TRAIL_NAME == "Alder 2",]$TRAIL_NAME <- "Alder"
    dat[dat$TRAIL_NAME == "Prator Spur",]$TRAIL_NAME <- "Rabbit Creek Greenbelt"
    dat[dat$TRAIL_NAME == "RC Meadow 1",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "RC Meadow 2",]$TRAIL_NAME <- "Rabbit Creek Meadow"    
    dat[dat$TRAIL_NAME == "RC Meadow 3",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "RC Meadow 4",]$TRAIL_NAME <- "Rabbit Creek Meadow"    
    dat[dat$TRAIL_NAME == "RC Meadow 5",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "E 142nd Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"    
    dat[dat$TRAIL_NAME == "N. Picket Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "East Boundary Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"  
    dat[dat$TRAIL_NAME == "Open Bog Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "Real Ridge",]$TRAIL_NAME <- "Rabbit Creek Meadow"    
    dat[dat$TRAIL_NAME == "Connect",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "Picket Spur Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"    
    dat[dat$TRAIL_NAME == "Mud Bog Trail",]$TRAIL_NAME <- "Rabbit Creek Meadow"
    dat[dat$TRAIL_NAME == "Snowmobile Entrance",]$TRAIL_NAME <- "Upper Huffman Entrance"    
    dat[dat$TRAIL_NAME == "Snowmobile Exit",]$TRAIL_NAME <- "Upper Huffman Entrance"
    dat[dat$TRAIL_NAME == "Powerline/gasline",]$TRAIL_NAME <- "Powerline"
    dat[dat$TRAIL_NAME == "Southpark Lp",]$TRAIL_NAME <- "Southpark Trail"    
    dat[dat$TRAIL_NAME == "Gasline To Rabbit Cr Rd",]$TRAIL_NAME <- "Southpark Trail"
    dat[dat$TRAIL_NAME == "Terrace Wood Connection",]$TRAIL_NAME <- "Southpark Trail"    
    dat[dat$TRAIL_NAME == "Little Rabbit Creek",]$TRAIL_NAME <- "Southpark Trail"
    dat[dat$TRAIL_NAME == "Blueberry Loop",]$TRAIL_NAME <- "Flattop Peak"    
    dat[dat$TRAIL_NAME == "E 172nd Trail",]$TRAIL_NAME <- "Belarde"
    dat[dat$TRAIL_NAME == "West Face",]$TRAIL_NAME <- "Elevator"    
    dat[dat$TRAIL_NAME == "W Face Baldy",]$TRAIL_NAME <- "Airstrip Trail"
    dat[dat$TRAIL_NAME == "ER Parks & Rec/HLB Trail",]$TRAIL_NAME <- "HLB Trail"    
    dat[dat$TRAIL_NAME == "East Rib To Mchugh Cr",]$TRAIL_NAME <- "Potter McHugh Ridge"
    dat[dat$TRAIL_NAME == "So Potter Ridge",]$TRAIL_NAME <- "Potter McHugh Ridge"    
    dat[dat$TRAIL_NAME == "Glenn Hwy. Trail",]$TRAIL_NAME <- "Glenn Hwy Trail"
    dat[dat$TRAIL_NAME == "McHugh",]$TRAIL_NAME <- "McHugh Lake"    
    dat[dat$TRAIL_NAME == "South Fork ER",]$TRAIL_NAME <- "Eagle Lake"
    dat[dat$TRAIL_NAME == "Lake Spur HV",]$TRAIL_NAME <- "Hanging Valley"    
    dat[dat$TRAIL_NAME == "Mchugh",]$TRAIL_NAME <- "McHugh Lake"
    dat[dat$TRAIL_NAME == "ER Access1",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "ER Access2",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "ER Access3",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "River Loop",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "River Loop Yurt",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "RL Interp",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "RL Interp 2",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "RL Interp 3",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "Connector 1",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "Connector 2",]$TRAIL_NAME <- "Dew Mound"
    dat[dat$TRAIL_NAME == "Yurt Classrom",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "Public Use Cabin",]$TRAIL_NAME <- "Crow Pass"
    dat[dat$TRAIL_NAME == "Rapids Camp Loop",]$TRAIL_NAME <- "Crow Pass"
    dat[dat$TRAIL_NAME == "Four Corners Loop",]$TRAIL_NAME <- "Albert Loop"
    dat[dat$TRAIL_NAME == "Bird Valley",]$TRAIL_NAME <- "Penguin Creek"
    dat[dat$TRAIL_NAME == "Eklutna Lake Spillway",]$TRAIL_NAME <- "Eklutna LAkeside"
    dat[dat$TRAIL_NAME == "Spur 1",]$TRAIL_NAME <- "Cali Creek"
    dat[dat$TRAIL_NAME == "Spur 2",]$TRAIL_NAME <- "Cali Creek"
    dat[dat$TRAIL_NAME == "Southwest",]$TRAIL_NAME <- "Connors Lake Trail"
    dat[dat$TRAIL_NAME == "Whaley Sch. Trail",]$TRAIL_NAME <- "Chester Creek Trail"
    #dat[dat$TRAIL_NAME == "",]$TRAIL_NAME <- ""
    
    ###################################
    
    dat$MANAGEMENT <- as.character(dat$MANAGEMENT)
    dat$MANAGEMENT[is.na(dat$MANAGEMENT)] <- "NA"
    dat$MANAGEMENT <- factor(dat$MANAGEMENT)
    managers <- levels(dat$MANAGEMENT)
    stewardID <- seq(managers)
    
    
    trails <- levels(factor(dat$TRAIL_NAME))
    
    
    
    trail_segments <- list(features = x <- vector(mode = "list", length = length(trails)),
                           type = "FeatureCollection")
    
    for(i in seq(dim(dat)[1])) {

        trail_id <- which(dat$TRAIL_NAME[i] == trails)
     
        trail_segments$features[[i]] <- list(geometry = list(coordinates = list(lines[[i]]@Lines[[1]]@coords),
                                                                       type = "MultiLineString"),
                                             id = dat$new_id[i],
                                             properties = list(id = dat$new_id[i],
                                                               source_id = 3,
                                                               steward_id = 3,
                                                               length = 1.4, 
                                                               trail1 = dat$TRAIL_NAME[i],
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
                                                               steward_fullname = dat$MANAGEMENT[i],
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
    
    
    named_trails <- data.frame(id = ninja$new_id,
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
        
        #turtles <- lapply(lines[lines_rows], function(x) { x@Lines[[1]]@coords})    
        
        trailheads$features[[i]] <- list(type = "Feature",
                                         geometry = list(type = "Point",
                                                         coordinates = lines[lines_rows][[1]]@Lines[[1]]@coords[1,]),
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
                                                           equestrian = as.character(dat[lines_rows,]$LIGHTING[1]), 
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

