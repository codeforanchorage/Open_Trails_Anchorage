library(ggplot2)
library(plyr)
library(ggmap)
library(mapproj)


setwd("C:/Users/Hans T/Desktop/Open_Trails_Anchorage/trail_plots")

for(i in seq(trails)) {
    
    lines_rows <- which(dat$TRAIL_NAME == trails[i])

    latlon <- data.frame()
    for(j in seq(length(lines_rows))) {
    
    latlon<-   rbind(latlon, lines[lines_rows[j]][[1]]@Lines[[1]]@coords)
    }
    
    bounding_box <- c(min(latlon$V1) - 0.02, min(latlon$V2) - 0.02, 
                      max(latlon$V1) + 0.02, max(latlon$V2) + 0.02)
    
anc_map <- get_map(bounding_box, source = "google", maptype = "hybrid")

p  <- ggmap(anc_map, extent = "device") + geom_point(data = latlon, aes(x = V1, y = V2), color = "red") +
                      ggtitle( trails[i])

ggsave(filename = paste0(gsub(" ", "_", trails[i]), ".png"), plot = p)
}

#foo <- paste0("https://github.com/codeforanchorage/Open_Trails_Anchorage/blob/master/trail_plots/", 
#              gsub(" ", "_", trails),
#              ".png")

#write.csv(foo, "foo.csv")
