togeojson <- function(file, writepath = "~") {
    require(httr)
    url <- "http://ogre.adc4gis.com/convert"
    tt <- POST(url, body = list(upload = upload_file(file)))
    out <- content(tt, as = "text")
    fileConn <- file(writepath)
    writeLines(out, fileConn)
    close(fileConn)
}