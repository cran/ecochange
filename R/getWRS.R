getWRS <- structure(function #Get WRS
### This function processes regions of interest (a polygon geometry or
### \code{GADM} unit) to find corresponding Landsat Path/Row World
### Reference System (\code{WRS}) polygons. This function is
### internally implemented by \code{\link{getrsp}}
(
    roi = NULL, ##<<\code{SpatialPolygonsDataFrame}; or
                ##\code{character}; or \code{NULL}. Region of
                ##interest. This can be whether 1) a polygon geometry;
                ##or 2) the name of a \code{GADM} unit (see
                ##\code{\link{getGADM}}); or 3) a \code{NULL}
                ##value. Default \code{NULL} makes the function to
                ##print a list of \code{GADM} units.
    path = tempdir(), ##<<\code{character}. Path name indicating where
                     ##the \code{WRS} data are processed.
    ... ##<<Additional arguments in \code{\link{getGADM}}.
) {
    if(inherits(roi, getOption('inh')[3:4])){
        roi. <- roi
        roi <- getGADM(roi,..., path = path)
        if(is.null(roi.))
            return(roi)}
    bs <- basename(getOption('wrs'))
    fx <- any(grepl(bs, dir(path)))
    if(!fx){
        fg <- fget(getOption('wrs'), file.path(path,bs), path = path)}
    lstar <- unzip(file.path(path, bs), exdir = path)
    patt. <- gsub('_0.zip','',bs)
glshp <- st_read(dsn = path, layer = patt., quiet = TRUE)
roi. <- st_as_sf(roi)
## st_agr(glshp)  <- 'constant'
## st_agr(roi.)  <- 'constant'
glshp <- st_set_agr(glshp, 'constant')
roi. <- st_set_agr(roi., 'constant')
    slc <- st_intersection(glshp[,'PR'], roi.)[,1L]

    ## glshp <- sf::as_Spatial(st_read(dsn = path, layer = patt., quiet = TRUE))
    ## tif <- lstar[grepl(patt., lstar, ignore.case = TRUE)]
    ## slc <- crop(glshp, roi)
    file.remove(file.path(lstar))
    return(slc)
    ###\code{SpatialPolygonsDataFrame}, or set of \code{GADM} units.
} , ex=function() {
    load(system.file('cchaira_roi.RData',package = 'ecochange'))
    ## \donttest{
    ## wrs_cchaira <- getWRS(cchaira_roi)
    ##     plot(wrs_cchaira)
    ## }
})
