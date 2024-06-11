getGADM <- structure(function #Get Geographic Adminitrative Unit
### This function can retrieve Geographic Administrative Data Maps
### (\code{GADM}).
                     ##references<<\href{https://gadm.org/}{https://gadm.org/}
(
    unit.nm = NULL, ##<<\code{character} or \code{NULL}. Name of
                    ##Geographic Administrative Data Map (e.g.,
                    ##municipality), or the name of such an unit plus
                    ##its corresponding higher-level unit (e.g.,
                    ##department/state). If \code{NULL} then a list of
                    ##administrative subdivisions is printed.
    level = 2, ##<<\code{numeric}. A number between zero and two,
               ##indicating any of the levels of administrative
               ##subdivisions (\code{0=country},
               ##\code{1=first administrative subdivision}, and
               ##\code{2=second administrative subdivision}).
    country = 'COL', ##<<\code{character}. \code{ISO} code specifying
                     ##a country. Default \code{'COL'}.
    ext = 'json',    ##<<\code{character}. File
                     ## extension of the retrieved data file. Default
                     ## \code{'json'}.
    path = tempdir() ##<<\code{character}. Path name indicating where
                     ##the unit will be stored. Default stores the
                     ##data in a temporary directory.
) {
    if(!is.null(unit.nm))
        if(is.na(unit.nm[1L]))
            return(NA)
    ## adm <- gadm(
    adm <- gadm.(
        path = path,
        country=country,
        level=level,
        ext = ext)
    if(level == 0){
        adm <- as(adm, 'Spatial')
        return(adm)
    }
    ## lvcol <<- paste('NAME',level, sep ='_')
    ## ds <- data.frame(adm)[,lvcol]
    ds <- data.frame(adm)[,paste('NAME',level, sep ='_')]
    if(level == 2){
        names(ds) <- data.frame(adm)[,'NAME_1']
    }
if(is.null(unit.nm))
    return(ds)
chm <- pmatch(unit.nm[1], ds)
unit.nm[1L] <- ds[chm]
if(level == 1)
    adm <- subset(adm, adm$'NAME_1' == unit.nm[1L], 'NAME_1')
    if(level == 2)
        adm <- subset(adm, adm$'NAME_2' == unit.nm[1L], c('NAME_1','NAME_2'))
     ## adm <- subset(adm, adm$'NAME_2' == unit.nm[1L], c('NAME_2'))
    if(length(adm) == 0){
            cat("WARNING: subdivision in 'unit.nm' was not found\n")
            cat("HINT: run getGADM(NULL, country = <ISO>, ...) and find a subdivision\n")
            stop("missing subdivision")
    }
adm <- as(adm, 'Spatial')
if(level == 2 & length(adm) > 1)
        if(length(unit.nm) == 1){
            cat("WARNING: argument 'unit.nm' matches several subdivisions\n")
            cat("HINT: run getGADM(NULL, country = <ISO>, ...)\n")
            cat("HINT: names of the printed vector are higher-level subdivisions\n")
            cat("HINT: provide a subdivision and a higher-level subdivision: e.g., unit.nm = c(<municipality>, <Department>)\n")
            stop("ambiguous subdivision")
        }
    if(length(adm) > 1){
ds <- data.frame(adm)[,'NAME_1']
chm <- pmatch(unit.nm[2L], ds)
if(is.na(chm)){
    cat("WARNING: subdivision in 'unit.nm' was not found\n")
    cat("HINT: run getGADM(NULL, country = <ISO>, ...) and find a subdivision\n")
    stop("missing subdivision")}
        unit.nm[2] <- ds[chm]
adm <- subset(adm, get('NAME_1')%in%unit.nm[2])}
    return(adm)
### \code{SpatialPolygonsDataFrame} or \code{character} vector of
### \code{GADM} units..
} , ex=function() {
## Printing municipalities of Colombia:

    ## \donttest{
    ##     muni <- getGADM(NA)
    ##     head(muni)
    ## }

})
