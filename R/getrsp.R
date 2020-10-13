getrsp <- structure(function #Get remote sensing product
### This function processes the extent of a predefined region of
### interest (polygon geometry or \code{GADM} unit) to download remote
### sensing products (RSP). Downloadable RSP include Global Surface
### Water, Forest Change, and Continuous Tree Cover data. See
### \code{\link{listGP}}.
                        ##details<< Downloads of Continuous Tree Cover
                        ##data require user authentication through
                        ##the NASA Earth data Login. To obtain a NASA
                        ##Earth data Login account, please visit:
                        ##\href{https://urs.earthdata.nasa.gov}
                        ##{https://urs.earthdata.nasa.gov/users/new}.

                      ##references<< {Pekel, J. F., Cottam, A.,
                      ##Gorelick, N., & Belward,
                      ##A. S. (2016). High-resolution mapping of
                      ##global surface water and its long-term
                      ##changes. Nature, 540(7633), 418-422.}
                      ## 
                      ##{Hansen, M. C., Potapov, P. V., Moore, R.,
                      ##Hancher, M., Turubanova, S. A., Tyukavina, A.,
                      ##... & Kommareddy, A. (2013). High-resolution
                      ##global maps of 21st-century forest cover
                      ##change. science, 342(6160), 850-853.}
                      ## 
                      ## {Sexton, J. O., Song, X. P., Feng, M.,
                      ##Noojipady, P., Anand, A., Huang, C., ... &
                      ##Townshend, J. R. (2013). Global, 30-m
                      ##resolution continuous fields of tree cover:
                      ##Landsat-based rescaling of MODIS vegetation
                      ##continuous fields with lidar-based estimates
                      ##of error. International Journal of Digital
                      ##Earth, 6(5), 427-448.}

(
    roi = NULL, ##<<\code{SpatialPolygonsDataFrame}; or
                ##\code{character}; or \code{NULL}. Region of
                ##interest. This can be whether 1) a polygon geometry;
                ##or 2) the name of a \code{GADM} unit (see
                ##\code{\link{getGADM}}); or 3) a \code{NULL}
                ##value. Default \code{NULL} makes the function to
                ##print a list of \code{GADM} units.
    ..., ##<<If \code{roi} is a \code{GADM} unit then additional
         ##arguments in \code{\link{getGADM}}.
    lyrs = NULL, ##<<\code{character}. Remote sensing
                ##products. Default \code{NULL} makes the function to
                ##print a list of Downloadable RSPs, see
                ##\code{\link{listGP}}.
    path, ##<<\code{character}. Path name indicating where the
          ##variables will be stored. Default uses a folder named as
          ##\code{'ecochange'} located in a current temporary
          ##directory.
    verify.web = FALSE, ##<<\code{logical}. Verify in the web whether
                        ## the \code{URL}s used to download the
                        ## \code{rsp} are available. See
                        ## \code{getOption('webs')}. Default \code{FALSE}.
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses around 60 percent of the
                                          ##cores.
) {
    old <- options()
    on.exit(options(old), add = TRUE)
    class. <- 'getrsp'
    if(missing(path)){
        dir.create(file.path(tempdir(),'ecochange'))
        path  <- file.path(tempdir(), 'ecochange')}
    if(inherits(roi, getOption('inh')[3:4])){
        roi. <- roi
        roi <- getGADM(roi,..., path = path)
        if(is.null(roi.))
            return(roi)}
    if(!compareCRS(crs(roi), getOption('longlat')))
        roi <- spTransform(roi, getOption('longlat'))
    if(is.null(lyrs))return(listGP()$'layer')
   if(any(grepl('TC_', lyrs)))
        lyrs <- rnm.lyrs0(lyrs)
    urt. <- suppressMessages(
        unlist(get_EOURL(roi, lyrs, path = path, verify.web = verify.web),
               use.names = FALSE))# <-
    urt1 <- urt.[!basename(urt.)%in%dir(path)]
    urt2 <- urt.[basename(urt.)%in%dir(path)]
    if(length(urt2) != 0){
        print('Data already retrieved:')
        print(file.path(path,basename(urt2)))}
    if(length(urt1) == 0){
        flcls <- file.path(path,basename(urt.))
        class(flcls) <- append(class(flcls),class.)
        return(flcls)}
    if(length(urt1) != 0){
        urt. <- urt1}
    fl <- file.path(path, basename(urt.))
    usgs <- grepl('usgs.gov', urt.)
    if(any(usgs))
        if(is.null(getOption('pw')))
            options(pw = flg()) 

    urt.. <- urt.[usgs]
    urth.. <- urt.[!usgs]
    fl.. <- fl[usgs]
    flh.. <- fl[!usgs]
    fprll <- getOption('fapp')
    print(paste0('The new data will be stored in ', path,':'))
    doc1 <- NULL
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}
    if(length(urt..)!=0){
        marg. <- c(list(FUN = function(x,y)
            tryCatch(fgetpss(x,y, path = path),
                     error = function(e){
                         print(e)}),
            x = urt..,
            y = fl..),marg)
        doc1 <- do.call(fprll, marg.)}
    doc2 <- NULL
    if(length(urth..)!=0){
        marg. <- c(list(FUN = function(x,y)
            tryCatch(fget(x,y, path = path), 
                     error = function(e){
                         print(e)}),
            x = urth..,
            y = flh..),marg)
        doc2 <- do.call(fprll, marg.)}
    pth <- file.path(path, dir(path))
    docs <- unlist(c(doc1, doc2))
    docs <- file.path(path,basename(c(docs, urt2)))
    names(docs) <- NULL
    class(docs) <- append(class(docs),class.)
    return(docs)
### Path names of the remote sensing products just retrieved, or
### character lists suggesting GADM units/Global Products that can be
### used to download \code{rsp} (see \code{NULL} defaults in arguments
### \code{'roi'} and \code{'lyrs'}).
} , ex=function() {

    ## Warnings from GDAL/PROJ are suppressed.

    ## A Global Surface Water layer ('seasonality') covering the extent of a
    ## Colombian municipality Cartagena del Chairá is retrieved:
        load(system.file('cchaira_roi.RData',package = 'ecochange'))

    ## \donttest{
    ## suppressWarnings(
    ## rsp_cchaira <- getrsp(roi = cchaira_roi,
    ##   lyrs = 'seasonality', mc.cores = 2, path = tempdir())
    ##)
    ## file.exists(rsp_cchaira) ## TRUE
    ## }
})
