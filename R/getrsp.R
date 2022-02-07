getrsp <- structure(function #Get remote sensing product
### This function processes the extent of a predefined region of
### interest (polygon geometry or \code{GADM} unit) to download
### ecosystem remote sensing products (ERSP). Downloadable ERSP include
### Global Surface Water, Forest Change, and Continuous Tree Cover
### data. See \code{\link{listGP}}.
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
                ##interest. This can be either 1) a polygon geometry;
                ##or 2) the name of a \code{GADM} unit (see
                ##\code{\link{getGADM}}); or 3) a \code{NULL}
                ##value. Default \code{NULL} makes the function to
                ##print a list of \code{GADM} units.
    ..., ##<<If \code{roi} is a \code{GADM} unit then additional
         ##arguments in \code{\link{getGADM}}.
    lyrs = NULL, ##<<\code{character}. Remote-sensing
                ##products. Default \code{NULL} makes the function to
                ##print a list of Downloadable data, see
                ##\code{\link{listGP}}.
    path, ##<<\code{character}. Path name indicating where the
          ## variables are stored. If missing then a folder
          ## named as \code{'ecochange'} created in a current
          ## temporary directory is used.
    rewrite.pass = FALSE, ##<<\code{logical}. Rewrite password. Only
                          ##valid to download new NASA Earth data, see
                          ##details section.
    verify.web = FALSE, ##<<\code{logical}. Verify in the web whether
                        ## the \code{URL}s used to download the
                        ## \code{rsp} are available. See
                        ## \code{getOption('webs')}. Default \code{FALSE}.
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses around 60 percent of the
                                          ##cores.
) {
        if(rewrite.pass)
        options('pw' = NULL)
    old <- options()
    on.exit(options(old), add = TRUE)
    class. <- 'getrsp'
    if(missing(path)){
        ecodir <- normalizePath(file.path(tempdir(),'ecochange'),winslash = '/',
                                mustWork = FALSE)
        if(!file.exists(ecodir))
        dir.create(ecodir)
        path  <- ecodir
    }else{
        path <- normalizePath(path,winslash = '/',
                              mustWork = FALSE)
    }
    if(inherits(roi, getOption('inh')[3:4])){
        roi. <- roi
        roi <- getGADM(roi,..., path = path)
        if(is.null(roi.))
            return(roi)}
    if(!compareCRS(crs(roi), getOption('longlat'))){
        roi <- st_as_sf(roi)
        roi <- st_transform(roi, crs = getOption('longlat'))
        roi <- as(roi, "Spatial")
    }
    if(is.null(lyrs))return(listGP()$'layer')
   if(any(grepl('TC_', lyrs)))
        lyrs <- rnm.lyrs0(lyrs)
    urt. <- suppressMessages(
        unlist(get_EOURL(roi, lyrs, path = path, verify.web = verify.web),
               use.names = FALSE))# <-
    lsRoi <- list2env(list(roi = roi))
    if(is.null(urt.)){
        ps <- paste(lyrs, collapse = '|')
        indir <- grep(ps,dir(path))
        if(length(indir) == 0){
            stop("'lyrs' can not be retrieved")}
        else{
                flcls <- file.path(path, dir(path)[indir])
                class(flcls) <- append(class(flcls),class.)
                ## print("Data already downloaded:")
                ## print(flcls)
                attributes(flcls) <- c(attributes(flcls), env = lsRoi)
                return(flcls)}
    }
    urt1 <- urt.[!basename(urt.)%in%dir(path)]
    urt2 <- urt.[basename(urt.)%in%dir(path)]
    ## if(length(urt2) != 0){
    ##     print("Data already downloaded:")
    ##     print(file.path(path,basename(urt2)))}
    if(length(urt1) == 0){
        flcls <- normalizePath(file.path(path, basename(urt.)),winslash = '/',
                               mustWork = FALSE)
        attributes(flcls) <- c(attributes(flcls), env = lsRoi)
        class(flcls) <- append(class(flcls), class.)
   return(flcls)}
    if(length(urt1) != 0){
        urt. <- urt1}
fl <- normalizePath(file.path(path, basename(urt.)),winslash = '/',
                    mustWork = FALSE)
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
docs <- normalizePath(file.path(path,basename(c(docs, urt2))),winslash = '/',
                      mustWork = FALSE)
    names(docs) <- NULL
    class(docs) <- append(class(docs),class.)
    attributes(docs) <- c(attributes(docs), env = lsRoi)
    return(docs)

   ##  if(rewrite.pass)
   ##      options('pw' = NULL)
   ##  old <- options()
   ##  on.exit(options(old), add = TRUE)
   ##  class. <- 'getrsp'
   ##  if(missing(path)){
   ##      # ecodir <- file.path(tempdir(),'ecochange')
   ##      ecodir <- normalizePath(file.path(tempdir(),'ecochange'),mustWork = FALSE)
   ##      if(!file.exists(ecodir))
   ##      dir.create(ecodir)
   ##      path  <- ecodir
   ##  }else{normalizePath(path)}
   ##  if(inherits(roi, getOption('inh')[3:4])){
   ##      roi. <- roi
   ##      roi <- getGADM(roi,..., path = path)
   ##      if(is.null(roi.))
   ##          return(roi)}
   ##  if(!compareCRS(crs(roi), getOption('longlat')))
   ##      ## roi <- spTransform(roi, getOption('longlat'))
   ##      roi <- spTransform(roi, CRSobj = getOption('longlat'))
   ##  if(is.null(lyrs))return(listGP()$'layer')
   ## if(any(grepl('TC_', lyrs)))
   ##      lyrs <- rnm.lyrs0(lyrs)
   ##  urt. <- suppressMessages(
   ##      unlist(get_EOURL(roi, lyrs, path = path, verify.web = verify.web),
   ##             use.names = FALSE))# <-
   ##  lsRoi <- list2env(list(roi = roi))
   ##  if(is.null(urt.)){
   ##      ps <- paste(lyrs, collapse = '|')
   ##      indir <- grep(ps,dir(path))
   ##      if(length(indir) == 0){
   ##          stop("'lyrs' can not be retrieved")}
   ##      else{
   ##              flcls <- file.path(path, dir(path)[indir])
   ##              class(flcls) <- append(class(flcls),class.)
   ##              ## print("'lyrs' already stored in path:")
   ##              ## print(flcls)
   ##              attributes(flcls) <- c(attributes(flcls), env = lsRoi)
   ##              return(flcls)}
   ##  }
   ##  urt1 <- urt.[!basename(urt.)%in%dir(path)]
   ##  urt2 <- urt.[basename(urt.)%in%dir(path)]
   ##  ## if(length(urt2) != 0){
   ##  ##     print("'lyrs' already retrieved:")
   ##  ##     print(file.path(path,basename(urt2)))}
   ##  if(length(urt1) == 0){
   ##      flcls <- normalizePath(file.path(path, basename(urt.)),mustWork = FALSE)
   ##      attributes(flcls) <- c(attributes(flcls), env = lsRoi)
   ##      class(flcls) <- append(class(flcls), class.)
   ## return(flcls)}
   ##  if(length(urt1) != 0){
   ##      urt. <- urt1}
   ##  fl <- normalizePath(file.path(path, basename(urt.)),mustWork = FALSE)
   ##  usgs <- grepl('usgs.gov', urt.)
   ##  if(any(usgs))
   ##      if(is.null(getOption('pw')))
   ##          options(pw = flg())

   ##  urt.. <- urt.[usgs]
   ##  urth.. <- urt.[!usgs]
   ##  fl.. <- fl[usgs]
   ##  flh.. <- fl[!usgs]
   ##  fprll <- getOption('fapp')
   ##  print(paste0('The new data will be stored in ', path,':'))
   ##  doc1 <- NULL
   ##  if(!getOption('isWin')){
   ##      marg[['mc.cores']] <- mc.cores}
   ##  if(length(urt..)!=0){
   ##      marg. <- c(list(FUN = function(x,y)
   ##          tryCatch(fgetpss(x,y, path = path),
   ##                   error = function(e){
   ##                       print(e)}),
   ##          x = urt..,
   ##          y = fl..),marg)
   ##      doc1 <- do.call(fprll, marg.)}
   ##  doc2 <- NULL
   ##  if(length(urth..)!=0){
   ##      marg. <- c(list(FUN = function(x,y)
   ##          tryCatch(fget(x,y, path = path),
   ##                   error = function(e){
   ##                       print(e)}),
   ##          x = urth..,
   ##          y = flh..),marg)
   ##      doc2 <- do.call(fprll, marg.)}
   ##  pth <- file.path(path, dir(path))
   ##  docs <- unlist(c(doc1, doc2))
   ##  docs <- normalizePath(file.path(path,basename(c(docs, urt2))),mustWork = FALSE)
   ##  names(docs) <- NULL
   ##  class(docs) <- append(class(docs),class.)
   ##  attributes(docs) <- c(attributes(docs), env = lsRoi)
   ##  return(docs)
### Path names of the remote sensing products just retrieved, or
### character vectors suggesting GADM units/Global Products that can be
### used to download ERSP (see \code{NULL} defaults in arguments
### \code{'roi'} and \code{'lyrs'}).
} , ex=function() {
    ## Polygon of the Colombian municipality of Cartagena del Chairá:
        load(system.file('cchaira_roi.RData',package = 'ecochange'))

    ## A Global Surface Water layer ('seasonality') which covers the
    ## extent of the polygon is retrieved:

    ## \donttest{
    ## rsp_cchaira <- getrsp(cchaira_roi,
    ##   lyrs = 'seasonality', mc.cores = 2, path = tempdir())
    ## file.exists(rsp_cchaira)
    ## }
})
