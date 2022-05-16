rsp2ebv <- structure(function#Integrate remote sensing products
###This function integrates ecosystem remote sensing products and
###produces raster-data sections with the cell values enclosed in a
###region of interest.
                           ##details<< This function implements
                           ##\code{'sf::gdal_utils'} so it assumes the
                           ##user's machine has a valid GDAL
                           ##installation.

                      ##references<< {Jetz, W., McGeoch, M. A.,
                      ##Guralnick, R., Ferrier, S., Beck, J.,
                      ##Costello, M. J., ... & Meyer,
                      ##C. (2019). Essential biodiversity variables
                      ##for mapping and monitoring species
                      ##populations. Nature Ecology & Evolution, 3(4),
                      ##539-551.}
                      ##
                      ##{O'Connor, B., Secades, C., Penner, J.,
                      ##Sonnenschein, R., Skidmore, A., Burgess,
                      ##N. D., & Hutton, J. M. (2015). Earth
                      ##observation as a tool for tracking progress
                      ##towards the Aichi Biodiversity Targets. Remote
                      ##sensing in ecology and conservation, 1(1),
                      ##19-28.}
                      ##
                      ##{Skidmore, A. K., & Pettorelli,
                      ##N. (2015). Agree on biodiversity metrics to
                      ##track from space: Ecologists and space
                      ##agencies must forge a global monitoring
                      ##strategy. Nature, 523(7561), 403-406.}



(
    ps = NULL, ##<<\code{SpatialPolygonsDataFrame}; or
                ##\code{character}; or \code{NULL}. Region of
                ##interest. This can be whether 1) a polygon geometry;
                ##or 2) the name of a \code{GADM} unit (see
                ##\code{\link{getGADM}}); or 3) a \code{NULL}
                ##value. Default \code{NULL} makes the function to
                ##print a list of \code{GADM} units.
    ..., ##<<Additional arguments in \code{\link{getGADM}} and
         ##\code{\link{getrsp}}.
    lyrs = NULL, ##<<\code{character}. Remote-sensing
                ##products. Default \code{NULL} makes the function to
                ##print a list of Downloadable data, see
                ##\code{\link{listGP}}.
    path, ##<<\code{character}. Path name indicating where the
          ## variables are stored. If missing then a folder
          ## named as \code{'ecochange'} created in a current
          ## temporary directory is used.
    sr, ##<< \code{character}. \code{PROJ.4} description of the target
        ##coordinate reference system. If missing then the target
        ##layers are projected to metric system \code{UTM}.
    ofr = c(30,30), ##<< \code{numeric}. \code{c(xres,yres)}.  Output
                   ##file resolution (in target georeferenced
    ##units). Default \code{c(30,30)} m2.
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses around 60 percent of
                                          ##the cores.

) {
    if(missing(path)){
        ecodir <- normalizePath(file.path(tempdir(),'ecochange'),
                                winslash = '/',mustWork = FALSE)
        if(!file.exists(ecodir))
            dir.create(ecodir)
        path  <- ecodir}
    if(!missing(path))
        path <- normalizePath(path.expand(path), winslash = '/',mustWork = FALSE)
    if(inherits(ps, getOption('inh')[c(1,3:4)])&
       !inherits(ps, 'getrsp')){
        ps. <- ps
        ps <- getrsp(ps, ...,lyrs = lyrs, path = path, mc.cores = mc.cores)
        if(is.null(ps.))
            return(ps)
        if(is.null(lyrs))
            return(ps)}
    ps <- attributes(ps)$env$roi
    if(is.null(lyrs))return(listGP()$'layer')
    if(any(grepl('.01.01', lyrs))){
        lyrs <- rnm.lyrs0(lyrs)}
    int.patt  <- '[[:digit:]|N|S|E|W].tif'
    zfe <- normalizePath(file.path(path, dir(path)), winslash = '/', mustWork = FALSE)
    dec <- zfe
    with_zip <- any(grepl(paste(lyrs, collapse = "|"), zfe)&grepl('.zip', zfe))
    if(with_zip){
        dec <- decompMap0(zfe, td = path, int.patt = int.patt)
        tor <- attr(dec,'inzip')}
    dec <- sort(dec[grepl(paste(lyrs, collapse = "|"), dec)])
    rst <- Map(function(x)
        tryCatch(raster(x),error = function(e){
            print(NULL)}), x = dec)
    ## ftr <- function(ps, z){
    ##     lapply(z, function(x)
    ##         spTransform(ps, CRSobj = crs(x)))}
    ftr <- function(ps, z){
        lapply(z, function(x){
        ## clsss <- class(ps)
        ps <- st_as_sf(ps)
        ps <- st_transform(ps, crs = crs(x))
        ps <- as(ps, "Spatial")})}
    et <- ftr(ps,rst)

    fext2sf <- function(x){
        maing <- 'SpatialPolygons'
        ep. <- as(extent(x),maing)
        crs(ep.) <- crs(x)
        ep. <- st_as_sf(ep.)}
    et. <- lapply(et, fext2sf)
    rst. <- lapply(rst, fext2sf)
    pred <- Map(function(x,y)
        st_intersection(x,y), rst., et.)
    mpred <- Map(function(x)
        st_bbox(x), pred)
    nmord <- names(mpred[[1L]])[c(c(1,3),c(2,4))]
    exts <- Map(function(x)
        extent(x[nmord]), mpred)
    NAext_to_NULL <- function(x)
    {
        x. <- as.vector(x)
        if(all(is.na(x.)))
            x <- NULL
        return(x)
    }
    trs. <- lapply(exts, function(x)NAext_to_NULL(x))

    ## trs. <- Map(function(x,y)
    ##     raster::intersect(extent(x),extent(y)),rst, et)

    trs <- names(Filter(function(x)
        !is.null(x),trs.))
    rst <- raster::subset(rst,names(rst)%in%trs)
    et <- et[trs]
    cr <- rst
    if(missing(sr)){
    l2u <- long2UTM(extent(ps)[1L])
    sr <- sub('utm.z',l2u, getOption('utm1'))
    }
    ## return(list(ps = ps, cr = cr, sr = sr))

    ## fext <- function(ps, tmp, sr){
    ##     pr <- lapply(tmp, function(x)
    ##         projectExtent(x, sr))
    ##     ux <- lapply(pr,'extent')}
    ## exts <- fext(ps,cr, sr)

    fext <- function(x, sr){
        ex_ <- extent(x)
        exd <- st_bbox(
            st_transform(st_as_sfc(st_bbox(x)), crs = sr))
        exord <- extent(exd[names(exd)[c(c(1,3),c(2,4))]])
        return(exord)}

    exts <- Map(function(x)
        fext(x, sr = sr), cr)

    
    uxt <- Reduce(raster::union,exts)
    ## return(list(cr = cr, sr = sr, uxt = uxt))
    ## pre <- projectExtent(cr[[1L]], crs = sr)
    ## pre <- setExtent(pre, uxt, keepres=TRUE)
    nex <- as.vector(uxt)
    te. <- nex[c(1,3,2,4)]
    flnm <- 'ebv'
    nwp <- normalizePath(file.path(tempdir(), flnm), winslash = '/', mustWork = FALSE)
    dir.create(nwp, showWarnings=FALSE)
    gdf <- normalizePath(names(cr), winslash = '/', mustWork = FALSE)
    nwp. <- normalizePath(file.path(nwp,basename(gdf)),winslash = '/', mustWork = FALSE)
    nmdst <- '250' ## <- preserves zero values
    fshp <- function(x,y){
        fname <- file.path(tempdir(),paste0(basename(y),'.shp'))
        raster::shapefile(x, filename = fname,
                          overwrite = TRUE)
        return(fname)}
    sps <- Map(function(x,y)
        fshp(x, y), et, names(et))
    sps <- lapply(sps, function(x)
        normalizePath(x, winslash = '/', mustWork = FALSE))
    ## return(list(gdf = gdf, nwp. = nwp.,nwp = nwp,sps = sps, cr = cr, sr = sr, ofr = ofr, nmdst = nmdst, te. = te.))

marg. <- c(list(FUN = function(x,y,z,w)
    sf::gdal_utils(util = 'warp',
                   source = x,
                   destination = y,
                   options = c("-s_srs", st_crs(proj4string(z))$wkt,
                               "-t_srs", st_crs(sr)$wkt,
                               "-te", te.,
                               "-tr", ofr,
                               "-overwrite",
                               "-crop_to_cutline",
                               "-cutline",w,
                               "-dstnodata", nmdst,
                               "-nomd")),
                   x = gdf, y = nwp., z = cr, w = sps), marg)
    pr <- unlist(do.call(getOption('fapp'), marg.), use.names = FALSE)

fmos <- function(x){
    dst <- file.path(nwp,paste0(x,'.tif'))
    gdf. <- nwp.[grepl(x, nwp.)]
        if(length(gdf.) == 1){
            mr <- raster(gdf.)
        }else{
            mr <- sf::gdal_utils(util = 'warp',
                                 source = gdf.,
                                 destination = dst,
                                 options = c("-overwrite"
                                             ## "-crop_to_cutline"))
                                             ))
            mr <- raster(dst)
        }
    return(mr)
}

    marg. <- c(list(FUN = function(x)
        fmos(x), x = lyrs), marg)
    temple <- do.call(getOption('fapp'), marg.)

        
    if(with_zip){
        file.remove(tor)}
    dr <- dir(path)
    dr1 <- dir(tempdir())

    rexp2rem <- '.vrt|.txt|.dbf|.prj|.shp|.shx'
    torem <- file.path(path,dr[grepl(rexp2rem,dr)])
    torem1 <- file.path(tempdir(),dr1[grepl(rexp2rem,dr1)])
    file.remove(c(torem,torem1))
   ## temple <- brick(temple)

      class(temple) <- append('echanges',class(temple))

    return(temple)

### Class \code{echanges}.
} , ex=function() {
    ## A Global Surface Water layer ('seasonality') covering the extent of a
    ## Colombian municipality Cartagena del Chaira is formated into an
    ## spatial EBV:
            load(system.file('cchaira_roi.RData',package = 'ecochange'))

    ## \donttest{
    ## rsp_cchaira <- getrsp(cchaira_roi,
    ##   lyrs = 'seasonality', mc.cores = 2, path = tempdir())

    ## file.exists(rsp_cchaira)

    ## season_cchaira <- rsp2ebv(cchaira_roi,
    ##                               lyrs = 'seasonality', path = tempdir())

    ## plot.echanges(season_cchaira)
    ## }
})
