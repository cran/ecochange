rsp2ebv <- structure(function #Remote Sensing Product to EBV
### This function processes regions of interest (polygon geometry or
### \code{GADM} unit) to integrate dissimilar remote sensing products
### (RSP) into Essential biodiversity variables.
                           ##details<< This function implements
                           ##\code{'gdalUtils'} so it assumes the user
                           ##has a working GDAL on their system. From
                           ##the documentation: "If the
                           ##\code{'gdalUtils_gdalPath'} option has
                           ##been set (usually by
                           ##\code{'gdal_setInstallation'}), the GDAL
                           ##found in that path will be used. If
                           ##nothing is found,
                           ##\code{'gdal_setInstallation'} will be
                           ##executed to attempt to find a working
                           ##GDAL that has the right drivers as
                           ##specified with the \code{'of'} (output
                           ##format) parameter", see example below.

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
    roi = NULL, ##<<\code{SpatialPolygonsDataFrame}; or
                ##\code{character}; or \code{NULL}. Region of
                ##interest. This can be whether 1) a polygon geometry;
                ##or 2) the name of a \code{GADM} unit (see
                ##\code{\link{getGADM}}); or 3) a \code{NULL}
                ##value. Default \code{NULL} makes the function to
                ##print a list of \code{GADM} units.
    ..., ##<<If \code{roi} is a \code{GADM} unit then additional
         ##arguments in \code{\link{getGADM}} can be specified here.
    lyrs = NULL, ##<<\code{character}. Remote sensing products, If
                 ##\code{NULL} then a list of products is printed, see
                 ##\code{\link{listGP}}.  Default \code{NULL}.
    path, ##<<\code{character}. Path name indicating where the
          ## variables are stored. If it is missing then a folder
          ## named as \code{'ecochange'} located in a current
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
    # unlink(file.path(path,'ebv'), recursive = TRUE)
    # unlink(file.path(path,'ecochange'), recursive = TRUE)
    
    if(missing(path)){
        ecodir <- file.path(tempdir(),'ecochange')
        if(!file.exists(ecodir))
        dir.create(ecodir)
        path  <- ecodir}

    if(inherits(roi, getOption('inh')[c(1,3:4)])){
        roi. <- roi
        roi <- getrsp(roi, ...,lyrs = lyrs, path = path, mc.cores = mc.cores)
        if(is.null(roi.))
            return(roi)
        if(is.null(lyrs))
            return(roi)}

    roi <- attributes(roi)$env$roi
    
    if(is.null(lyrs))return(listGP()$'layer')
    if(any(grepl('.01.01', lyrs))){
        lyrs <- rnm.lyrs0(lyrs)}
    int.patt  <- '[[:digit:]|N|S|E|W].tif'
    zfe <- file.path(path, dir(path))
    dec <- zfe
    with_zip <- any(grepl(paste(lyrs, collapse = "|"), zfe)&grepl('.zip', zfe))
    if(with_zip){
    dec <- decompMap0(zfe, td = path, int.patt = int.patt)
    tor <- attr(dec,'inzip')}
    dec <- sort(dec[grepl(paste(lyrs, collapse = "|"), dec)])
    rst <- Map(function(x)
        tryCatch(raster(x),error = function(e){
            print(NULL)}), x = dec)
    ftr <- function(roi, z){
        lapply(z, function(x)
            spTransform(roi, crs(x)))}
    et <- ftr(roi,rst)

    trs. <- Map(function(x,y)
        raster::intersect(extent(x),extent(y)),rst, et)
    trs <- names(Filter(function(x)
        !is.null(x),trs.))
    rst <- raster::subset(rst,names(rst)%in%trs)
    et <- et[trs]
    
    cr <- rst
    
    if(missing(sr)){
        l2u <- long2UTM(extent(roi)[1L])
        sr <- sub('utm.z',l2u, getOption('utm1'))}
    fext <- function(roi, tmp, sr){
        pr <- lapply(tmp, function(x)
            projectExtent(x, sr))
        ux <- lapply(pr,'extent')}
    exts <- fext(roi,cr, sr)
    uxt <- Reduce(raster::union,exts)
    pre <- projectExtent(cr[[1L]], crs = sr)
    pre <- setExtent(pre, uxt, keepres=TRUE)
    nex <- as.vector(uxt)
    te. <- nex[c(1,3,2,4)]
    flnm <- 'ebv'
    # if(length(file.path(path, flnm)) != 0){
    #     unlink(file.path(path, flnm), recursive = TRUE)}
    # crf <- dir(file.path(path, flnm))
    # if(length(crf)==0){
    #     dir.create(file.path(path,flnm))}
    # nwp <- file.path(path, 'ebv')
    
    # if(length(file.path(tempdir(), flnm)) != 0){
        unlink(file.path(tempdir(), flnm), recursive = TRUE)
        # }
    # crf <- dir(file.path(tempdir(), flnm))
    # if(length(crf)==0){
        dir.create(file.path(tempdir(),flnm))
        # }
    nwp <- file.path(tempdir(), flnm)
    
    gdf <- names(cr)

    nwp. <- file.path(tempdir(), flnm, basename(gdf))

        nmdst <- '250' ## <- preserves zero values

    fshp <- function(x,y){
        fname <- file.path(tempdir(),paste0(basename(y),'.shp'))
        raster::shapefile(x, filename = fname,
                          overwrite = TRUE)
        return(fname)
    }
    
    sps <- Map(function(x,y)
        fshp(x, y), et, names(et))



    marg. <- c(list(FUN = function(x,y,z,w)
        gdalUtils::gdalwarp(srcfile = x,
                            dstfile = y, crs(z),
                            sr, te = te., tr = ofr,
                            crop_to_cutline = TRUE,
                            cutline = w,
                            output_Raster = TRUE,
                            overwrite=TRUE,
                            dstnodata = nmdst,
                            nomd = TRUE),
        x = gdf, y = nwp., z = cr, w = sps), marg)
    pr <- stack(do.call(getOption('fapp'), marg.))

    fmos <- function(sll){
        crm <- 'Formatting image'
        dst <- file.path(nwp,paste0(sll,'.tif'))
        gdf. <- nwp.[grepl(sll, nwp.)]
        if(length(gdf.) == 1){
            ## print(paste0(crm,' of ', sll))
            mr <- raster(gdf.)}
        else{
                ## print(paste0(crm, ' mosaic of ', sll,':'))
                mr <- gdalUtils::mosaic_rasters(
                                     gdalfile=gdf.,
                                     dst_dataset=dst,
                                     output_Raster = TRUE)}
        return(mr)}
    
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
    temple <- stack(temple)
return(temple)
### \code{RasterStack} of essential biodiversity variables (UTM crs), or
### character lists suggesting GADM units/Global Products that can be
### used to download \code{rsp} (see \code{NULL} defaults in arguments
### \code{'roi'} and \code{'lyrs'}).
} , ex=function() {
    ## First, we'll check to make sure there is a valid GDAL
    ## installation (from 'gdalUtils):

    ## \donttest{
    ## gdalUtils::gdal_setInstallation()
    ## valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
    ## }

    ## Warnings from GDAL/PROJ are suppressed.
    
    ## A Global Surface Water layer ('seasonality') covering the extent of a
    ## Colombian municipality Cartagena del Chairá is formated into an
    ## spatial \code{EBV}:
            load(system.file('cchaira_roi.RData',package = 'ecochange'))

    ## \donttest{
    ## suppressWarnings(
    ## rsp_cchaira <- getrsp(roi = cchaira_roi,
    ##   lyrs = 'seasonality', mc.cores = 2, path = tempdir())
    ##)
    
    ## file.exists(rsp_cchaira) ## TRUE

    ## suppressWarnings(
    ## season_cchaira <- rsp2ebv(roi = cchaira_roi,
    ##                               lyrs = 'seasonality', path = tempdir())
    ## )

    ## suppressWarnings(
    ## plotebv(season_cchaira)
    ## )
    ## }
})
