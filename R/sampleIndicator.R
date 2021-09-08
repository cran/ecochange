sampleIndicator <- structure(function #Sample Biodiversity indicator 
### This function divides into fixed-size grids each of the scenes of
### a stack of ecosystem-spatial data and samples a biodiversity
### indicator by every grid. To compute biodiversity indicators at the
### class and landscape levels, see \code{\link{gaugeIndicator}}

                      ##references<< {Hesselbarth, M. H., Sciaini,
                      ##M., With, K. A., Wiegand, K., & Nowosad,
                      ##J. (2019). landscapemetrics: an open source R
                      ##tool to calculate landscape
                      ##metrics. Ecography, 42(10), 1648-1657.}
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
        ps = NULL, ##<<\code{SpatialPolygonsDataFrame} or
                    ##\code{RasterStack}. Polygon geometry used to
                    ##produce ecosystem-change maps via the
                    ##implementation of \code{\link{echanges}} or the
                    ##stack of ecosystem-change maps.
    ..., ##<<If \code{ps} is a \code{polygon} then additional
         ##arguments in \code{\link{echanges}}  or
         ##\code{\link{rsp2ebv}}.
    metric = 'condent', ##<<\code{character}. The name of an indicator
                        ##other than ecosystem extent. This can be
                        ##cohesion (\code{'cohesion'}), conditional
                        ##entropy (\code{'condent'}), perimeter-area
                        ##fractal dimension (\code{'pafrac'}), among
                        ##others, see package
                        ##\code{\link{list_lsm}}. Default
                        ##\code{'condent'}.
    classes = 5, ##<<\code{numeric}; or \code{NULL}. Number of evenly
                 ##spaced classes used to reclassify the
                 ##layers. Default \code{5}. If \code{NULL} then the
                 ##layers are not reclassified.
    min = 1, ##<<\code{numeric}. If \code{classes != NULL} then
             ##minimum cell value in the layers. Default \code{1}
    max = 100, ##<<\code{numeric}. If \code{classes != NULL} then
               ##maximum cell value in the layers. Default \code{100}
    side, ##<<\code{numeric}. Side of the sampling grid (\code{m}). If
          ##missing the function tries to find a grid size the samples
          ##at least a grid with a non-NA value of the indicator.
    smp_lsm = list(level = 'landscape'), ##<<\code{List}. Additional
                                         ##arguments in
                                         ##\code{\link{sample_lsm}}
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses 60 percent of the
                                          ##cores.
) {
    isLayer <- 'lyrs'%in%names(list(...))
    if(isLayer)
        isLayer <- is.null(list(...)$'lyrs')

    if(inherits(ps, getOption('inh'))|is.logical(ps)){
        ps. <- ps
        ps <- echanges(ps,mc.cores = mc.cores, ...)
        if(is.null(ps.)|is.logical(ps.))
            return(ps)
        if(isLayer)
            return(ps)
    }
    
    
    nm. <- names(ps)
    if(!is.null(classes)){
    recl.m <- recMatrix(min:max, classes)
    ps <- reclassify(ps, recl.m)}
    fnrs <- function(x){
        pjr <- projectRaster(ps, crs = crs(ps),
                             res = x, method = 'ngb')
        return(pjr)}
    if(missing(side)){
        sdc <- c(10^-c(1:3),5*(10^-c(2:3)))
        dff <- diff(extent(ps)[1:2])
        side <- dff*sdc[order(sdc, decreasing = TRUE)]
        recr.fnrs <- function(x){
            if(all(is.finite(fnrs(x)@'data'@'max')))
                return(min(res(fnrs(x))))
            else return(recr.fnrs(x + 1))}
        side <- recr.fnrs(side)
        side <- rep(side,2)}
    pr <- fnrs(side)
    spr <- sapply(raster::as.list(pr), function(x)x@'data'@'max')
        if(all(is.infinite(spr)))
            stop(unique(paste0("Failed to set side = ", side,
                        ", change 'side' argument")))
    r2pol <- rasterToPolygons(pr)
    r2pol <- lapply(1:nlayers(r2pol), function(x)r2pol[x])
    ps <- raster::as.list(ps)
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}

    fn_smp_lsm <- function(w,z, metric, smp_lsm){
        lst2 <- c(list(landscape = w, y = z, metric = metric), smp_lsm)
        return(lst2)}
    args <- Map(function(w,z)
        fn_smp_lsm(w, z, metric, smp_lsm), ps, r2pol)
    marg. <- c(list(FUN = function(x)
        do.call('sample_lsm', x),
        x = args), marg)
    myMetric <- do.call(getOption('fapp'), marg.)
    if(any(is.character(myMetric))){
        stop(myMetric)}

    rasterizeMetric <- function(x,w,z, val = 'value'){
        spl1 <- as.data.frame(x)
        rstt <- rasterize(w, z, field = spl1[,val])
        return(rstt)}
    rspr <- Map(function(x,y)
        rasterizeMetric(x,y,z=pr, val = 'value'), myMetric,r2pol)
    rspr <- stack(rspr)
    rspr <- round(rspr,2)
    names(rspr) <- nm.
    return(rspr)
### \code{Raster*}.
} , ex=function() {

    ## Warnings from GDAL/PROJ are suppressed.

    ## RasterBrick of structural Essential Biodiversity Variables
    ## covering the extent of a location in the northern Amazon basin
    ## (Colombia) is imported:
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- suppressWarnings(brick(path.))
    
    ## Changes in layers of tree-canopy cover (TC) in the 'amazon'
    ## brick are computed:
    suppressWarnings(
    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)
    )


    plotebv(amazon)

    ## Function 'sampleIndicator' is implemented to sample a metric of
    ## conditional entropy (default):

    suppressWarnings(
        def_condent <- sampleIndicator(def, side = 400, mc.cores = 2)
    )

    plotebv(def_condent)

})
