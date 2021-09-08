gaugeIndicator <- structure(function #Gauge Biodiversity Indicator
### This function processes ecosystem-change maps from
### \code{\link{echanges}} to calculate biodiversity indicators,
### including ecosystem extent, entropy, fractal dimension, among
### others. To sample the indicators across fixed-size grids see
### \code{\link{sampleIndicator}}.
                      ##details<< Coordinate reference system of the
                      ##spatial units must have metric units \code{
                      ##UTM}. Performance in the computation of
                      ##ecosystem extents is optimized via the
                      ##implementation of the function
                      ##\code{\link{tabuleRaster}}.  Indicators other
                      ##than ecosystem extents are calculated
                      ##implementing \code{\link{calculate_lsm}}.
                      ##
                      ##references<< {Hesselbarth, M. H., Sciaini, M.,
                      ##With, K. A., Wiegand, K., & Nowosad,
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
    ps, ##<<\code{SpatialPolygonsDataFrame} or
         ##\code{RasterStack}. Polygon geometry used to produce
         ##ecosystem-change maps via the implementation of
         ##\code{\link{echanges}} or the stack of ecosystem-change
         ##maps.
    ..., ##<< If \code{ps} is a \code{polygon} then additional
         ##arguments in \code{\link{echanges}} or
         ##\code{\link{rsp2ebv}}.
    metric = 'area_ha', ##<<\code{character}. The name of an
                       ##indicator. Default \code{'area_ha'} computes
                       ##ecosystem areas (ha) at class level. See the
                       ##argument \code{'metric'} in
                       ##\code{\link{list_lsm}} to implement other
                       ##metrics.
    smp_lsm = list(), ##<<\code{list}. List of arguments in
                      ##\code{\link{calculate_lsm}}. This argument is
                      ##ignored when \code{metric = 'area_ha'}.
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses around 60 percent of
                                          ##the cores.
) {

    isLayer <- 'lyrs'%in%names(list(...))
    if(isLayer)
        isLayer <- is.null(list(...)$'lyrs')
    
    if(inherits(ps, getOption('inh'))|is.logical(ps)){
        ps. <- ps
        ps <- echanges(ps,mc.cores = mc.cores, ...)
        if(is.null(ps.)|is.logical(ps.)|is.logical(ps))
            return(ps)
        if(isLayer)
            return(ps)
    }

    tyr <- as.numeric(
        sub("\\D+","", names(ps)))
    tyr. <- tyr
    if(all(is.na(tyr)))
        tyr <- names(ps)

    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}

    if(metric == 'area_ha'){
        fa_smp_lsm <- function(w, del0=TRUE){
            lst2 <- list(layer = w, del0 = del0)
            return(lst2)}
        args <- Map(function(w)
            fa_smp_lsm(w), as.list(ps))
        marg. <- c(list(FUN = function(x)
            do.call('tabuleRaster', x),
            x = args), marg)
    }else{
        fn_smp_lsm <- function(w, metric, smp_lsm){
            lst2 <- c(list(landscape = w, metric = metric), smp_lsm)
            return(lst2)}
    args <- Map(function(w)
        fn_smp_lsm(w, metric, smp_lsm), as.list(ps))
    marg. <- c(list(FUN = function(x)
        do.call('calculate_lsm', x),
        x = args), marg)}
    
    are.. <- do.call(getOption('fapp'), marg.)

    
    fnnm <- function(x,y){
        x$'layer' <- y
        return(x)}
    
        are.. <- Map(function(x,y)
          fnnm(x,y), are.., names(ps))

    lsdot <- list(...) 

    
    if(!'spread'%in%names(lsdot))
        mets <- tibble(do.call('rbind', are..))
        
        if('spread'%in%names(lsdot)){
            if(!lsdot$'spread'){                
                sum <- Map(function(x){
                    x <- sum(x[,'value'])
                    return(x[[1]])}, are..)
                tocum <- Map(function(x)
                    x[-1L,], are..)
                toadd. <- Map(function(x)
                    x[1L,], are..)
                toadd <- Map(function(x,y){
                    x[,'value'] <- y
                    return(x)}, toadd., sum)
                cum <- Map(function(x){
                    x[,'value'] <- cumsum(x[,'value'])
                    return(x)}, tocum)
                rest <- Map(function(x,y){
                    y[,'value'] <- x - y[,'value']
                    return(y)},
                    sum, cum)
                binded <- Map(function(x,y){
                    rbind(x,y)},toadd, rest)
                mets <- tibble(do.call('rbind', binded))
            }else{
                mets <- tibble(do.call('rbind', are..))

            }}

    class(mets) <- append('Indicator',class(mets))
    return(mets)
### \code{tibble}.
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
    
    ## Function 'gaugeIndicator' is used to compute ecosystem areas
    ## (default):
    am_areas <- gaugeIndicator(def,
                               mc.cores = 2)

    
})
