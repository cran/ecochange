gaugeIndicator <- structure(function #Gauge Indicator
### This function processes either regions of interest (polygons) or
### stacks of essential biodiversity variables (\code{ebv} stacks) to
### calculate indicators related to horizontal ecosystem extent,
### degradation, fragmentation, among others. To sample the indicators
### in fixed-size grids across \code{ebv} stacks see
### \code{\link{sampleIndicator}}.
                      ##details<< Coordinate reference system of the
                      ##spatial units must have metric units \code{
                      ##UTM}. Indicators other than ecosystem areas
                      ##are calculated implementing
                      ##\code{\link{calculate_lsm}}.
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
    pol, ##<<\code{SpatialPolygonsDataFrame} or \code{Raster*}. Region
         ##of interest or Stack of essential biodiversity variables.
    ..., ##<<If \code{pol} is not a \code{Raster*} then additional
         ##arguments in \code{\link{echanges}}.
    met = 'area_ha', ##<<\code{character}. An \code{ebv}
                       ##metric. Default \code{'area_ha'} computes
                       ##ecosystem areas (ha) at class level. See
                       ##argument 'metric' in \code{\link{list_lsm}}
                       ##to compute other metrics.
    ## spread = TRUE, ##<<\code{logical}. Try to dimension the ecosystem
    ##                ##variable across space and along time.
    smp_lsm = list(level = 'landscape'), ##<<\code{list}. If
                                         ##\code{met} is not
                                         ##\code{'area_ha'} then
                                         ##additional arguments in
                                         ##\code{\link{calculate_lsm}}. 
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses 60 percent of the
                                          ##cores.
) {

    isLayer <- 'lyrs'%in%names(list(...))
    if(isLayer)
        isLayer <- is.null(list(...)$'lyrs')
    
    if(inherits(pol, getOption('inh'))|is.logical(pol)){
        pol. <- pol
        ## pol <- echanges(pol,mc.cores = mc.cores, spread = spread, ...)
        pol <- echanges(pol,mc.cores = mc.cores, ...)
        if(is.null(pol.)|is.logical(pol.))
            return(pol)
        if(isLayer)
            return(pol)
    }

    tyr <- as.numeric(
        sub("\\D+","", names(pol)))
    tyr. <- tyr
    if(all(is.na(tyr)))
        tyr <- names(pol)

    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}

    if(met == 'area_ha'){
        ## fn_smp_lsm <- function(w, del0=TRUE){
        ##     lst2 <- list(layer = w, del0 = del0)
        ##     return(lst2)}
        ## args <- Map(function(w)
        ##     fn_smp_lsm(w), as.list(pol))
        fa_smp_lsm <- function(w, del0=TRUE){
            lst2 <- list(layer = w, del0 = del0)
            return(lst2)}
        args <- Map(function(w)
            fa_smp_lsm(w), as.list(pol))
        marg. <- c(list(FUN = function(x)
            do.call('tabuleRaster', x),
            x = args), marg)
    }else{
        fn_smp_lsm <- function(w, met, smp_lsm){
            lst2 <- c(list(landscape = w, metric = met), smp_lsm)
            return(lst2)}
    args <- Map(function(w)
        fn_smp_lsm(w, met, smp_lsm), as.list(pol))
    marg. <- c(list(FUN = function(x)
        do.call('calculate_lsm', x),
        x = args), marg)}
    
    are.. <- do.call(getOption('fapp'), marg.)

    fnnm <- function(x,y){
        x$'layer' <- y
        return(x)}
    
    if(met == 'area_ha'){
        are.. <- Map(function(x,y)
          fnnm(x,y), are.., names(pol))
    }

    lsdot <- list(...) 

    if(!'spread'%in%names(lsdot))
        mets <- tibble(do.call('rbind', are..))

        if('spread'%in%names(lsdot)){
            if(!lsdot$'spread'){
                print('you are good!')
                tocum <- Filter(function(x)nrow(x) != 1, are..)
                torest <- Filter(function(x)nrow(x) == 1, are..)
                cum <- Map(function(x){
                    x[,'value'] <- cumsum(x[,'value'])
                    return(x)}, tocum)
                rest <- Map(function(x,y){
                    y[,'value'] <- x[,'value'] - y[,'value']
                    return(y)},
                    torest, cum)
                binded <- Map(function(x,y){
                    rbind(x,y)},torest, rest)
                bndly <- Map(function(x){
                    x[,'layer'] <- x[1L,1L]
                    return(x)},
                    binded)
                bndcl0 <- Map(function(x){
                    x[1L,'class'] <- 0
                    return(x)}, bndly)
                mets <- tibble(do.call('rbind', bndcl0))
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
