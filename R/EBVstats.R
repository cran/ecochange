EBVstats <- structure(function #EBV Stats
### This function is a wrapper of \code{cellStats} used to compute
### statistics of essential biodiversity variables.
(
    ccp, ##<< \code{RasterStack} or \code{NULL}. If \code{NULL} then
         ##\code{NULL} is returned.
    stats, ##<<\code{character}. vector of stats defined in
           ##\code{\link{cellStats}}. If missing then six summary
           ##statistics, including \code{'mean'}, \code{'sd'},
           ##\code{'min'}, \code{'max'}, are computed.
    ... ##<<Additional arguments in \code{cellStats}

){
    if(is.logical(ccp)){
        class(ccp) <- append('EBVstats', class(ccp))
        return(ccp)
    }
    if(missing(stats))
        stats <- c('min','mean', 'max', 'sd', 'skew')
    tyr <- names(ccp)
    sts <- Map(function(x,...)
        raster::cellStats(ccp, x,...),
        stats,...)
    sts1 <- Map(function(x)
        raster::ncell(x), raster::as.list(ccp))
    sts <- t(do.call('rbind', sts))
    n.grids <- as.vector(do.call('rbind', sts1))
    sts <- cbind(layer = tyr,n.grids, sts)
    sts <- suppressMessages(readr::type_convert(as_tibble(sts)))
    class(sts) <- append('EBVstats', class(sts))
    return(sts)
### \code{tibble}.
} , ex=function(){
    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):

    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- brick(path.)
    
    ## Tree-cover layers in the 'amazon' brick are both formatted and
    ## deforested:

    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)

    ## Deforestation Statistics:

    defstats <- EBVstats(def)

    ## barplot method:

    barplot(defstats)
})
