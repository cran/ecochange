EBVstats <- structure(function #EBV Stats
### This function is a wrapper of \code{cellStats} used to compute
### statistics for spatial indicators in the EBV class ecosystem
### structure. To derive the spatial indicators see functions
### \code{\link{echanges}} and \code{\link{sampleIndicator}} 
(
    ccp, ##<< \code{echanges}, or \code{RasterStack} or
         ##\code{NULL}. If \code{NULL} then \code{NULL} is returned.
    stats, ##<<\code{character}. vector of stats defined in
           ##\code{\link{cellStats}}. If missing then six summary
           ##statistics, including \code{'mean'}, \code{'sd'},
           ##\code{'min'}, \code{'max'}, are computed.
    ... ##<<Additional arguments in \code{cellStats}

){

        if('echanges'%in%class(ccp))
            ccp <- stack(unclass(ccp))
            
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
    ## return(list(sts=sts)) #erase me pleses!!
    ## sts <- suppressMessages(readr::type_convert(as_tibble(sts)))
    sts <- suppressMessages(
        utils::type.convert(as_tibble(sts), as.is = FALSE))
    class(sts) <- append('EBVstats', class(sts))
    return(sts)
### \code{tibble}.
} , ex=function(){
    ## RasterBrick of structural Essential Biodiversity Variables
    ## covering the extent of a location in the northern Amazon basin
    ## (Colombia) is imported:
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- brick(path.)
    
    ## Changes in layers of tree-canopy cover (TC) are computed by
    ## processing the 'amazon' brick:
    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)
    
    ## Function 'EBVstats' is used to compute ecosystem statistics
    st_amazon <- EBVstats(def)

    ## A plot of the 'st_amazon' object
    plot.EBVstats(st_amazon,
                   cex = 1.5,
                   xlab = 'Year',
                   ylab = 'Canopy cover (%)',
                   main = 'Ecosystem changes',
                   sub = 'Northern Amazon',
                   fill = 'Layer')
})
