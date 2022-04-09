plot.echanges <- structure(function #Visualize ecosystem changes
### This function aims to display level plots for remote sensing
### products using common scale-bars via the implementation of the
### suggested \code{rasterVis}. If this is not installed then method
### \code{plot} of \code{raster} package is used.
(
    x, ##<<\code{Raster*}. Raster Object.
    y, ##<<\code{}. Color palette. If missing or the suggest
                 ##'viridis' is not installed then
                 ##\code{\link{terrain.colors}} is used.
    ... ##<<\code{}. Additional arguments in \code{panel.levelplot}.
) {

    if('echanges'%in%class(x)){
        ## x <- brick(unclass(x))
        ebv <- stack(unclass(x))
    }else{
        ebv <- x
    }
    ## class(x) <- 'list'
    ## ebv <- stack(x)
    
    mx.. <- max(mapply(function(x)
        x@data@max, raster::as.list(ebv)))
    if(is.logical(ebv))
        return(plot(ebv))
        dep <- 'viridis'
        if(missing(y)){
            y  <-  rev(terrain.colors(mx..))
            if(requireNamespace(dep, quietly = TRUE)&dep%in% (.packages())){
                y <- do.call('viridis_pal', list(direction = -1))
            }## else{
            ##    print("Package 'viridis' can help improve color palette")
            ##    }
        }

        indots <- list(...)
        cex <- 1
        if('cex'%in%names(indots))
            cex <- indots$'cex'

            xlab = NULL
        if('xlab'%in%names(indots))
            xlab <- indots$'xlab'

            ylab = NULL
        if('ylab'%in%names(indots))
            ylab <- indots$'ylab'

            main <- NULL
        if('main'%in%names(indots))
            main <- indots$'main'

            
        dep1 <- 'rasterVis'
        if(requireNamespace(dep1, quietly = TRUE)&dep1%in% (.packages())){
            plt <- rasterVis::levelplot(ebv,
                                        margin = list(x = TRUE,
                                                      y = TRUE),
                                        col.regions = y,
                                        font = 1,
                                        pretty = T,#)
                                        xlab = xlab,
                                        ylab = ylab,
                                        main = main,
                                        scales = list(cex = c(cex,cex),## x = list(cex = cex),
                                                      ## y = list(cex = cex),
                                                      xlab = list(cex = cex),
                                                      ylab = list(cex = cex)),
                                        par.strip.text = list(col = 'black',
                                                              font = 1,
                                                              cex = cex),
                                        panel = function(x, y, ...){
                                            panel.grid(v = -1, h = -1,
                                                       col = 'grey95',
                                                       lty = 1)
                                            panel.levelplot(x, y, ...)})
        }else{
            ## print("Package 'rasterVis' can help improve panel aesthetics")
            plt <- raster::plot(ebv, col = y)
        }
        return(plt)
### \code{levelplot}.
} , ex=function() {
    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- brick(path.)
    
    ## Changes in layers of tree-canopy cover (TC) in the 'amazon'
    ## brick are computed:
    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)
    
    plot.echanges(def)
})
