plotebv <- structure(function #Plot EBV
### This function prints levelplots of Essential Biodiverstiy
### Variables using a common spatial scale bar. The function is helpful
### to compare EBV indicators.
(
    ebv, ##<<\code{Raster*}. Raster Object.
    ...
) {
    plt <- rasterVis::levelplot(ebv,
                                margin = list(x = TRUE,
                                              y = TRUE),
                                col.regions = rev(viridis_pal(option="D")(255)),
                                font = 1,
                                pretty = T,#)
                                xlab = NULL,
                                ylab = NULL,
                                scales = list(x = list(cex = 1.1),
                                              y = list(cex = 1.1),
                                              xlab = list(cex = 1.1)),
                                par.strip.text = list(col = 'black',
                                                      font = 1,
                                                      cex = 1.2),
                                panel = function(x, y, ...){
                                    panel.grid(v = -1, h = -1,
                                               col = 'grey95',
                                               lty = 1)
                                    panel.levelplot(x, y, ...)})
    print(plt)
### \code{levelplot}.
} , ex=function() {
    ## Warnings from GDAL/PROJ are suppressed.

    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):

    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- suppressWarnings(brick(path.))
    
    ## Tree-cover layers in the 'amazon' brick are both formatted and
    ## deforested:

    suppressWarnings(
        def <- deforest(amazon, names(amazon)[grepl('TC', names(amazon))],
                        ebv.vals = 0:100,
                        remnant.areas = TRUE, keep.ebv = TRUE, mc.cores = 2)
    )

    ## Plot:
    suppressWarnings(
        plotebv(def)
    )
})
