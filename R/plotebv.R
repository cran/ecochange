plotebv <- structure(function #Plot EBV
### This function displays level plots for ecosystem remote sensing
### products using common scale-bars. The function is helpful to
### visualize patterns in biodiversity indicators.
(
    ebv, ##<<\code{Raster*}. Raster Object.
    col.regions = rev(viridis_pal(option="D")(255)), ##<<\code{}. Color
                                                     ##palette. If
                                                     ##null then
                                                     ##\code{viridis_pal(option
                                                     ##= 'D')} is
                                                     ##implemented.
    ... ##<<\code{}. Further arguments in \code{panel.levelplot()}
) {
    if(is.logical(ebv))
        return(plot(ebv))
    
    ## if(is.null(col.regions)){
    ##     pal.n <- max(ebv@'data'@'max')
    ##     col.regions <- rev(viridis_pal(option="D")(pal.n))}

      if (requireNamespace("rasterVis", quietly = TRUE)) {

    plt <- rasterVis::levelplot(ebv,
                                margin = list(x = TRUE,
                                              y = TRUE),
                                col.regions = col.regions,
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
      }else{
          print("Package 'rasterVis' needed for this function to display panels with improved scale-bar")
         plt <- raster::plot(ebv, col = col.regions)
          }
    return(plt)
### \code{levelplot}.
} , ex=function() {
    ## Warnings from GDAL/PROJ are suppressed.

    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):
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
    
    ## Function 'plotebv' allows comparing rasters using a common scale bar:
    suppressWarnings(
    plotebv(def)
)
})
