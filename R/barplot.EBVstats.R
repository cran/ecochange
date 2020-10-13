barplot.EBVstats <- structure(function #barplot EBV Stats
###A barplot of \code{\link{EBVstats}} is printed.
(
    height, ##<< \code{tibble} of \code{EBVstats}.
    ... ##<< Additional arguments in \code{\link{barplot}}.
    

){
    opar <- par('oma')
    on.exit(par(opar))
    height <- na.omit(height)
    if(!all(c('mean','sd')%in%names(height)))
        stop("Should provide at least: 'mean', and 'sd'")
    height. <- height$'mean'
    names  <- height$'layer'
    error  <-  height$'sd'
    maxLim <- 1.1* max(mapply(sum, height., error))
    ylim = c(0,maxLim)
    par(oma = c(0,1,0,0))
    bp <- barplot(height., names.arg = names,
                  ylim = ylim, mgp = c(2.8,1,0),
                  lwd = 2, cex.lab = 1.5, cex.axis = 1.5,
                  cex.names = 1.5,...)
    arrows(x0 = bp, y0 = height., y1 = height. + error,
           angle = 90, lwd = 1.3)
    arrows(x0 = bp, y0 = height., y1 = height. - error,
           angle = 90, lwd = 1.3)
### Plot of \code{EBVstats}.
} , ex=function(){
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

    ## Deforestation Statistics:

    defstats <- EBVstats(def)

    ## Barplot:

    barplot(defstats)
})
