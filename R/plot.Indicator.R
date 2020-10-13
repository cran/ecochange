plot.Indicator <- structure(function#Plot indicator
###A plot of \code{\link{gaugeIndicator}} is printed.
(
    x, ##<<\code{\link{tibble}}. Data set of indicators such as that
       ##produced by \code{\link{gaugeIndicator}}.
    ... ##<<Further arguments in \code{\link{aes}}.
){
    data <- x
    ell <- list(...)

    ggplot2::ggplot(data,
                    aes(x = factor(.data$layer, levels = unique(.data$layer)),
                        y = .data$value, fill = fct_rev(factor(.data$class)),...)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", color = 'black') +
    ggplot2::xlab('Layer') + ggplot2::ylab(unique(.data$metric)) +
    labs(fill = '') +
    ggplot2::theme(legend.position="right",
          aspect.ratio = 1/1, text = element_text(size = 14))
} , ex=function(){
    ## Warnings from GDAL/PROJ are suppressed.

    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):

    path. <- system.file('amazon.grd', package = 'ecochange')
    amazon <- suppressWarnings(brick(path.))
    
    ## Tree-cover layers in the 'amazon' brick are both formatted and
    ## deforested:
    suppressWarnings(
        def <- deforest(amazon, names(amazon)[grepl('TC', names(amazon))],
                        ebv.vals = 0:100,
                        remnant.areas = TRUE, keep.ebv = TRUE, mc.cores = 2)
    )

    ## Binary layers of Forest and non-forest areas (keep.ebv = FALSE):
    suppressWarnings(
        def_bin <- deforest(amazon, names(amazon)[grepl('TC', names(amazon))],
                            ebv.vals = 0:100,
                            remnant.areas = TRUE, keep.ebv = FALSE, mc.cores = 2)
    )

    ## Areas for both the binary and the continuous forest layers:

    defareas <- suppressWarnings(gaugeIndicator(def_bin, ind = 'lsm_c_ca'))
    defclasses <- suppressWarnings(gaugeIndicator(def, ind = 'lsm_c_ca'))

    ## Plot the indicators:

    plot(defareas)
    plot(defclasses)

})
