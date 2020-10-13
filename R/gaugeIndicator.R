gaugeIndicator <- structure(function #Gauge Indicator
### This function processes stacks of essential biodiversity variables
### (\code{ebv} stacks) to gauge indicators related to horizontal
### ecosystem extent, degradation, fragmentation, among others. To
### sample the indicators in fixed-size grids across \code{ebv} stacks
### see \code{\link{sampleIndicator}}.
                      ##details<< Coordinate system of the spatial
                      ##units should be \code{ UTM}. Metrics other
                      ##than \code{'lsm_l_tafc'} are calculated
                      ##implementing \code{\link{calculate_lsm}}.

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
    pol,  ##<<\code{RasterStack}. \code{ebv} stack such as that
          ##produced by \code{\link{rsp2ebv}}.
    ind = 'lsm_l_tafc', ##<<\code{character}. An \code{ebv}
                         ## metric. \code{'lsm_l_tafc'} computes total
                         ## forest-cover areas (ha). See
                         ## \code{\link{calculate_lsm}} to compute
                         ## other metrics. Default
                         ## (\code{'lsm_l_tafc'}).
    ... ##<< additional arguments in \code{\link{calculate_lsm}}.

) {
    inw <- grepl('lsm_l_tafc', ind)
    yswhat <- ind[inw]  
    ntwhat <- ind[!inw]
    tyr <- as.numeric(
        sub("\\D+","", names(pol)))
    tyr. <- tyr
    if(all(is.na(tyr)))
        tyr <- names(pol)
        are. <- tibble()
    if(!length(yswhat) == 0)
        are. <- ftibb(pol, tyr = tyr)
        are.. <- tibble()
        if(!length(ntwhat) == 0){
            are.. <- calculate_lsm(pol,
                                   what = ind, ...)
            years <- pull(unique(are..['layer']))
            names(years) <- tyr
            yr.. <- pull(are..['layer'])
            are..['layer'] <- names(years)[yr..]
            if(!all(is.na(tyr.)))
                are..['layer'] <- as.numeric(names(years)[yr..])
        }
        mets <- rbind(are., are..)
        class(mets) <- append('Indicator',class(mets))
        return(mets)
### \code{tibble}.
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

    ## Binary layers of Forest and non-forest areas (keep.ebv = FALSE):

    suppressWarnings(
        def_bin <- deforest(amazon, names(amazon)[grepl('TC', names(amazon))],
                            ebv.vals = 0:100,
                            remnant.areas = TRUE, keep.ebv = FALSE, mc.cores = 2)
    )

    ## Areas for both the binary and the continuous forest layers:

    defareas <- suppressWarnings(gaugeIndicator(def_bin, ind = 'lsm_c_ca'))
    defclasses <- suppressWarnings(gaugeIndicator(def, ind = 'lsm_c_ca'))

    ## plot.gaugeIndicator method:

    plot(defareas)
    plot(defclasses)

})
