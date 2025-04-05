tabuleRaster <- structure(function # Fast tabulation of pixel values
### This function generates frequency tables for scenes in ecosystem
### remote sensing products by wrapping \code{\link[rasterDT]{rasterDT}}. The
### function is mapped by \code{\link{gaugeIndicator}} to optimize
### computation of ecoystem extents.
(
    layer = '',      ##<<\code{character}. File path to an ERSP scene.
    del0 = TRUE,    ##<<\code{logical}. Remove the 0-count categories.
    useNA = "no",   ##<<\code{logical}. Include NA values. This
                    ##argument is passed to \code{\link[rasterDT]{freqDT}}.
    n256 = FALSE      ##<<\code{logical}. Do the raster
                      ##contains less than 256 unique values?
) {
    allowedRastClass <- c('RasterLayer', 'RasterBrick', 'RasterStack')
    if (!class(layer) %in% c(allowedRastClass, 'character')){
        stop('Class not RasterLayer or character')
    }
    if (class(layer) %in% c('character')){
        if (!file.exists(layer)){
            stop('File not found')
        }
    }
    if (n256){
        if (class(layer) %in% allowedRastClass){
            if (layer[[1]]@file@name != ''){
                layerPath <- layer[[1]]@file@name
            } else {
                layerPath <- paste0(tempfile(), '.tif')
                writeRaster(layer, filename = layerPath)
            }
            layer <- layerPath
        }
        ## gdalLog <- capture.output(gdalUtilities::gdalinfo(datasetname = layer, hist = TRUE))
        gdalLog <- capture.output(sf::gdal_utils(source = layer, options = c('-hist')))
     ## return(gdalLog)   
        (nbands <- grep('Band [[:digit:]]{1,} Block', gdalLog))
        ansList <- list()
        for(n in 1:length(nbands)){ # n <- 1
            (bucxml <- as.numeric(sub('buckets.+', '', grep('buckets ', gdalLog, value = TRUE)))[n])
            (minxml <- as.numeric(gsub('.+from | to.+', '', grep('buckets ', gdalLog, value = TRUE)) )[n] )
            (maxxml <- as.numeric(gsub('.+to |:', '', grep('buckets ', gdalLog, value = TRUE)))[n] )
            (histxml <- as.numeric(strsplit(split = '[[:space:]]', gsub("^ |^  ", "", 
                                                                        gdalLog[grep('buckets', gdalLog)[n]+1]
                                                                        ))[[1]]))
            labs <- seq(from = minxml, to = maxxml, length.out = bucxml)
                                        # length(histxml)
            df2 <- data.frame(labs, nwlab = c(ceiling(labs[1]),
                                              round(labs[2:(bucxml-1)]),
                                              floor(labs[bucxml])), 
                              val = histxml)
            hist2 <- aggregate(df2$val, by = list(df2$nwlab), sum)
            ## result <- data.frame(id = hist2$Group.1, count = hist2$x, stringsAsFactors = FALSE)
            result <- data.frame(id = hist2$Group.1, cnt = hist2$x, stringsAsFactors = FALSE)
            ## Delete 0 count values
            if(del0){
                ## result <- subset(result, count > 0)
                result <- subset(result, result$'cnt' > 0)
            }
            ansList[[n]] <- result
        }
        if(length(nbands) == 1){
            result <- ansList[[1]]
        } else {
            result <- ansList
        }
    } else {
        if (class(layer) %in% c('character')){
            layer <- tryCatch(raster::stack(layer), error = function (e) stop( "Can't open raster layer"))
        }
        if(!grepl('+units=m',crs(layer))){
            warning("missing +units=m in 'crs(layer)'")
        }
        res. <- Reduce('*',raster::res(layer))
        freqTable <-  rasterDT::freqDT(layer, useNA = useNA)
        if(length(freqTable$'freq') == 0)
            ## freqTable  <- data.table(ID = NA, freq = NA)
            freqTable  <- data.frame(ID = NA, freq = NA)
        if (all(class(freqTable) == 'list')){
            result <- lapply(freqTable, function(x){
                data.frame(class = x$ID, class = NA, metric = 'area_ha',
                           value = x$freq * res. * 1E-4, stringsAsFactors = FALSE )
            })
        } else {
            result <- data.frame(class = freqTable$ID, id = NA,metric = 'area_ha', value = freqTable$freq * res. * 1E-4, stringsAsFactors = FALSE )
        }
    }
    result  <- cbind(layer = names(layer), level = 'class', 
                     result)
    return(result)
### \code{data.frame}.
} , ex = function() {
    ## \donttest{
    ## tabuleRaster(raster(volcano), n256 = FALSE)
    ## }
})
