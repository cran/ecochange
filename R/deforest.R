deforest <- structure(function #Deforest EBV
###This function can extract/subtract deforested areas from essential
###biodiversity variables (\code{ebv}) while focusing the analysis on
###a predefined area of occupancy.
                      ##references<< {Jetz, W., McGeoch, M. A.,
                      ##Guralnick, R., Ferrier, S., Beck, J., Costello,
                      ##M. J., ... & Meyer, C. (2019). Essential
                      ##biodiversity variables for mapping and
                      ##monitoring species populations. Nature Ecology &
                      ##Evolution, 3(4), 539-551.}
                      ##
                      ##{Pekel, J. F., Cottam, A., Gorelick, N., & Belward,
                      ##A. S. (2016). High-resolution mapping of global
                      ##surface water and its long-term changes. Nature,
                      ##540(7633), 418-422.}
                      ## 
                      ##{Hansen, M. C., Potapov, P. V., Moore, R.,
                      ##Hancher, M., Turubanova, S. A., Tyukavina, A.,
                      ##... & Kommareddy, A. (2013). High-resolution
                      ##global maps of 21st-century forest cover
                      ##change. science, 342(6160), 850-853.}
                      ## 
                      ## {Sexton, J. O., Song, X. P., Feng, M.,
                      ##Noojipady, P., Anand, A., Huang, C., ... &
                      ##Townshend, J. R. (2013). Global, 30-m
                      ##resolution continuous fields of tree cover:
                      ##Landsat-based rescaling of MODIS vegetation
                      ##continuous fields with lidar-based estimates
                      ##of error. International Journal of Digital
                      ##Earth, 6(5), 427-448.}
(
    stk, ##<<\code{Raster*}. Stack of \code{ebv} such as that produced
         ##by \code{\link{rsp2ebv}} containing a \code{'lossyear'}
         ##layer.
    ebv, ##<<\code{character}. Name of the target layers in
         ##\code{stk}.
    loss = 'lossyear', ##<<\code{character}. Name of the
                       ##\code{'lossyear'} layer. Only for the case
                       ##that it has a different name in
                       ##\code{stk}. Default \code{'lossyear'}.
    aoo, ##<<\code{character}. Name of an optional binary raster in
         ##\code{stk} used to focus the analysis on a specific Area of
         ##Occupancy.
    ebv.vals, ##<< \code{numeric}.  Cell values in \code{ebv}. If
              ##missing then the whole range of values is processed
    loss.vals, ##<<\code{numeric}. Cell values in \code{loss}. If
               ##missing then the function try to extract the values
               ##from the target layers in \code{ebv}. Otherwise the
               ##whole range of values in \code{loss} is processed.
    incremental = TRUE, ##<<\code{logical}. Develop incremental
                        ##instead of discrete masking along the
                        ##\code{ebv.vals}. Default \code{TRUE}.
    remnant.areas = TRUE, ##<<\code{logical}. Extract from \code{ebv}
                          ##remnant areas instead of deforested areas
                          ##. Default \code{TRUE}.
    keep.ebv = FALSE, ##<< \code{logical}. Keep in the extracted areas
                      ##the corresponding cell values in
                      ##\code{ebv}. Default \code{FALSE} produces
                      ##binary masks: \code{c(0,1)}.
    mc.cores = round(detectCores()*0.6,0) ##<<\code{numeric}. The
                                          ##number of cores. Default
                                          ##uses around 60 percent CPU
                                          ##capacity.
    
)
{
    ebv <- raster::subset(stk,ebv)
    loss <- raster::subset(stk,loss)
    if(!missing(aoo)){
        aoo <- raster::subset(stk,aoo)
        mskd <- mask(stack(ebv, loss), aoo, maskvalue = 0)
        ebv  <- subset(mskd, names(ebv))
        loss  <- subset(mskd, names(loss))}
    intr <- seq(fmmx(loss,'min'),fmmx(loss,'max'), 1)
    if(missing(loss.vals)){
        if(any(grepl('[[:digit:]]+',
                     names(ebv)))){
            loss.vals <- as.numeric(
                unique(unlist(regmatches(names(ebv),
                                         gregexpr("[[:digit:]]+", names(ebv))))))}
        else{loss.vals <- intr}}
    loss.vals. <- loss.vals <- scaleYear(loss.vals)
    if(any(!loss.vals%in%intr)){
        yrlk <- loss.vals[!loss.vals%in%intr]
        stop(paste(getOption('miss'),
                   yrlk,'; ', sep = ''))}
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}
    marg. <- c(list(FUN = function(x)
        f8(loss, x), x = loss.vals.), marg)
    if(incremental){
        marg.[['FUN']] <- function(x)
            f8(loss, 1:x)}
    w <- do.call(getOption('fapp'), marg.)
    X0 <- setValues(projectExtent(ebv, crs = crs(ebv)), NA)
    names(w) <- loss.vals
    rpc <- sapply(w,function(x)
        all(is.infinite(c(x@'data'@'min',x@'data'@'max'))|
            sum(c(x@'data'@'min',x@'data'@'max')) == 0))
    w[names(rpc)[rpc]] <- X0
    if(any(loss.vals == 0)){
        w[[1L]] <- X0}
    w <- brick(w)
    tr <- ebv
    if(!missing(ebv.vals)){
        tr <- stack(Map(function(x)
            f16(x, ebv.vals),
            raster::as.list(ebv)))}
    w <- mask(tr, w, inverse = remnant.areas, updatevalue = 0)
    if(!keep.ebv){
        w <- reclassify(w, rcl = recTable(recl = 0,oneFirst = FALSE))}
    names(w) <- paste0(names(ebv),'_',loss.vals)
    return(w)
### \code{RasterBrick}.
} ,
ex=function() {

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
    
    ## Function 'plotebv' allows comparing rasters using a common scale bar:
    suppressWarnings(
    plotebv(def)
)
})
