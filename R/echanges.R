echanges <- structure(function #Ecological changes
###This function can compute ecological changes from spatial variables, focussing analyses on affected or unaffected areas and across predefined species distribution ranges.
                      ##references<< {Jetz, W., McGeoch, M. A.,
                      ##Guralnick, R., Ferrier, S., Beck, J.,
                      ##Costello, M. J., ... & Meyer,
                      ##C. (2019). Essential biodiversity variables
                      ##for mapping and monitoring species
                      ##populations. Nature Ecology & Evolution, 3(4),
                      ##539-551.}
                      ##
                      ##{Pekel, J. F., Cottam, A., Gorelick, N., &
                      ##Belward, A. S. (2016). High-resolution mapping
                      ##of global surface water and its long-term
                      ##changes. Nature, 540(7633), 418-422.}
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
    stk, ##<<\code{Raster*} or \code{SpatialPolygonsDataFrame}. Stack
         ##of spatial variables or polygon geometry.
    eco = names(stk[[1:(nlayers(stk)-1)]]), ##<<\code{character}. Regular
                                            ##expression matching the
                                            ##name of the ecological
                                            ##variable. Default uses
                                            ##the names of the first
                                            ##\code{1:(n-1)} layers in
                                            ##\code{souk} are
                                            ##processed.
    change = names(stk[[(nlayers(stk))]]), ##<<\code{character}. Name
                               ##of the ecosystem-change variable. If
                               ##missing, then the name of the last
                               ##layer in \code{stk} is processed.
    sp_dist, ##<<\code{character}. Name of an alternative species
             ##distribution layer covering the region of interest.
    eco_range = c(1,100), ##<< \code{numeric}. Range of values in the
                          ##ecological variable.
    change_vals = 1:19, ##<<\code{numeric}. Vector of values in the
                        ##change variable.
    sp_dist_range = c(1,1), ##<<\code{numeric}. Range of values in the
                            ##species distribution layer.
    spread = TRUE, ##<<\code{logical}. Spread changes according to
                   ##both the number of layers in \code{eco} and the
                   ##values in \code{change_vals}. Users do not need
                   ##to change this argument. It is used by other
                   ##rouines to fasten computation of ecosystem
                   ##horizontal extents. If \code{FALSE} then two sets
                   ##of layers are extrated, including masks of
                   ##ecological variables and layers of
                   ##changes. Default \code{TRUE}.
    get_unaffected = TRUE, ##<<\code{logical}. Process unaffected
                           ##areas. If \code{FALSE} then ranges of
                           ##values in the ecological variable across
                           ##the changed areas are extracted. Default
                           ##\code{TRUE}.
    binary_output = FALSE, ##<<\code{logical}. Produce binary outputs
                           ##(masks). If \code{FALSE} then ranges of
                           ##values of the ecological variable are
                           ##maintained. Default \code{FALSE}.
    mc.cores = round(detectCores()*0.6,0), ##<<\code{numeric}. The
                                           ##number of cores. Default
                                           ##uses around 60 percent
                                           ##CPU capacity.
    ... ##<<If \code{stk} is a polygon then additional arguments in
        ##\code{\link{rsp2ebv}}.
)

{

    if(getOption('isWin')&!getOption('hasOsgeo4w')){
        ## if(isWin&!hasOsgeo4w){
        print('missing OSGeo4W64 binary')
        return(FALSE)
    }
 
    if(is.logical(stk))
        return(stk)
    
    ## unlink(file.path(tempdir(),'ecochange'), recursive = TRUE)
    unlink(file.path(tempdir(),'ecochange','change'), recursive = TRUE)
    
        if(length(eco_range) > 2){
            warning("'eco_range': the vector has length > 2 and only its range will be used")
            eco_range  <- range(eco_range)
    }

    isLayer <- 'lyrs'%in%names(list(...))
    if(isLayer)
        isLayer <- is.null(list(...)$'lyrs')
    
    if(inherits(stk, getOption('inh'))){
        stk. <- stk
        stk <- rsp2ebv(stk,mc.cores = mc.cores, ...)
        if(is.null(stk.))
            return(stk)
        if(isLayer)
            return(stk)
    }

        ecopatrn <- gsub("\\d+", "", eco)
        if(!all(grepl(ecopatrn[1L], ecopatrn))){
            stop("Ambiguous layer names: provide arguments 'eco' and 'change'")}
        
    reg2rst <- function(exp){ 
        exp. <- paste(exp, collapse = '|')
        exp <- names(stk)[grepl(exp., names(stk))]
        rst <- raster::subset(stk,exp)
        return(rst)}
        eco <- reg2rst(eco)
        change <- reg2rst(change)
        if(!missing(sp_dist)){
            sp_dist <- reg2rst(sp_dist)}
    
        if(!missing(sp_dist)){
            ## for the case of species d.ranges
        ## sp_dist <- raster::subset(stk,sp_dist)
        marg. <- c(list(FUN = function(x)
            msk_1(x, sp_dist,
                 remnant = FALSE,
                 keep = TRUE,
                 perc=eco_range, tim = sp_dist_range),
            x = raster::as.list(eco)),marg)
    eco <- stack(do.call(getOption('fapp'), marg.))
    }

    if(dim(eco)[3] > 1){
        print("'eco' has length > 1: matching names of 'eco' with values in 'change_vals'")
        change_vals <- nm2yr(eco)
    }
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}
    
        if(!spread){
            print("'spreads = FALSE: Fast-computing inputs for ecosystem horizontal extents")
            eco_range[eco_range==0] <- 1 # this is weird, verify it!
            marg. <- c(list(FUN = function(x,y)
                msk_0(x, y,
                      perc=eco_range,
                      tim = c(1, max(change_vals))),
                x = raster::as.list(eco),
                y = raster::as.list(change)),
                marg)
            w <- do.call(getOption('fapp'), marg.)
            w <- stack(w)
            return(w)
        }        

    change[change == 0] <- NA

## return(list(eco = eco, change = change))

  ## ## if(missing(td)){
  ##   td <- file.path(tempdir(),'ecochange','change')
  ##   ## td <- file.path(tempdir(),'ecochange')
  ##   if(!file.exists(td)){
  ##     dir.create(td)}
  ##   td  <- td
    
    marg. <- c(list(FUN = function(x,y)
        msk_2(x, change,
              remnant = get_unaffected,
              keep = !binary_output,
              perc=eco_range, tim = c(0,y)),
      ## , td = td),
        x = raster::as.list(eco),
        y = change_vals), marg)

    w <- stack(do.call(getOption('fapp'), marg.))

    ## if(!missing(sp_dist)){
    ##     dr <- file.path(tempdir(),'ecochange','aoo')
    ##     drr <- file.path(dr, dir(dr))
    ##     file.remove(drr,dr)
    ## }
    
    return(w)
### \code{RasterBrick}.
} ,
ex=function() {

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
