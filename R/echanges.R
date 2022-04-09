echanges <- structure(function#Ecosystem changes
###This function produces ecosystem-change maps by masking cell values
###in a layer of ecosystem changes over a target set of ecosystem
###variables. The function allows focusing the ecosystem-change
###analysis on a species distribution range.
                      ##references<< {Jetz, W., McGeoch, M. A.,
                      ##Guralnick, R., Ferrier, S., Beck, J.,
                      ##Costello, M. J., ... & Meyer,
                      ##C. (2019). Essential biodiversity variables
                      ##for mapping and monitoring species
                      ##populations. Nature Ecology & Evolution, 3(4),
                      ##539-551.}
                      ##
                      ##{Hansen, M. C., Potapov, P. V., Moore, R.,
                      ##Hancher, M., Turubanova, S. A., Tyukavina, A.,
                      ##... & Kommareddy, A. (2013). High-resolution
                      ##global maps of 21st-century forest cover
                      ##change. science, 342(6160), 850-853.}
                      ##
                      ##{Pekel, J. F., Cottam, A., Gorelick, N., &
                      ##Belward, A. S. (2016). High-resolution mapping
                      ##of global surface water and its long-term
                      ##changes. Nature, 540(7633), 418-422.}
                      ##
                      ##{Pereira, H.M., Ferrier, S., Walters,
                      ##M., Geller, G.N., Jongman, R.H.G., Scholes,
                      ##R.J., Bruford, M.W., Brummitt, N., Butchart,
                      ##S.H.M., Cardoso, A.C. and Coops, N.C.,
                      ##2013. Essential biodiversity
                      ##
                      ## {Sexton, J. O., Song, X. P., Feng, M.,
                      ##Noojipady, P., Anand, A., Huang, C., ... &
                      ##Townshend, J. R. (2013). Global, 30-m
                      ##resolution continuous fields of tree cover:
                      ##Landsat-based rescaling of MODIS vegetation
                      ##continuous fields with lidar-based estimates
                      ##of error. International Journal of Digital
                      ##Earth, 6(5), 427-448.}
                      ##variables. Science, 339(6117), pp.277-278}.
(
    ps, ##<<\code{RasterStack} or
        ##\code{SpatialPolygonsDataFrame}. Stack of spatial data,
        ##including the target ecosystem variables, a layer of
        ##changes, and an alternative layer of a species distribution
        ##range. This argument can also be a polygon geometry used to
        ##integrate such spatial data via implementation of
        ##\code{\link{rsp2ebv}}; see the ellipsis term below.
    eco = names(ps[[1:(nlayers(ps)-1)]]), ##<<\code{character}. Regular
                                          ##expression matching names
                                          ##of a subset of layers
                                          ##representing the target
                                          ##ecosystem
                                          ##variables. Default matches
                                          ##names of the first
                                          ##\code{1:(n-1)} layers in
                                          ##\code{ps}.
    change = names(ps[[(nlayers(ps))]]), ##<<\code{character}. Name of
                                         ##the layer of ecosystem
                                         ##changes. Default matches
                                         ##the name of the last layer
                                         ##in \code{ps}.
    sp_dist, ##<<\code{character}. Name of an alternative layer
             ##representing a species distribution range. If missing
             ##then this argument is ignored.
    eco_range = c(1,100), ##<< \code{numeric}. Range of values in the
                          ##target ecosystem variable.
    change_vals = 1:19, ##<<\code{numeric}. Vector of values in the
                        ##layer of ecosystem changes.
    sp_dist_range = c(1,1), ##<<\code{numeric}. Range of values in the
                            ##alternative layer of species.
                            ##distribution range. This argument is
                            ##ignored if \code{sp_dist} is missing.
    spread = TRUE, ##<<\code{logical}. Spread representation of
                   ##ecosystem changes. Users do not need to change
                   ##this argument. It is used by other rouines to
                   ##fastening computation of ecosystem horizontal
                   ##extents. If \code{FALSE} then the function mask
                   ##cell values in the target ecosystem variables
                   ##over over the layer of ecosystem changes. Default
                   ##\code{TRUE}.
    get_unaffected = TRUE, ##<<\code{logical}. Extract unaffected
                           ##areas. If \code{FALSE} then pixel
                           ##values of the ecological variable across
                           ##the changed areas are extracted. Default
                           ##\code{TRUE}.
    binary_output = FALSE, ##<<\code{logical}. Produce binary outputs
                           ##(masks). If \code{FALSE} then ranges of
                           ##values of the ecological variable are
                           ##maintained. Default \code{FALSE}.
    noDataValue = 0, ##<<\code{numeric}. Output NoDataValue. Default
                     ##\code{0}.
    mc.cores = round(detectCores()*0.6,0), ##<<\code{numeric}. The
                                           ##number of cores. Default
                                           ##uses around 60 percent
                                           ##of the CPU capacity.
    ... ##<<If \code{ps} is a polygon then additional arguments in
        ##\code{\link{rsp2ebv}}.
)

{
    if(is.logical(ps))
        return(ps)

    unlink(file.path(tempdir(),'ecochange','change'), recursive = TRUE)

        if(length(eco_range) > 2){
            warning("'eco_range': the vector has length > 2 and only its range will be used")
            eco_range  <- range(eco_range)
    }

    isLayer <- 'lyrs'%in%names(list(...))
    if(isLayer)
        isLayer <- is.null(list(...)$'lyrs')

    if(inherits(ps, getOption('inh'))){
        ps. <- ps
        ps <- rsp2ebv(ps,mc.cores = mc.cores, ...)
        if(is.null(ps.))
            return(ps)
        if(isLayer)
            return(ps)
    }

    if('echanges'%in%class(ps))
    ps <- stack(unclass(ps))
           
    ecopatrn <- gsub("\\d+", "", eco)
    if(!all(grepl(ecopatrn[1L], ecopatrn))){
        stop("Ambiguous layer names: provide arguments 'eco' and 'change'")}

    reg2rst <- function(exp){
        exp. <- paste(exp, collapse = '|')
        exp <- names(ps)[grepl(exp., names(ps))]
        rst <- raster::subset(ps,exp)
        return(rst)}
        eco <- reg2rst(eco)
    change <- reg2rst(change)

    if(dim(change)[3] > 1)
        stop("'change' must be a single layer")

        if(!missing(sp_dist)){
            sp_dist <- reg2rst(sp_dist)}

        if(!missing(sp_dist)){
            ## No missing sp_dist
        marg. <- c(list(FUN = function(x)
         msk_sp_(x, sp_dist,
                tim = sp_dist_range),
            x = raster::as.list(eco)),marg)
            eco <- stack(do.call(getOption('fapp'), marg.))
        }
    if(dim(eco)[3] > 1){
        print("'eco' has length > 1: matching alphanumerics in 'eco' with values in 'change'...")
        change_vals <- nm2yr(eco)
    }
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores}

        if(!spread){
            print("'Fast-computing inputs for landscape areas")
            marg. <- c(list(FUN = function(x,y)
                msk_0_(x, y,
                      perc=eco_range,
                      tim = c(1, max(change_vals))),
                x = raster::as.list(eco),
                y = raster::as.list(change)),
                marg)
            w <- do.call(getOption('fapp'), marg.)
            w <- stack(w)
            return(w)
        }

    marg. <- c(list(FUN = function(x,y)
        msk_2_(x, change,
              remnant = get_unaffected,
              keep = !binary_output,
              perc = eco_range,
              tim = c(0,y),
              noData = noDataValue),
        x = raster::as.list(eco),
        y = change_vals), marg)

    ## w <- stack(do.call(getOption('fapp'), marg.))
    w <- do.call(getOption('fapp'), marg.)
    names(w) <- lapply(w, 'names')
    class(w) <- append('echanges',class(w))

    return(w)
### Class \code{echanges}.
} ,
ex=function() {
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

    ## Method 'plot.echanges' allows comparing rasters using a common scale bar:
    plot.echanges(def)
})
