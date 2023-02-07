plot.echanges <- structure(function #Visualize ecosystem changes
### This function can display level and box plots for objects from
### \code{\link{rsp2ebv}}, \code{\link{echanges}}, or
### \code{\link{sampleIndicator}}.
(
    x, ##<<\code{Raster*}, or \code{echanges}. RasterStack object or
       ##ecoystem-change representation.
    y, ##<<\code{character}. A color palette. If this is missing or
       ##the suggest \code{viridis} is not installed then
       ##\code{\link{terrain.colors}} is implemented.
    ... ##<<Graphical arguments: \itemize{\item{\code{type}: what type
        ##of plot should be drawn: \code{"p"} for level plots
        ##(default), or \code{"b"} for box
        ##plots},\item{\code{cex}: adjustment of sizes for
        ##most text values. If missing then \code{cex = 1}; if a main
        ##title is specified then it is increased \code{1.4*cex}},
        ##\item{\code{xlab}, and \code{ylab}: titles for the \code{x}
        ##and \code{y} axes},\item{\code{main}: a text of the main
        ##title}, \item{\code{labels}: a string or numeric sequence
        ##for the panel titles}}
) {

    ## if('echanges'%in%class(x)){
    ##     ebv <- stack(unclass(x))
    ## }else{ebv <- x}

    type <- 'p'
    indots <- list(...)

    if('type'%in%names(indots))
        type  <- indots$'type'

        
    if(!type%in%'b'){
    if('echanges'%in%class(x)){
        ebv <- stack(unclass(x))
    }else{ebv <- x}
    
    mx.. <- max(mapply(function(x)
        ## x@data@max, raster::as.list(ebv)))
        max(x[], na.rm = TRUE), raster::as.list(ebv)))
    
    if(is.logical(ebv))
        return(plot(ebv))
        dep <- 'viridis'
        if(missing(y)){
            y  <-  rev(terrain.colors(mx..))
            if(requireNamespace(dep, quietly = TRUE)&
               dep%in% (.packages())){
                y <- do.call('viridis_pal', list(direction = -1))
            }
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


            names.attr <- names(ebv)
        if('labels'%in%names(indots)){
            names.attr <- indots$'labels'
            if(all(is.numeric(names.attr)))
                names.attr <- paste0('', names.attr)}


        p.strip <- list(col = 'black', font = 1, cex = cex)

        main.tit <- list(label = main, cex = 1.4*cex)
        xlab.tit <- list(label = xlab, cex = cex)
        ylab.tit <- list(label = ylab, cex = cex)
        dep1 <- 'rasterVis'
        if(requireNamespace(
            dep1, quietly = TRUE)&dep1%in% (.packages())){
            plt <- rasterVis::levelplot(ebv,
                                        margin = list(x = TRUE,
                                                      y = TRUE),
                                        col.regions = y,
                                        font = 1,
                                        pretty = T,#)
                                        xlab = xlab.tit,
                                        ylab = ylab.tit,
main = main.tit,
                                        names.attr = names.attr,
                                        scales = list(cex = c(cex,cex),                                                      xlab = list(cex = cex),
                                                      ylab = list(cex = cex)),
                                        par.strip.text = p.strip,
                                        panel = function(x, y, ...){
                                            panel.grid(v = -1, h = -1,
                                                       col = 'grey95',
                                                       lty = 1)
                                            panel.levelplot(x, y, ...)})
        }else{
            plt <- raster::plot(ebv, col = y)
        }
    return(plt)
    } else {
        if(!'echanges'%in%class(x)){
            ebv <- raster::as.list(x)
            names(ebv) <- sapply(ebv, names)
        }else{
        ebv <- unclass(x)}
        maxamp <- 1E5
        if('maxamp'%in%names(indots))
            maxamp <- indots$'maxamp'
        sums <- data.frame(t(mapply(function(m)
            raster::summary(m, maxamp = maxamp), ebv)))
        nms <- names(ebv)
        fill.. <- nrow(sums)
        tofac <- function(x, rev = TRUE){
    tf <- factor(x, levels = unique(x))
    if(rev)
    tf <- factor(x, levels = rev(levels(factor(x))))
    return(tf)}
        xx <- tofac(nms, rev = FALSE)
        ls2pl <- list()
        ls2pl$'p' <- ggplot2::ggplot(sums,           
                                     aes(x = xx,
                                         ymin = .data$X1,
                                         lower = .data$X2,
                                         middle = .data$X3,
                                         upper = .data$X4,
                                         ymax = .data$X5,
                                         fill = xx))
        ls2pl$'q' <- geom_boxplot(stat = "identity")
        ang. <- 0
if(max(nchar(levels(xx))) > 2){
    ang. <- 90
}
dep <- 'viridis'
if(missing(y)){
    y <- terrain.colors(fill..)
    if(requireNamespace(dep, quietly = TRUE)&dep%in% (.packages()))
        y <- do.call(dep,list(n = fill..))
}
ls2pl$'cl'  <-  scale_fill_manual(values = y)
xyl <- c(x = 'xlab', y = 'ylab', title = 'main', subtitle = 'sub')
xyl.. <- paste0('^', xyl,'$')
inl <- grepl(paste(xyl.., collapse = '|'), names(indots))
names(indots)[inl] <- names(xyl)[xyl%in%names(indots)[inl]] 
lst <- list(x = 'Layer', y = 'Value', fill = 'Layer')
        labs  <-  modifyList(lst, indots)
ls2pl$'r' <- do.call('labs', labs)
if('labels'%in%names(indots)){
    if(max(nchar(indots$'labels')) <= 2)
        ang. <- 0
    ls2pl$'e' <- scale_x_discrete(breaks = xx,
                                  labels = indots$'labels')} 
cex <- 1
if('cex'%in%names(indots))
    cex <- indots$'cex'
width_scale = 12
ls2pl$'th' <- ggplot2::theme(legend.position="right",
                             aspect.ratio = 1/1,
                             text = element_text(
                                 size = cex*width_scale),
                             axis.text.x = element_text(
                                 angle = ang.,
                                 vjust = 0.5, hjust=1),
                             legend.key.size = grid::unit(
                                                         width_scale/50, "inch"),
                             legend.box.margin = margin(
                                 rep(width_scale/2,4)),
)
        Reduce('+', ls2pl)}
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
