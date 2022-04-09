plot.EBVstats <- structure(function#Visualize EBVstats objects
###Plots for objects from \code{\link{EBVstats}} are printed.
                            ## details<<
(
    x, ##<<\code{\link{tibble}}. Data set of statistics such as that
       ##produced by \code{\link{EBVstats}}.
    y, ##<<\code{\link{character}}. Color scale. If missing then
       ##\code{grDevices::terrain.colors(n)}, where \code{n} is the
       ##number of layers, is implemented.
   ... ##<<Arguments to be passed to some plot methods.  
){

data <- x
xx <- factor(rownames(data), levels = rownames(data))
fill. <- factor(data$'layer', levels = rev(levels(factor(data$'layer'))))
ang. <- 0

if(length(unique(data$'layer')) != 1){
    xx <- factor(data$'layer', levels = unique(data$'layer'))
    ang. <- 90}
fill. <- xx
fill.. <- length(xx)
## if(any(!is.na(data$'class'))){
##     fill. <- factor(
##         data$'class', levels = rev(levels(factor(data$'class'))))
##     if(length(unique(data$'layer')) == 1)
##         xx <- factor(data$'class', levels = unique(data$'class'))
## fill.. <- max(as.numeric(as.character(fill.)))}
ls2pl <- list()


ls2pl$'p' <- ggplot2::ggplot(data = data,
                     aes(x = xx,
                         y = .data$mean, fill = fill.))
ls2pl$'q' <- ggplot2::geom_bar(stat = "identity",
                       position = "stack")#,

ls2pl$'m' <- geom_errorbar(aes(ymin=.data$mean-.data$sd,
                               ymax=.data$mean+.data$sd), width=.2,
                 position=position_dodge(.9))  

ell <- list(...)
typl <- any(grepl('type', names(ell))) 
if(typl){
    if(!ell$'type'%in%'p')
        cat("EBVstats: argument 'type' is not supported\n")}
    ## fill.. <- length(levels(xx))}
dep <- 'viridis'
if(missing(y)){
    y <- terrain.colors(fill..)
    if(requireNamespace(dep, quietly = TRUE)&dep%in% (.packages()))
        ## y <- viridis(fill..)}
        y <- do.call(dep,list(n = fill..))}
ls2pl$'cl'  <-  scale_fill_manual(values = y)
## if(typl)
##     if(ell$'type'%in%'b'){
##         ls2pl$'p' <- ggplot2::ggplot(data = data,
##                                      aes(x = .data$layer,
##                                          y = .data$value,
##                                          fill = .data$layer))
##         ls2pl$'q' <- geom_boxplot()
## ls2pl$'cl'  <-  scale_fill_manual(values = y)}
xyl <- c(x = 'xlab', y = 'ylab', title = 'main', subtitle = 'sub')
xyl.. <- paste0('^', xyl,'$')
inl <- grepl(paste(xyl.., collapse = '|'), names(ell))
names(ell)[inl] <- names(xyl)[xyl%in%names(ell)[inl]] 
lst <- list(x = 'Layer', y = 'Value', fill = 'class')
labs  <-  modifyList(lst, ell)
ls2pl$'r' <- do.call('labs', labs)

cex <- 1
if('cex'%in%names(ell))
    cex <- ell$'cex'
width_scale = 12
ls2pl$'th' <- ggplot2::theme(legend.position="right",
                     aspect.ratio = 1/1, text = element_text(size = cex*width_scale),
                     axis.text.x = element_text(angle = ang., vjust = 0.5, hjust=1),
                     legend.key.size = grid::unit(width_scale/50, "inch"),
legend.box.margin = margin(rep(width_scale/2,4)),
)

Reduce('+', ls2pl)
    
} , ex=function(){
    ## RasterBrick of structural Essential Biodiversity Variables
    ## covering the extent of a location in the northern Amazon basin
    ## (Colombia) is imported:
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- brick(path.)
    
    ## Changes in layers of tree-canopy cover (TC) are computed by
    ## processing the 'amazon' brick:
    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)
    
    ## Function 'EBVstats' is used to compute ecosystem statistics
    st_amazon <- EBVstats(def)

    ## A plot of the 'st_amazon' object
    plot.EBVstats(st_amazon,
                   cex = 1.5,
                   xlab = 'Year',
                   ylab = 'Canopy cover (%)',
                   main = 'Ecosystem changes',
                   sub = 'Municipality: Cartagena del Chaira',
                   fill = 'Layer')

})
