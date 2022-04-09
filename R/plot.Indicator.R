plot.Indicator <- structure(function#Visualize Indicator objects
###Plots for objects from \code{\link{gaugeIndicator}} are produced.
                            ## details<<
(
    x, ##<<\code{\link{tibble}}. Data set of indicators such as that
       ##produced by \code{\link{gaugeIndicator}}.
    y, ##<<\code{\link{character}}. Color scale. If missing then
       ##\code{grDevices::terrain.colors(n)}, where \code{n} is the
       ##number of colors, is implemented.
   ... ##<<Arguments to be passed to plot methods.  
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
if(any(!is.na(data$'class'))){
    fill. <- factor(
        data$'class', levels = rev(levels(factor(data$'class'))))
    if(length(unique(data$'layer')) == 1)
        xx <- factor(data$'class', levels = unique(data$'class'))
fill.. <- max(as.numeric(as.character(fill.)))}
ls2pl <- list()
ls2pl$'p' <- ggplot2::ggplot(data = data,
                     aes(x = xx,
                         y = .data$value, fill = fill.))
ls2pl$'q' <- ggplot2::geom_bar(stat = "identity",
                       position = "stack")#,
ell <- list(...)
typl <- any(grepl('type', names(ell))) 
if(typl){
    if(ell$'type'%in%'b')
    fill.. <- length(levels(xx))}
dep <- 'viridis'
if(missing(y)){
    y <- terrain.colors(fill..)
    if(requireNamespace(dep, quietly = TRUE)&dep%in% (.packages()))
        ## y <- viridis(fill..)}
        y <- do.call(dep,list(n = fill..))}
ls2pl$'cl'  <-  scale_fill_manual(values = y)
if(typl)
    if(ell$'type'%in%'b'){
                data$'layer' <- factor(data$'layer', levels = unique(data$'layer'))

        ls2pl$'p' <- ggplot2::ggplot(data = data,
                                     aes(x = .data$layer,
                                         y = .data$value,
                                         fill = .data$layer))
        ls2pl$'q' <- geom_boxplot()
ls2pl$'cl'  <-  scale_fill_manual(values = y)}
xyl <- c(x = 'xlab', y = 'ylab', title = 'main', subtitle = 'sub')
xyl.. <- paste0('^', xyl,'$')
inl <- grepl(paste(xyl.., collapse = '|'), names(ell))
names(ell)[inl] <- names(xyl)[xyl%in%names(ell)[inl]] 
lst <- list(x = 'Layer', y = unique(as.character(x$'metric')), fill = 'class')
labs  <-  modifyList(lst, ell)
ls2pl$'r' <- do.call('labs', labs)
## ls2pl$'l' <- do.call('labs', labs)

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
    
    ## Function 'gaugeIndicator' is used to compute ecosystem areas
    ## (default metric = 'area_ha'):
    am_areas <- gaugeIndicator(def,
                             mc.cores = 2)

    ## A plot of the 'am_areas' object
    plot.Indicator(am_areas,
                   cex = 1.5,
                   xlab = 'Year',
                   ylab = 'Area (ha)',
                   main = 'Ecosystem changes',
                   sub = 'Municipality: Cartagena del Chaira',
                   fill = 'Forest cover (%)')

})
