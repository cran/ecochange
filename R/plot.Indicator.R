plot.Indicator <- structure(function#Visualize Indicator objects
###Plots for objects from \code{\link{gaugeIndicator}} are produced.
(
    x, ##<<\code{\link{tibble}}. Data set of indicators such as that
       ##produced by \code{\link{gaugeIndicator}}.
    y, ##<<\code{character}. A color palette. If this is missing or
       ##the suggest \code{viridis} is not installed then
       ##\code{\link{terrain.colors}} is implemented.
    ... ##<<Graphical arguments: \itemize{\item{\code{type}: what type
        ##of plot should be drawn: \code{"s"} for stacked bar plots
        ##(default), or \code{"b"} for box plots},\item{\code{cex}:
        ##adjustment of sizes for most text values},
        ##\item{\code{xlab}, and \code{ylab}: titles for the \code{x}
        ##and \code{y} axes},\item{\code{main}: a text of the main
        ##title}, \item{\code{sub}: a text for the sub title},
        ##\item{\code{labels}: a string or numeric sequence for the
        ##x-axis labels}, \item{\code{fill}: a text for the legend
        ##title}}
){

    data <- x
    ell <- list(...)

isclass <- any(!is.na(data$'class'))
isbox <- FALSE
if(any(grepl('type', names(ell)))){
    isbox <- ell$'type'%in%'b'}
tofac <- function(x, rev = TRUE){
    tf <- factor(x, levels = unique(x))
    if(rev)
    tf <- factor(x, levels = rev(levels(factor(x))))
    return(tf)}
xx <- tofac(data$'layer', rev = FALSE)
fill. <- xx
if(isclass){
    fill. <- tofac(data$'class')}
if(isbox){
    fill. <- tofac(data$'layer', FALSE)}
ang. <- 0
if(max(nchar(levels(xx))) > 2){
    ang. <- 90
}
fill.. <- length(levels(fill.))
dep <- 'viridis'
if(missing(y)){
    y <- terrain.colors(fill..)
    if(requireNamespace(dep, quietly = TRUE)&dep%in% (.packages()))
        y <- do.call(dep,list(n = fill..))
}
ls2pl <- list()
ls2pl$'p' <- ggplot2::ggplot(data = data,
                     aes(x = xx,
                         y = .data$value, fill = fill.))
ls2pl$'q' <- ggplot2::geom_bar(stat = "identity",
                               position = "stack")
if(isbox){
    ls2pl$'q' <- geom_boxplot()}
ls2pl$'cl'  <-  scale_fill_manual(values = y)
xyl <- c(x = 'xlab', y = 'ylab', title = 'main', subtitle = 'sub')
xyl.. <- paste0('^', xyl,'$')
inl <- grepl(paste(xyl.., collapse = '|'), names(ell))
names(ell)[inl] <- names(xyl)[xyl%in%names(ell)[inl]] 
lst <- list(x = 'Layer', y = unique(as.character(data$'metric')), fill = 'class')
labs  <-  modifyList(lst, ell)
ls2pl$'r' <- do.call('labs', labs)
if('labels'%in%names(ell)){
    if(max(nchar(ell$'labels')) <= 2)
        ang. <- 0
    ls2pl$'l' <- scale_x_discrete(breaks = levels(xx),
                                  labels = ell$'labels')} 
cex <- 1
if('cex'%in%names(ell))
    cex <- ell$'cex'
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
                   sub = 'Northern amazon',
                   fill = 'Forest cover (%)')

})
