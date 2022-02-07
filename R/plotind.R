plotind <- structure(function#Plot indicator
###A plot of \code{\link{gaugeIndicator}} is printed.
(
    x, ##<<\code{\link{tibble}}. Data set of indicators such as that
       ##produced by \code{\link{gaugeIndicator}}.
    labs = list(x = 'layer', y = unique(as.character(x$'metric')), fill = 'class') ##<<Further
                                                                                   ##arguments
                                                                                   ##in
                                                                                   ##\code{\link{labs}}.
){
    data <- x
    
    if(is.logical(data)){
        return(plot(data))}

    xx <- factor(rownames(data), levels = rownames(data))
    fill. <- fct_rev(factor(data$'layer'))
    ang. <- 0
    if(length(unique(data$'layer')) != 1){
        xx <- factor(data$'layer', levels = unique(data$'layer'))
    ang. <- 90}

    if(any(!is.na(data$'class'))){
        fill. <- fct_rev(factor(data$'class'))
        if(length(unique(data$'layer')) == 1)
        xx <- factor(data$'class', levels = unique(data$'class'))
    }

    p <- ggplot2::ggplot(data = data,
                         aes(x = xx,
                             y = .data$value, fill = fill.))

    q <- ggplot2::geom_bar(stat = "identity", position = "stack", color = 'black')

     if (requireNamespace("viridis", quietly = TRUE)) {
         
         p + q +
             do.call('labs', labs) +
             ggplot2::theme(legend.position="right",
                            aspect.ratio = 1/1, text = element_text(size = 14),
                            axis.text.x = element_text(angle = ang., vjust = 0.5, hjust=1)) +
             viridis::scale_fill_viridis(discrete = TRUE)
     } else {
         p + q +
             do.call('labs', labs) +
             ggplot2::theme(legend.position="right",
                            aspect.ratio = 1/1, text = element_text(size = 14),
                            axis.text.x = element_text(angle = ang., vjust = 0.5, hjust=1)) 
         
     }
    
} , ex=function(){
    ## RasterBrick of structural Essential Biodiversity Variables
    ## covering the extent of a location in the northern Amazon basin
    ## (Colombia) is imported:
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
    
    ## Function 'gaugeIndicator' is used to compute ecosystem areas
    ## (default metric of the function):
    am_areas <- gaugeIndicator(def,
                             mc.cores = 2)

    ## Plot of the output from 'gaugeIndicator'
    plotind(am_areas)

})
