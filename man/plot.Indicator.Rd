\name{plot.Indicator}
\alias{plot.Indicator}
\title{Visualize Indicator objects}
\description{Plots for objects from \code{\link{gaugeIndicator}} are produced.}
\usage{\method{plot}{Indicator}(x, y, 
    ...)}
\arguments{
  \item{x}{\code{\link{tibble}}. Data set of indicators such as that
produced by \code{\link{gaugeIndicator}}.}
  \item{y}{\code{\link{character}}. Color scale. If missing then
\code{grDevices::terrain.colors(n)}, where \code{n} is the
number of colors, is implemented.}
  \item{\dots}{Arguments to be passed to plot methods.}
}



\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
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
               main = 'Cartagena del Chairá',
               sub = 'Ecosystem changes',
               fill = 'Forest cover (\%)')

}
