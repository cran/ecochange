\name{plot.Indicator}
\alias{plot.Indicator}
\title{Visualize Indicator objects}
\description{Plots for objects from \code{\link{gaugeIndicator}} are produced.}
\usage{\method{plot}{Indicator}(x, y, 
    ...)}
\arguments{
  \item{x}{\code{\link{tibble}}. Data set of indicators such as that
produced by \code{\link{gaugeIndicator}}.}
  \item{y}{\code{character}. A color palette. If this is missing or
the suggest \code{viridis} is not installed then
\code{\link{terrain.colors}} is implemented.}
  \item{\dots}{Graphical arguments: \itemize{\item{\code{type}: what type
of plot should be drawn: \code{"s"} for stacked bar plots
(default), or \code{"b"} for box plots},\item{\code{cex}:
adjustment of sizes for most text values},
\item{\code{xlab}, and \code{ylab}: titles for the \code{x}
and \code{y} axes},\item{\code{main}: a text of the main
title}, \item{\code{sub}: a text for the sub title},
\item{\code{labels}: a string or numeric sequence for the
x-axis labels}, \item{\code{fill}: a text for the legend
title}}}
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
               main = 'Ecosystem changes',
               sub = 'Northern amazon',
               fill = 'Forest cover (\%)')

}
