\name{plot.EBVstats}
\alias{plot.EBVstats}
\title{Visualize EBVstats objects}
\description{Plots for objects from \code{\link{EBVstats}} are printed.}
\usage{\method{plot}{EBVstats}(x, y, ...)}
\arguments{
  \item{x}{\code{\link{tibble}}. Data set of statistics such as that
produced by \code{\link{EBVstats}}.}
  \item{y}{\code{\link{character}}. Color scale. If missing then
\code{grDevices::terrain.colors(n)}, where \code{n} is the
number of layers, is implemented.}
  \item{\dots}{Arguments to be passed to some plot methods.}
}



\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan cre Gonzalez [ctb], Maria C. Londono [ctb]}




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

## Function 'EBVstats' is used to compute ecosystem statistics
st_amazon <- EBVstats(def)

## A plot of the 'st_amazon' object
plot.EBVstats(st_amazon,
               cex = 1.5,
               xlab = 'Year',
               ylab = 'Canopy cover (\%)',
               main = 'Ecosystem changes',
               sub = 'Municipality: Cartagena del Chaira',
               fill = 'Layer')

}
