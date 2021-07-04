\name{sampleIndicator}
\alias{sampleIndicator}
\title{Sample Indicator }
\description{This function can sample biodiversity indicators in equally spaced
grids disctributed across Earth Observation Variables. To compute
indicators avoiding the grid sampling procedure see
\code{\link{gaugeIndicator}}}
\usage{sampleIndicator(roi = NULL, 
    ..., metric = "condent", 
    classes = 5, min = 1, 
    max = 100, side, 
    smp_lsm = list(level = "landscape"), 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{roi}{\code{Raster*}; or
\code{SpatialPolygonsDataFrame}; or
\code{character}; or \code{NULL}. Raster object
such as these produced by
\code{\link{echanges}}; or region of interest
(\code{roi}). The \code{roi} can be whether 1) a
polygon geometry; or 2) the name of a
\code{GADM} unit (see \code{\link{getGADM}}); or
3) a \code{NULL} value. Default \code{NULL}
makes the function to print a list of
\code{GADM} units.}
  \item{\dots}{If \code{roi} is a \code{polygon} then additional
arguments in \code{\link{echanges}} can be specified here.}
  \item{metric}{\code{character}. Indicator. This can be
cohesion (\code{'cohesion'}), conditional
entropy (\code{'condent'}), perimeter-area
fractal dimension (\code{'pafrac'}), among
others, see package
\code{\link{list_lsm}}. Default
\code{'condent'}.}
  \item{classes}{\code{numeric}; or \code{NULL}. Number of evenly
spaced \code{classes} used to reclassify the
layers. Default \code{5}. If \code{NULL} then the
layers are not reclassified.}
  \item{min}{\code{numeric}. If \code{classes != NULL} then
minimum cell value in the layers. Default \code{1}}
  \item{max}{\code{numeric}. If \code{classes != NULL} then
maximum cell value in the layers. Default \code{100}}
  \item{side}{\code{numeric}. Side for the sampling grids
(\code{m}). If missing the function tries to find the a
minimum number of grids that samples at least a non-NA
indicator.}
  \item{smp_lsm}{\code{List}. Additional
arguments in
\code{\link{sample_lsm}}}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses 60 percent of the
cores.}
}

\value{\code{Raster*}.}
\references{{Hesselbarth, M. H., Sciaini,
M., With, K. A., Wiegand, K., & Nowosad,
J. (2019). landscapemetrics: an open source R
tool to calculate landscape
metrics. Ecography, 42(10), 1648-1657.}

{O'Connor, B., Secades, C., Penner, J.,
Sonnenschein, R., Skidmore, A., Burgess,
N. D., & Hutton, J. M. (2015). Earth
observation as a tool for tracking progress
towards the Aichi Biodiversity Targets. Remote
sensing in ecology and conservation, 1(1),
19-28.}

{Skidmore, A. K., & Pettorelli,
N. (2015). Agree on biodiversity metrics to
track from space: Ecologists and space
agencies must forge a global monitoring
strategy. Nature, 523(7561), 403-406.}}
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzales [aut]}




\examples{

## Warnings from GDAL/PROJ are suppressed.

## RasterBrick of structural Essential Biodiversity Variables
## covering the extent of a location in the northern Amazon basin
## (Colombia) is imported:
path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- suppressWarnings(brick(path.))

\donttest{
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


plotebv(amazon)

## Function 'sampleIndicator' is implemented to sample a metric of
## conditional entropy (default):

suppressWarnings(
    def_condent <- sampleIndicator(def, side = 400, mc.cores = 2)
)

plotebv(def_condent)

}
}