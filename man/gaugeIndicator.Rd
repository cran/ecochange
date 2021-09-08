\name{gaugeIndicator}
\alias{gaugeIndicator}
\title{Gauge Biodiversity Indicator}
\description{This function processes ecosystem-change maps from
\code{\link{echanges}} to calculate biodiversity indicators,
including ecosystem extent, entropy, fractal dimension, among
others. To sample the indicators across fixed-size grids see
\code{\link{sampleIndicator}}.}
\usage{gaugeIndicator(ps, ..., 
    metric = "area_ha", 
    smp_lsm = list(), 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{ps}{\code{SpatialPolygonsDataFrame} or
\code{RasterStack}. Polygon geometry used to produce
ecosystem-change maps via the implementation of
\code{\link{echanges}} or the stack of ecosystem-change
maps.}
  \item{\dots}{If \code{ps} is a \code{polygon} then additional
arguments in \code{\link{echanges}} or
\code{\link{rsp2ebv}}.}
  \item{metric}{\code{character}. The name of an
indicator. Default \code{'area_ha'} computes
ecosystem areas (ha) at class level. See the
argument \code{'metric'} in
\code{\link{list_lsm}} to implement other
metrics.}
  \item{smp_lsm}{\code{list}. List of arguments in
\code{\link{calculate_lsm}}. This argument is
ignored when \code{metric = 'area_ha'}.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent of
the cores.}
}
\details{Coordinate reference system of the
spatial units must have \code{metric} units \code{
UTM}. Performance in the computation of
ecosystem extents is optimized via the
implementation of the function
\code{\link{tabuleRaster}}.  Indicators other
than ecosystem extents are calculated
implementing \code{\link{calculate_lsm}}.
}
\value{\code{tibble}.}
\references{{Hesselbarth, M. H., Sciaini, M.,
With, K. A., Wiegand, K., & Nowosad,
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
\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [aut], Maria C. Londono [aut]}




\examples{
## Warnings from GDAL/PROJ are suppressed.

## RasterBrick of structural Essential Biodiversity Variables
## covering the extent of a location in the northern Amazon basin
## (Colombia) is imported:
path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- suppressWarnings(brick(path.))

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

## Function 'gaugeIndicator' is used to compute ecosystem areas
## (default):
am_areas <- gaugeIndicator(def,
                           mc.cores = 2)


}
