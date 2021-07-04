\name{gaugeIndicator}
\alias{gaugeIndicator}
\title{Gauge Indicator}
\description{This function processes either regions of interest (polygons) or
stacks of essential biodiversity variables (\code{ebv} stacks) to
calculate indicators related to horizontal ecosystem extent,
degradation, fragmentation, among others. To sample the indicators
in fixed-size grids across \code{ebv} stacks see
\code{\link{sampleIndicator}}.}
\usage{gaugeIndicator(pol, ..., 
    met = "area_ha", 
    smp_lsm = list(level = "landscape"), 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{pol}{\code{SpatialPolygonsDataFrame} or \code{Raster*}. Region
of interest or Stack of essential biodiversity variables.}
  \item{\dots}{If \code{pol} is not a \code{Raster*} then additional
arguments in \code{\link{echanges}}.}
  \item{met}{\code{character}. An \code{ebv}
metric. Default \code{'area_ha'} computes
ecosystem areas (ha) at class level. See
argument 'metric' in \code{\link{list_lsm}}
to compute other metrics.
spread = TRUE, ##<<\code{logical}. Try to dimension the ecosystem
##variable across space and along time.}
  \item{smp_lsm}{\code{list}. If
\code{met} is not
\code{'area_ha'} then
additional arguments in
\code{\link{calculate_lsm}}. }
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses 60 percent of the
cores.}
}
\details{Coordinate reference system of the
spatial units must have metric units \code{
UTM}. Indicators other than ecosystem areas
are calculated implementing
\code{\link{calculate_lsm}}.
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
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzales [aut]}




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
