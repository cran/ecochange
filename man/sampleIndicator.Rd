\name{sampleIndicator}
\alias{sampleIndicator}
\title{Sample Biodiversity indicator }
\description{This function divides into fixed-size grids each of the scenes of
a stack of ecosystem-spatial data and samples a biodiversity
indicator by every grid. To compute biodiversity indicators at the
class and landscape levels, see \code{\link{gaugeIndicator}}}
\usage{sampleIndicator(ps = NULL, 
    ..., metric = "condent", 
    classes = 5, min = 1, 
    max = 100, side, 
    smp_lsm = list(level = "landscape"), 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{ps}{\code{SpatialPolygonsDataFrame} or
\code{RasterStack}. Polygon geometry used to
produce ecosystem-change maps via the
implementation of \code{\link{echanges}} or the
stack of ecosystem-change maps.}
  \item{\dots}{If \code{ps} is a \code{polygon} then additional
arguments in \code{\link{echanges}}  or
\code{\link{rsp2ebv}}.}
  \item{metric}{\code{character}. The name of an indicator
other than ecosystem extent. This can be
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
  \item{side}{\code{numeric}. Side of the sampling grid (\code{m}). If
missing the function tries to find a grid size the samples
at least a grid with a non-NA value of the indicator.}
  \item{smp_lsm}{\code{List}. Additional
arguments in
\code{\link{sample_lsm}}}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses 60 percent of the
cores.}
}

\value{Class \code{echanges}}
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
\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{

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


plot.echanges(amazon)

## Function 'sampleIndicator' is implemented to sample a metric of
## conditional entropy (default):

    def_condent <- sampleIndicator(def, side = 400, mc.cores = 2)

plot.echanges(def_condent, cex = 1.5)

}
