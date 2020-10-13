\name{sampleIndicator}
\alias{sampleIndicator}
\title{Sample Indicator }
\description{This function can divide Essential Biodiversity Variables into
fixed-size grids and calculate biodiversity indicators in the
grids. To compute indicators avoiding the grid sampling
procedure see \code{\link{gaugeIndicator}}}
\usage{sampleIndicator(roi = NULL, 
    ..., ind = "condent", 
    min = 1, max = 100, 
    classes = 5, side, 
    perc. = 15, mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{roi}{\code{Raster*}; or
\code{SpatialPolygonsDataFrame}; or
\code{character}; or \code{NULL}. Raster object such as
these produced by \code{\link{rsp2ebv}} and
\code{\link{deforest}}; or region of interest
(\code{roi}). The \code{roi} can be whether 1) a
polygon geometry; or 2) the name of a
\code{GADM} unit (see \code{\link{getGADM}}); or
3) a \code{NULL} value. Default \code{NULL}
makes the function to print a list of
\code{GADM} units.}
  \item{\dots}{If \code{roi} is not a \code{Raster*} then additional
arguments in \code{\link{rsp2ebv}} can be specified here.}
  \item{ind}{\code{character}. Indicator. This can be
cohesion (\code{'cohesion'}), conditional
entropy (\code{'condent'}), perimeter-area
fractal dimension (\code{'condent'}), among
other, see package
\code{\link{landscapemetrics}}. Default
computes conditional entropy \code{'condent'}.}
  \item{min}{\code{numeric}. Minimum cell value in the
layers. This value is used to subset the data before it
is reclassified, see argument \code{'classes'}
below. Default \code{1}}
  \item{max}{\code{numeric}. Maximum cell value in the
layers. This value is used to subset the data before it
is reclassified, see argument \code{'classes'}
below. Default \code{100}}
  \item{classes}{\code{numeric}; or \code{NULL}. Number of \code{classes}
between \code{1-30} used to reclassify the
layers. Default \code{5}. If \code{NULL} then the
layers are not reclassified.}
  \item{side}{\code{numeric}. The \code{side} of the sampling grid
(\code{m}). If this is not specified, the function tries
to find the maximum \code{side} length that allows splitting the
layer extents into \code{n} suitable grids.}
  \item{perc.}{\code{numeric}. Minimum percentage of features per
grid. Grids with lower percentages than this value are set
to \code{NA}.}
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
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut]}




\examples{
## Warnings from GDAL/PROJ are suppressed.

## Brick with structural Essential Biodiversity Variables covering the
## extent of a location in the northern Amazon basin (Colombia):

path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- suppressWarnings(brick(path.))

## Tree-cover layers in the 'amazon' brick are both formatted and
## deforested:

suppressWarnings(
    def <- deforest(amazon, names(amazon)[grepl('TC', names(amazon))],
                    ebv.vals = 0:100,
                    remnant.areas = TRUE, keep.ebv = TRUE, mc.cores = 2)
)

## Conditional entropy is sampled along the deforested layers using
## cell sides of 300m:
suppressWarnings(
condent <- sampleIndicator(def, side = 300, mc.cores = 2)
)
suppressWarnings(
    plotebv(condent)
    )
}
