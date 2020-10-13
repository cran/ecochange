\name{plotebv}
\alias{plotebv}
\title{Plot EBV}
\description{This function prints levelplots of Essential Biodiverstiy
Variables using a common spatial scale bar. The function is helpful
to compare EBV indicators.}
\usage{plotebv(ebv, ...)}
\arguments{
  \item{ebv}{\code{Raster*}. Raster Object.}
  \item{\dots}{
}
}

\value{\code{levelplot}.}

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

## Plot:
suppressWarnings(
    plotebv(def)
)
}
