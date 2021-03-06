\name{EBVstats}
\alias{EBVstats}
\title{EBV Stats}
\description{This function is a wrapper of \code{cellStats} to compute 
statistics of essential biodiversity variables.}
\usage{EBVstats(ccp, stats, 
    ...)}
\arguments{
  \item{ccp}{\code{RasterStack} or \code{NULL}. If \code{NULL} then
\code{NULL} is returned.}
  \item{stats}{\code{character}. vector of \code{stats} defined in
\code{\link{cellStats}}. If missing then six summary
statistics, including \code{'mean'}, \code{'sd'},
\code{'min'}, \code{'max'}, are computed.}
  \item{\dots}{Additional arguments in \code{cellStats}}
}

\value{\code{list} of \code{EBVstats}.}

\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzales [aut]}




\examples{
## Warnings from GDAL/PROJ are suppressed.

## Brick with structural Essential Biodiversity Variables covering the
## extent of a location in the northern Amazon basin (Colombia):

path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- suppressWarnings(brick(path.))

## Tree-cover layers in the 'amazon' brick are both formatted and
## deforested:

\donttest{
suppressWarnings(
def <- echanges(amazon, eco = 'TC',
                change = 'lossyear',
                eco_range = c(1,80),
                get_unaffected = TRUE,
                binary_output = FALSE,
                mc.cores = 2)
)

## Deforestation Statistics:

defstats <- suppressWarnings(EBVstats(def))

## barplot method:

barplot(defstats)
}
}