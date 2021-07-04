\name{barplot.EBVstats}
\alias{barplot.EBVstats}
\title{barplot EBV Stats}
\description{A barplot of \code{\link{EBVstats}} is printed.}
\usage{\method{barplot}{EBVstats}(height, 
    ...)}
\arguments{
  \item{height}{\code{tibble} of \code{EBVstats}.}
  \item{\dots}{Additional arguments in \code{\link{barplot}}.}
}

\value{Plot of \code{EBVstats}.}

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
