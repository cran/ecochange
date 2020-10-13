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

## Deforestation Statistics:

defstats <- EBVstats(def)

## Barplot:

barplot(defstats)
}
