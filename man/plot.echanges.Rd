\name{plot.echanges}
\alias{plot.echanges}
\title{Visualize ecosystem changes}
\description{This function aims to display level plots for remote sensing
products using common scale-bars via the implementation of the
suggested \code{rasterVis}. If this is not installed then method
\code{plot} of \code{raster} package is used.}
\usage{\method{plot}{echanges}(x, y, ...)}
\arguments{
  \item{x}{\code{Raster*}. Raster Object.}
  \item{y}{. Color palette. If missing or the suggest
'viridis' is not installed then
\code{\link{terrain.colors}} is used.}
  \item{\dots}{. Additional arguments in \code{panel.levelplot}.}
}

\value{\code{levelplot}.}

\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
## Brick with structural Essential Biodiversity Variables covering the
## extent of a location in the northern Amazon basin (Colombia):
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

plot.echanges(def)
}
