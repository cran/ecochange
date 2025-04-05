\name{plot.echanges}
\alias{plot.echanges}
\title{Visualize ecosystem changes}
\description{This function can display level and box plots for objects from
\code{\link{rsp2ebv}}, \code{\link{echanges}}, or
\code{\link{sampleIndicator}}.}
\usage{\method{plot}{echanges}(x, y, ...)}
\arguments{
  \item{x}{\code{Raster*}, or \code{echanges}. RasterStack object or
ecoystem-change representation.}
  \item{y}{\code{character}. A color palette. If this is missing or
the suggest \code{viridis} is not installed then
\code{\link{terrain.colors}} is implemented.}
  \item{\dots}{Graphical arguments: \itemize{\item{\code{type}: what type
of plot should be drawn: \code{"p"} for level plots
(default), or \code{"b"} for box
plots},\item{\code{cex}: adjustment of sizes for
most text values. If missing then \code{cex = 1}; if a main
title is specified then it is increased \code{1.4*cex}},
\item{\code{xlab}, and \code{ylab}: titles for the \code{x}
and \code{y} axes},\item{\code{main}: a text of the main
title}, \item{\code{labels}: a string or numeric sequence
for the panel titles}}}
}



\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




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
