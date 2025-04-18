\name{EBVstats}
\alias{EBVstats}
\title{EBV Stats}
\description{This function is a wrapper of \code{\link[raster]{cellStats}} used to compute
statistics for spatial indicators in the EBV class ecosystem
structure. To derive the spatial indicators see functions
\code{\link{echanges}} and \code{\link{sampleIndicator}} }
\usage{EBVstats(ccp, stats, 
    ...)}
\arguments{
  \item{ccp}{\code{echanges}, or \code{RasterStack} or
\code{NULL}. If \code{NULL} then \code{NULL} is returned.}
  \item{stats}{\code{character}. vector of \code{stats} defined in
\code{\link[raster]{cellStats}}. If missing then six summary
statistics, including \code{'mean'}, \code{'sd'},
\code{'min'}, \code{'max'}, are computed.}
  \item{\dots}{Additional arguments in \code{\link[raster]{cellStats}}.}
}

\value{\code{tibble}.}

\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




\examples{
## RasterBrick of structural Essential Biodiversity Variables
## covering the extent of a location in the northern Amazon basin
## (Colombia) is imported:
path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- brick(path.)

## Changes in layers of tree-canopy cover (TC) are computed by
## processing the 'amazon' brick:
def <- echanges(amazon, eco = 'TC',
                change = 'lossyear',
                eco_range = c(1,80),
                get_unaffected = TRUE,
                binary_output = FALSE,
                mc.cores = 2)

## Function 'EBVstats' is used to compute ecosystem statistics
st_amazon <- EBVstats(def)

## A plot of the 'st_amazon' object
plot.EBVstats(st_amazon,
               cex = 1.5,
               xlab = 'Year',
               ylab = 'Canopy cover (\%)',
               main = 'Ecosystem changes',
               sub = 'Northern Amazon',
               fill = 'Layer')
}
