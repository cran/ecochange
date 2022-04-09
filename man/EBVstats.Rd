\name{EBVstats}
\alias{EBVstats}
\title{EBV Stats}
\description{This function is a wrapper of \code{cellStats} used to compute
statistics for spatial indicators in the EBV class ecosystem
structure. To derive the spatial indicators see functions
\code{\link{echanges}} and \code{\link{sampleIndicator}} }
\usage{EBVstats(ccp, stats, 
    ...)}
\arguments{
  \item{ccp}{\code{echanges}, or \code{RasterStack} or
\code{NULL}. If \code{NULL} then \code{NULL} is returned.}
  \item{stats}{\code{character}. vector of \code{stats} defined in
\code{\link{cellStats}}. If missing then six summary
statistics, including \code{'mean'}, \code{'sd'},
\code{'min'}, \code{'max'}, are computed.}
  \item{\dots}{Additional arguments in \code{cellStats}}
}

\value{\code{tibble}.}

\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




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
