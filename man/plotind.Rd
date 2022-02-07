\name{plotind}
\alias{plotind}
\title{Plot indicator}
\description{A plot of \code{\link{gaugeIndicator}} is printed.}
\usage{plotind(x, labs = list(x = "layer", 
    y = unique(as.character(x$metric)), 
    fill = "class"))}
\arguments{
  \item{x}{\code{\link{tibble}}. Data set of indicators such as that
produced by \code{\link{gaugeIndicator}}.}
  \item{labs}{Further
arguments
in
\code{\link{labs}}.}
}



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

## Function 'gaugeIndicator' is used to compute ecosystem areas
## (default metric of the function):
am_areas <- gaugeIndicator(def,
                         mc.cores = 2)

## Plot of the output from 'gaugeIndicator'
plotind(am_areas)

}
