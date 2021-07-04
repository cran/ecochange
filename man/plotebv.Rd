\name{plotebv}
\alias{plotebv}
\title{Plot EBV}
\description{This function displays levelplots of Essential Biodiverstiy
Variables using common scale-bar. The function is helpful
to compare EBV indicators.}
\usage{plotebv(ebv, col.regions = rev(viridis_pal(option = "D")(255)), 
    ...)}
\arguments{
  \item{ebv}{\code{Raster*}. Raster Object.}
  \item{col.regions}{. Color
palette. If
null then
\code{viridis_pal(option
= 'D')} is
implemented.}
  \item{\dots}{
}
}

\value{\code{levelplot}.}

\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzales [aut]}




\examples{
    ## Warnings from GDAL/PROJ are suppressed.

    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- suppressWarnings(brick(path.))
    
    ## Changes in layers of tree-canopy cover (TC) in the 'amazon'
    ## brick are computed:
    suppressWarnings(
    def <- echanges(amazon, eco = 'TC',
                    change = 'lossyear',
                    eco_range = c(1,80),
                    get_unaffected = TRUE,
                    binary_output = FALSE,
                    mc.cores = 2)
    )
    
    ## Function 'plotebv' allows comparing rasters using a common scale bar:
    suppressWarnings(
    plotebv(def)
)
}
