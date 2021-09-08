\name{plotebv}
\alias{plotebv}
\title{Plot EBV}
\description{This function displays level plots for ecosystem remote sensing
products using common scale-bars. The function is helpful to
visualize patterns in biodiversity indicators.}
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
  \item{\dots}{. Further arguments in \code{panel.levelplot()}}
}

\value{\code{levelplot}.}

\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [aut], Maria C. Londono [aut]}




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
