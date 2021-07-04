\name{echanges}
\alias{echanges}
\title{Ecological changes}
\description{This function can compute ecological changes from spatial variables, focussing analyses on affected or unaffected areas and across predefined species distribution ranges.}
\usage{echanges(stk, eco = names(stk[[1:(nlayers(stk) - 
    1)]]), change = names(stk[[(nlayers(stk))]]), 
    sp_dist, eco_range = c(1, 
        100), change_vals = 1:19, 
    sp_dist_range = c(1, 
        1), spread = TRUE, 
    get_unaffected = TRUE, 
    binary_output = FALSE, 
    mc.cores = round(detectCores() * 
        0.6, 0), ...)}
\arguments{
  \item{stk}{\code{Raster*} or \code{SpatialPolygonsDataFrame}. Stack
of spatial variables or polygon geometry.}
  \item{eco}{\code{character}. Regular
expression matching the
name of the ecological
variable. Default uses
the names of the first
\code{1:(n-1)} layers in
\code{souk} are
processed.}
  \item{change}{\code{character}. Name
of the ecosystem-change variable. If
missing, then the name of the last
layer in \code{stk} is processed.}
  \item{sp_dist}{\code{character}. Name of an alternative species
distribution layer covering the region of interest.}
  \item{eco_range}{\code{numeric}. Range of values in the
ecological variable.}
  \item{change_vals}{\code{numeric}. Vector of values in the
\code{change} variable.}
  \item{sp_dist_range}{\code{numeric}. Range of values in the
species distribution layer.}
  \item{spread}{\code{logical}. Spread changes according to
both the number of layers in \code{eco} and the
values in \code{change_vals}. Users do not need
to \code{change} this argument. It is used by other
rouines to fasten computation of ecosystem
horizontal extents. If \code{FALSE} then two sets
of layers are extrated, including masks of
ecological variables and layers of
changes. Default \code{TRUE}.}
  \item{get_unaffected}{\code{logical}. Process unaffected
areas. If \code{FALSE} then ranges of
values in the ecological variable across
the changed areas are extracted. Default
\code{TRUE}.}
  \item{binary_output}{\code{logical}. Produce binary outputs
(masks). If \code{FALSE} then ranges of
values of the ecological variable are
maintained. Default \code{FALSE}.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent
CPU capacity.}
  \item{\dots}{If \code{stk} is a polygon then additional arguments in
\code{\link{rsp2ebv}}.}
}

\value{\code{RasterBrick}.}
\references{{Jetz, W., McGeoch, M. A.,
Guralnick, R., Ferrier, S., Beck, J.,
Costello, M. J., ... & Meyer,
C. (2019). Essential biodiversity variables
for mapping and monitoring species
populations. Nature Ecology & Evolution, 3(4),
539-551.}

{Pekel, J. F., Cottam, A., Gorelick, N., &
Belward, A. S. (2016). High-resolution mapping
of global surface water and its long-term
changes. Nature, 540(7633), 418-422.}

{Hansen, M. C., Potapov, P. V., Moore, R.,
Hancher, M., Turubanova, S. A., Tyukavina, A.,
... & Kommareddy, A. (2013). High-resolution
global maps of 21st-century forest cover
change. science, 342(6160), 850-853.}

{Sexton, J. O., Song, X. P., Feng, M.,
Noojipady, P., Anand, A., Huang, C., ... &
Townshend, J. R. (2013). Global, 30-m
resolution continuous fields of tree cover:
Landsat-based rescaling of MODIS vegetation
continuous fields with lidar-based estimates
of error. International Journal of Digital
Earth, 6(5), 427-448.}}
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzales [aut]}




\examples{

    ## Warnings from GDAL/PROJ are suppressed.

    ## Brick with structural Essential Biodiversity Variables covering the
    ## extent of a location in the northern Amazon basin (Colombia):
    path. <- system.file('amazon.grd',package = 'ecochange')
    amazon <- suppressWarnings(brick(path.))

\donttest{
    
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
}
