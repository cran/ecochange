\name{echanges}
\alias{echanges}
\title{Ecosystem changes}
\description{This function produces ecosystem-change maps by masking cell values
in a layer of ecosystem changes over a target set of ecosystem
variables. The function also allows focusing the ecosystem-change
analysis on a species distribution range.}
\usage{echanges(ps, eco = names(ps[[1:(nlayers(ps) - 
    1)]]), change = names(ps[[(nlayers(ps))]]), 
    sp_dist, eco_range = c(1, 
        100), change_vals = 1:19, 
    sp_dist_range = c(1, 
        1), spread = TRUE, 
    get_unaffected = TRUE, 
    binary_output = FALSE, 
    noDataValue = 0, 
    mc.cores = round(detectCores() * 
        0.6, 0), ...)}
\arguments{
  \item{ps}{\code{RasterStack} or
\code{SpatialPolygonsDataFrame}. Stack of spatial data,
including the target ecosystem variables, a layer of
changes, and an alternative layer of a species distribution
range. This argument can also be a polygon geometry used to
integrate such spatial data via implementation of
\code{\link{rsp2ebv}}; see the ellipsis term below.}
  \item{eco}{\code{character}. Regular
expression matching names
of a subset of layers
representing the target
ecosystem
variables. Default matches
names of the first
\code{1:(n-1)} layers in
\code{ps}.}
  \item{change}{\code{character}. Name of
the layer of ecosystem
changes. Default matches
the name of the last layer
in \code{ps}.}
  \item{sp_dist}{\code{character}. Name of an alternative layer
representing a species distribution range. If missing
then this argument is ignored.}
  \item{eco_range}{\code{numeric}. Range of values in the
target ecosystem variable.}
  \item{change_vals}{\code{numeric}. Vector of values in the
layer of ecosystem changes.}
  \item{sp_dist_range}{\code{numeric}. Range of values in the
alternative layer of species.
distribution range. This argument is
ignored if \code{sp_dist} is missing.}
  \item{spread}{\code{logical}. Spread representation of
ecosystem changes. Users do not need to \code{change}
this argument. It is used by other rouines to
fastening computation of ecosystem horizontal
extents. If \code{FALSE} then the function mask
cell values in the target ecosystem variables
over over the layer of ecosystem changes. Default
\code{TRUE}.}
  \item{get_unaffected}{\code{logical}. Extract unaffected
areas. If \code{FALSE} then pixel
values of the ecological variable across
the changed areas are extracted. Default
\code{TRUE}.}
  \item{binary_output}{\code{logical}. Produce binary outputs
(masks). If \code{FALSE} then ranges of
values of the ecological variable are
maintained. Default \code{FALSE}.}
  \item{noDataValue}{\code{numeric}. Output NoDataValue. Default
\code{0}.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent
of the CPU capacity.}
  \item{\dots}{If \code{ps} is a polygon then additional arguments in
\code{\link{rsp2ebv}}.}
}

\value{Class \code{echanges}.}
\references{{Jetz, W., McGeoch, M. A.,
Guralnick, R., Ferrier, S., Beck, J.,
Costello, M. J., ... & Meyer,
C. (2019). Essential biodiversity variables
for mapping and monitoring species
populations. Nature Ecology & Evolution, 3(4),
539-551.}

{Hansen, M. C., Potapov, P. V., Moore, R.,
Hancher, M., Turubanova, S. A., Tyukavina, A.,
... & Kommareddy, A. (2013). High-resolution
global maps of 21st-century forest cover
change. science, 342(6160), 850-853.}

{Pekel, J. F., Cottam, A., Gorelick, N., &
Belward, A. S. (2016). High-resolution mapping
of global surface water and its long-term
changes. Nature, 540(7633), 418-422.}

{Pereira, H.M., Ferrier, S., Walters,
M., Geller, G.N., Jongman, R.H.G., Scholes,
R.J., Bruford, M.W., Brummitt, N., Butchart,
S.H.M., Cardoso, A.C. and Coops, N.C.,
2013. Essential biodiversity

{Sexton, J. O., Song, X. P., Feng, M.,
Noojipady, P., Anand, A., Huang, C., ... &
Townshend, J. R. (2013). Global, 30-m
resolution continuous fields of tree cover:
Landsat-based rescaling of MODIS vegetation
continuous fields with lidar-based estimates
of error. International Journal of Digital
Earth, 6(5), 427-448.}
variables. Science, 339(6117), pp.277-278}.}
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

## Method 'plot.echanges' allows comparing rasters using a common scale bar:
plot.echanges(def)
}
