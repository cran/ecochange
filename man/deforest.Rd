\name{deforest}
\alias{deforest}
\title{Deforest EBV}
\description{This function can extract/subtract deforested areas from essential
biodiversity variables (\code{ebv}) while focusing the analysis on
a predefined area of occupancy.}
\usage{deforest(stk, ebv, loss = "lossyear", 
    aoo, ebv.vals, loss.vals, 
    incremental = TRUE, 
    remnant.areas = TRUE, 
    keep.ebv = FALSE, 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{stk}{\code{Raster*}. Stack of \code{ebv} such as that produced
by \code{\link{rsp2ebv}} containing a \code{'lossyear'}
layer.}
  \item{ebv}{\code{character}. Name of the target layers in
\code{stk}.}
  \item{loss}{\code{character}. Name of the
\code{'lossyear'} layer. Only for the case
that it has a different name in
\code{stk}. Default \code{'lossyear'}.}
  \item{aoo}{\code{character}. Name of an optional binary raster in
\code{stk} used to focus the analysis on a specific Area of
Occupancy.}
  \item{ebv.vals}{\code{numeric}.  Cell values in \code{ebv}. If
missing then the whole range of values is processed}
  \item{loss.vals}{\code{numeric}. Cell values in \code{loss}. If
missing then the function try to extract the values
from the target layers in \code{ebv}. Otherwise the
whole range of values in \code{loss} is processed.}
  \item{incremental}{\code{logical}. Develop \code{incremental}
instead of discrete masking along the
\code{ebv.vals}. Default \code{TRUE}.}
  \item{remnant.areas}{\code{logical}. Extract from \code{ebv}
remnant areas instead of deforested areas
. Default \code{TRUE}.}
  \item{keep.ebv}{\code{logical}. Keep in the extracted areas
the corresponding cell values in
\code{ebv}. Default \code{FALSE} produces
binary masks: \code{c(0,1)}.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent CPU
capacity.}
}

\value{\code{RasterBrick}.}
\references{{Jetz, W., McGeoch, M. A.,
Guralnick, R., Ferrier, S., Beck, J., Costello,
M. J., ... & Meyer, C. (2019). Essential
biodiversity variables for mapping and
monitoring species populations. Nature Ecology &
Evolution, 3(4), 539-551.}

{Pekel, J. F., Cottam, A., Gorelick, N., & Belward,
A. S. (2016). High-resolution mapping of global
surface water and its long-term changes. Nature,
540(7633), 418-422.}

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
    
    ## Function 'plotebv' allows comparing rasters using a common scale bar:
    suppressWarnings(
    plotebv(def)
)
}
