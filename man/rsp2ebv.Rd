\name{rsp2ebv}
\alias{rsp2ebv}
\title{Remote Sensing Product to EBV}
\description{This function processes regions of interest (polygon geometry or
\code{GADM} unit) to integrate dissimilar remote sensing products
(RSP) into Essential biodiversity variables.}
\usage{rsp2ebv(roi = NULL, ..., 
    lyrs = NULL, path, 
    sr, ofr = c(30, 30))}
\arguments{
  \item{roi}{\code{SpatialPolygonsDataFrame}; or
\code{character}; or \code{NULL}. Region of
interest. This can be whether 1) a polygon geometry;
or 2) the name of a \code{GADM} unit (see
\code{\link{getGADM}}); or 3) a \code{NULL}
value. Default \code{NULL} makes the function to
print a list of \code{GADM} units.}
  \item{\dots}{If \code{roi} is a \code{GADM} unit then additional
arguments in \code{\link{getGADM}} can be specified here.}
  \item{lyrs}{\code{character}. Remote sensing products, If
\code{NULL} then a list of products is printed, see
\code{\link{listGP}}.  Default \code{NULL}.}
  \item{path}{\code{character}. Path name indicating where the
variables are stored. If it is missing then a folder
named as \code{'ecochange'} located in a current
temporary directory is used.}
  \item{sr}{\code{character}. \code{PROJ.4} description of the target
coordinate reference system. If missing then the target
layers are projected to metric system \code{UTM}.}
  \item{ofr}{\code{numeric}. \code{c(xres,yres)}.  Output
file resolution (in target georeferenced
units). Default \code{c(30,30)} m2.}
}
\details{This function implements
\code{'gdalUtils'} so it assumes the user
has a working GDAL on their system. From
the documentation: "If the
\code{'gdalUtils_gdalPath'} option has
been set (usually by
\code{'gdal_setInstallation'}), the GDAL
found in that \code{path} will be used. If
nothing is found,
\code{'gdal_setInstallation'} will be
executed to attempt to find a working
GDAL that has the right drivers as
specified with the \code{'of'} (output
format) parameter", see example below.}
\value{\code{RasterBrick} of essential biodiversity variables (UTM crs), or
character lists suggesting GADM units/Global Products that can be
used to download \code{rsp} (see \code{NULL} defaults in arguments
\code{'roi'} and \code{'lyrs'}).}
\references{{Jetz, W., McGeoch, M. A.,
Guralnick, R., Ferrier, S., Beck, J.,
Costello, M. J., ... & Meyer,
C. (2019). Essential biodiversity variables
for mapping and monitoring species
populations. Nature Ecology & Evolution, 3(4),
539-551.}

{O'Connor, B., Secades, C., Penner, J.,
Sonnenschein, R., Skidmore, A., Burgess,
N. D., & Hutton, J. M. (2015). Earth
observation as a tool for tracking progress
towards the Aichi Biodiversity Targets. Remote
sensing in ecology and conservation, 1(1),
19-28.}

{Skidmore, A. K., & Pettorelli,
N. (2015). Agree on biodiversity metrics to
track from space: Ecologists and space
agencies must forge a global monitoring
strategy. Nature, 523(7561), 403-406.}}
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut]}




\examples{
## First, we'll check to make sure there is a valid GDAL
## installation (from 'gdalUtils):

## \\donttest{
## gdalUtils::gdal_setInstallation()
## valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
## }

## Warnings from GDAL/PROJ are suppressed.

## A Global Surface Water layer ('seasonality') covering the extent of a
## Colombian municipality Cartagena del Chairá is formated into an
## spatial EBV:
        load(system.file('cchaira_roi.RData',package = 'ecochange'))

 \donttest{
 suppressWarnings(
 rsp_cchaira <- getrsp(roi = cchaira_roi,
   lyrs = 'seasonality', mc.cores = 2, path = tempdir())
)

 file.exists(rsp_cchaira) #TRUE

 suppressWarnings(
 season_cchaira <- rsp2ebv(roi = cchaira_roi,
                               lyrs = 'seasonality', path = tempdir())
 )

 suppressWarnings(
 plotebv(season_cchaira)
 )
 }
}
