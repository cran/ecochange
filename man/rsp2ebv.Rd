\name{rsp2ebv}
\alias{rsp2ebv}
\title{Integrate remote sensing products}
\description{This function integrates ecosystem remote sensing products and
produces raster-data sections with the cell values enclosed in a
region of interest.}
\usage{rsp2ebv(ps = NULL, ..., 
    lyrs = NULL, path, 
    sr, ofr = c(30, 30), 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{ps}{\code{SpatialPolygonsDataFrame}; or
\code{sf}; or \code{character};
or \code{NULL}. Region of interest. This can be
whether 1) a polygon geometry; or 2) the name of a
\code{GADM} unit (see \code{\link{getGADM}}); or 3) a
\code{NULL} value. Default \code{NULL} makes the
function to print a list of \code{GADM} units.}
  \item{\dots}{Additional arguments in \code{\link{getGADM}} and
\code{\link{getrsp}}.}
  \item{lyrs}{\code{character}. Remote-sensing
products. Default \code{NULL} makes the function to
print a list of Downloadable data, see
\code{\link{listGP}}.}
  \item{path}{\code{character}. Path name indicating where the
variables are stored. If missing then a folder
named as \code{'ecochange'} created in a current
temporary directory is used.}
  \item{sr}{\code{character}. \code{PROJ.4} description of the target
coordinate reference system. If missing then the target
layers are projected to metric system \code{UTM}.}
  \item{ofr}{\code{numeric}. \code{c(xres,yres)}.  Output
file resolution (in target georeferenced
units). Default \code{c(30,30)} m2.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent of
the cores.}
}
\details{This function implements
\code{'sf::gdal_utils'} so it assumes the
user's machine has a valid GDAL
installation.}
\value{Class \code{echanges}.}
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
\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




\examples{
## A Global Surface Water layer ('seasonality') covering the extent of a
## Colombian municipality Cartagena del Chaira is formated into an
## spatial EBV:
        load(system.file('cchaira_roi.RData',package = 'ecochange'))

\donttest{
rsp_cchaira <- getrsp(cchaira_roi,
  lyrs = 'seasonality', mc.cores = 2, path = tempdir())

file.exists(rsp_cchaira)

season_cchaira <- rsp2ebv(cchaira_roi,
                              lyrs = 'seasonality', path = tempdir())
}
}
