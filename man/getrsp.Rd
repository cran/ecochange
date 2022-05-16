\name{getrsp}
\alias{getrsp}
\title{Get remote sensing product}
\description{This function processes the extent of a predefined region of
interest (polygon geometry or \code{GADM} unit) to download
ecosystem remote sensing products (ERSP). Downloadable ERSP include
Global Surface Water, Forest Change, and Continuous Tree Cover
data. See \code{\link{listGP}}.}
\usage{getrsp(roi = NULL, ..., 
    lyrs = NULL, path, 
    rewrite.pass = FALSE, 
    verify.web = FALSE, 
    mc.cores = round(detectCores() * 
        0.6, 0))}
\arguments{
  \item{roi}{\code{SpatialPolygonsDataFrame}; or
\code{character}; or \code{NULL}. Region of
interest. This can be either 1) a polygon geometry;
or 2) the name of a \code{GADM} unit (see
\code{\link{getGADM}}); or 3) a \code{NULL}
value. Default \code{NULL} makes the function to
print a list of \code{GADM} units.}
  \item{\dots}{If \code{roi} is a \code{GADM} unit then additional
arguments in \code{\link{getGADM}}.}
  \item{lyrs}{\code{character}. Remote-sensing
products. Default \code{NULL} makes the function to
print a list of Downloadable data, see
\code{\link{listGP}}.}
  \item{path}{\code{character}. Path name indicating where the
variables are stored. If missing then a folder
named as \code{'ecochange'} created in a current
temporary directory is used.}
  \item{rewrite.pass}{\code{logical}. Rewrite password. Only
valid to download new NASA Earth data, see
details section.}
  \item{verify.web}{\code{logical}. Verify in the web whether
the \code{URL}s used to download the
\code{rsp} are available. See
\code{getOption('webs')}. Default \code{FALSE}.}
  \item{mc.cores}{\code{numeric}. The
number of cores. Default
uses around 60 percent of the
cores.}
}
\details{Downloads of Continuous Tree Cover
data require user authentication through
the NASA Earth data Login. To obtain a NASA
Earth data Login account, please visit:
\href{https://urs.earthdata.nasa.gov}{https://urs.earthdata.nasa.gov/users/new}.}
\value{Path names of the remote sensing products just retrieved, or
character vectors suggesting GADM units/Global Products that can be
used to download ERSP (see \code{NULL} defaults in arguments
\code{'roi'} and \code{'lyrs'}).}
\references{{Pekel, J. F., Cottam, A.,
Gorelick, N., & Belward,
A. S. (2016). High-resolution mapping of
global surface water and its long-term
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
\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
## Polygon of the Colombian municipality of Cartagena del Chaira:
    load(system.file('cchaira_roi.RData',package = 'ecochange'))

## A Global Surface Water layer ('seasonality') which covers the
## extent of the polygon is retrieved:

 \donttest{
 rsp_cchaira <- getrsp(cchaira_roi,
   lyrs = 'seasonality', mc.cores = 2, path = tempdir())
 file.exists(rsp_cchaira)
 }
}
