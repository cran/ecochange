\name{getWRS}
\alias{getWRS}
\title{Get WRS}
\description{This function processes regions of interest (a polygon geometry or
\code{GADM} unit) to find corresponding Landsat Path/Row World
Reference System (\code{WRS}) polygons. This function is
internally implemented by \code{\link{getrsp}}}
\usage{getWRS(roi = NULL, path = tempdir(), 
    ...)}
\arguments{
  \item{roi}{\code{SpatialPolygonsDataFrame}; or
\code{character}; or \code{NULL}. Region of
interest. This can be whether 1) a polygon geometry;
or 2) the name of a \code{GADM} unit (see
\code{\link{getGADM}}); or 3) a \code{NULL}
value. Default \code{NULL} makes the function to
print a list of \code{GADM} units.}
  \item{path}{\code{character}. Path name indicating where
the \code{WRS} data are processed.}
  \item{\dots}{Additional arguments in \code{\link{getGADM}}.}
}

\value{\code{SpatialPolygonsDataFrame}, or set of \code{GADM} units.}

\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
load(system.file('cchaira_roi.RData',package = 'ecochange'))
## \\donttest{
## wrs_cchaira <- getWRS(cchaira_roi)
##     plot(wrs_cchaira)
## }
}
