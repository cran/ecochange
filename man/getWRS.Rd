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

\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




\examples{
load(system.file('cchaira_roi.RData',package = 'ecochange'))
\donttest{
wrs_cchaira <- getWRS(cchaira_roi)
    plot(wrs_cchaira)
}
}
