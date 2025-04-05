\name{getGADM}
\alias{getGADM}
\title{Get Geographic Adminitrative Unit}
\description{This function can retrieve Geographic Administrative Data Maps
(\code{GADM}).}
\usage{getGADM(unit.nm = NULL, 
    level = 2, country = "COL", 
    ext = "json", path = tempdir())}
\arguments{
  \item{unit.nm}{\code{character} or \code{NULL}. Name of
Geographic Administrative Data Map (e.g.,
municipality), or the name of such an unit plus
its corresponding higher-level unit (e.g.,
department/state). If \code{NULL} then a list of
administrative subdivisions is printed.}
  \item{level}{\code{numeric}. A number between zero and two,
indicating any of the levels of administrative
subdivisions (\code{0=country},
\code{1=first administrative subdivision}, and
\code{2=second administrative subdivision}).}
  \item{country}{\code{character}. \code{ISO} code specifying
a \code{country}. Default \code{'COL'}.}
  \item{ext}{\code{character}. File
extension of the retrieved data file. Default
\code{'json'}.}
  \item{path}{\code{character}. Path name indicating where
the unit will be stored. Default stores the
data in a temporary directory.}
}

\value{\code{SpatialPolygonsDataFrame} or \code{character} vector of
\code{GADM} units..}
\references{\href{https://gadm.org/}{https://gadm.org/}}
\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




\examples{
## Printing municipalities of Colombia:

    ## \\donttest{
    ##     muni <- getGADM(NA)
    ##     head(muni)
    ## }

}
