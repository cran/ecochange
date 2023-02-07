\name{getGADM}
\alias{getGADM}
\title{Get Geographic Adminitrative Unit}
\description{This function is a wrapper of \code{\link{gadm}} that helps users
to retrieve Geographic Administrative Data Maps (\code{GADM}).}
\usage{getGADM(unit.nm = NULL, 
    level = 2, country = "COL", 
    path = tempdir())}
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
\code{1=first administrative subdivision}, and \code{2=second administrative subdivision}).}
  \item{country}{\code{character}. \code{ISO} code specifying
a \code{country}. Default \code{'COL'}}
  \item{path}{\code{character}. Path name indicating where
the unit will be stored. Default stores the
data in a temporary directory.}
}

\value{\code{SpatialPolygonsDataFrame} or \code{character} vector of
\code{GADM} units..}
\references{\href{https://gadm.org/}{https://gadm.org/}}
\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
## Printing municipalities of Colombia:

     \donttest{
         muni <- getGADM(NA)
         head(muni)
     }

}
