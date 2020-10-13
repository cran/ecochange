\name{getGADM}
\alias{getGADM}
\title{Get Geographic Adminitrative Unit}
\description{This function is a wrapper of \code{\link{getData}} used to import
levels in Geographic Administrative Departments and Municipalities
(\code{GADM}).}
\usage{getGADM(unit.nm = NULL, 
    level = 2, country = "COL", 
    path = tempdir())}
\arguments{
  \item{unit.nm}{\code{character} or \code{NULL}. Name of an
administrative unit (e.g. municipality), or the
name of such an unit plus its corresponding
higher-level unit (e.g. department/state). If
\code{NULL} then a list of unit names is
printed.}
  \item{level}{\code{numeric}. A number between zero and two,
indicating any of the levels of administrative
subdivisions in \code{GADM} (\code{0=country},
\code{1=first level of subdivision}, and
\code{2=second level of subdivision}).}
  \item{country}{\code{character}. \code{ISO} code specifying
a \code{country}. Default \code{'COL'}}
  \item{path}{\code{character}. Path name indicating where
the unit will be stored.Default
\code{tempdir()}.}
}

\value{\code{SpatialPolygonsDataFrame} or \code{character} vector of
\code{GADM} units..}
\references{\href{https://gadm.org/}{https://gadm.org/}}
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut]}




\examples{
## Printing municipalities of Colombia:    

     \donttest{
         muni <- getGADM()
         head(muni)
     }

}
