\name{tabuleRaster}
\alias{tabuleRaster}
\title{Fast tabulation of pixel values}
\description{This function generates frequency tables for scenes in ecosystem
remote sensing products by wrapping \code{\link[rasterDT]{rasterDT}}. The
function is mapped by \code{\link{gaugeIndicator}} to optimize
computation of ecoystem extents.}
\usage{tabuleRaster(layer = "", 
    del0 = TRUE, useNA = "no", 
    n256 = FALSE)}
\arguments{
  \item{layer}{\code{character}. File path to an ERSP scene.}
  \item{del0}{\code{logical}. Remove the 0-count categories.}
  \item{useNA}{\code{logical}. Include NA values. This
argument is passed to \code{\link[rasterDT]{freqDT}}.}
  \item{n256}{\code{logical}. Do the raster
contains less than 256 unique values?}
}

\value{\code{data.frame}.}

\author{Wilson Lara Henao [aut, cre] (<https://orcid.org/0000-0003-3527-1380>),
Victor Gutierrez-Velez [aut] (<https://orcid.org/0000-0003-1338-2020>),
Ivan Gonzalez [ctb] (<https://orcid.org/0000-0002-0313-398X>),
Maria C. Londono [ctb] (<https://orcid.org/0000-0002-2317-5503>)}




\examples{
\donttest{
tabuleRaster(raster(volcano), n256 = FALSE)
}
}
