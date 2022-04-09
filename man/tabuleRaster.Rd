\name{tabuleRaster}
\alias{tabuleRaster}
\title{Fast tabulation of pixel values}
\description{This function generates frequency tables for scenes in ecosystem
remote sensing products by wrapping \code{\link{rasterDT}}. The
function is mapped by \code{\link{gaugeIndicator}} to optimize
computation of ecoystem extents.}
\usage{tabuleRaster(layer = "", 
    del0 = TRUE, useNA = "no", 
    n256 = FALSE)}
\arguments{
  \item{layer}{\code{character}. File path to an ERSP scene.}
  \item{del0}{\code{logical}. Remove the 0-count categories.}
  \item{useNA}{\code{logical}. Include NA values. This
argument is passed to \code{rasterDT::freqDT}.}
  \item{n256}{\code{logical}. Do the raster
contains less than 256 unique values?}
}

\value{\code{data.frame}.}

\author{Ivan Gonzalez [aut], Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut],  Maria C. Londono [ctb]}




\examples{
 \donttest{
 tabuleRaster(raster(volcano), n256 = FALSE)
 }
}
