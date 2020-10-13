\name{listGP}
\alias{listGP}
\title{List of global products}
\description{This function prints a list of remote sensing products that can be
downloaded with \code{\link{getrsp}}.}
\usage{listGP(layer = TRUE, 
    Algorithm = TRUE, 
    author = TRUE, funs = FALSE, 
    api.code = FALSE)}
\arguments{
  \item{layer}{\code{character}. Add column \code{'layer'} to
the data.}
  \item{Algorithm}{\code{character}. Add column
\code{'Algorithm'} to the data.}
  \item{author}{\code{character}. Add column \code{'author'} to
the data.}
  \item{funs}{\code{character}. Add column \code{'funs'} to
the data.}
  \item{api.code}{\code{character}. Add column
\code{'api.code'} to the data.}
}

\value{\code{tibble}.}
\references{{Pekel, J. F., Cottam, A.,
Gorelick, N., & Belward,
A. S. (2016). High-resolution mapping of global
surface water and its long-term changes. Nature,
540(7633), 418-422.}

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
\author{Wilson Lara Henao <wilarhen@temple.edu> [aut, cre], Victor Gutierrez-Velez [aut]}




\examples{
lst <- listGP()

}
