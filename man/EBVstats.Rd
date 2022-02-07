\name{EBVstats}
\alias{EBVstats}
\title{EBV Stats}
\description{This function is a wrapper of \code{cellStats} used to compute
statistics of essential biodiversity variables.}
\usage{EBVstats(ccp, stats, 
    ...)}
\arguments{
  \item{ccp}{\code{RasterStack} or \code{NULL}. If \code{NULL} then
\code{NULL} is returned.}
  \item{stats}{\code{character}. vector of \code{stats} defined in
\code{\link{cellStats}}. If missing then six summary
statistics, including \code{'mean'}, \code{'sd'},
\code{'min'}, \code{'max'}, are computed.}
  \item{\dots}{Additional arguments in \code{cellStats}}
}

\value{\code{tibble}.}

\author{Wilson Lara Henao <wilarhen@gmail.com> [aut, cre], Victor Gutierrez-Velez [aut], Ivan Gonzalez [ctb], Maria C. Londono [ctb]}




\examples{
## Brick with structural Essential Biodiversity Variables covering the
## extent of a location in the northern Amazon basin (Colombia):

path. <- system.file('amazon.grd',package = 'ecochange')
amazon <- brick(path.)

## Tree-cover layers in the 'amazon' brick are both formatted and
## deforested:

def <- echanges(amazon, eco = 'TC',
                change = 'lossyear',
                eco_range = c(1,80),
                get_unaffected = TRUE,
                binary_output = FALSE,
                mc.cores = 2)

## Deforestation Statistics:

defstats <- EBVstats(def)

## barplot method:

barplot(defstats)
}
