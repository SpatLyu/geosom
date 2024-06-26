% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geosom_bestparam.R
\name{geosom_bestparam}
\alias{geosom_bestparam}
\title{Best param for geosom model}
\usage{
geosom_bestparam(
  data,
  coords,
  wt,
  xdim,
  ydim,
  topo = c("rectangular", "hexagonal"),
  neighbourhood.fct = c("bubble", "gaussian"),
  cores = 1,
  ...
)
}
\arguments{
\item{data}{A data.frame or tibble}

\item{coords}{The coordinate column name in the \code{data}.}

\item{wt}{The weight vector of spatial coordination.}

\item{xdim}{X dimensions of the grid, a numeric vector.}

\item{ydim}{Y dimensions of the grid, a numeric vector.}

\item{topo}{(optional) Default use \code{hexagonal} and \code{rectangular}.}

\item{neighbourhood.fct}{(optional) Default use \code{bubble} and \code{gaussian} neighbourhoods together
when training a GeoSOM model.}

\item{cores}{positive integer(default is 1). If cores > 1, a 'parallel' package
cluster with that many cores is created and used. You can also supply a cluster
object.}

\item{...}{(optional) Other arguments passed to \code{geosom()}.}
}
\value{
A list with the optimal parameter in the provided parameter combination and
the corresponding error.
}
\description{
Function for determining the best parameter for the geosom model
}
\examples{
\dontrun{
data(pmc)
set.seed(2004)
geosom_bestparam(data = pmc, coords = c("centroidx","centroidy"),
wt = seq(0.1,5,by = 0.1),xdim = 3:10, ydim = 3:10,cores = 6)
}
}
\author{
Wenbo Lv \email{lyu.geosocial@gmail.com}
}
