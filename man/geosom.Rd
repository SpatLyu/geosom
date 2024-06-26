% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geosom_model.R
\name{geosom}
\alias{geosom}
\title{Spatially-aware Self-Organizing Maps(GeoSOM) model}
\usage{
geosom(data, coords, wt, grid, normalize = TRUE, ...)
}
\arguments{
\item{data}{A data.frame or tibble}

\item{coords}{The coordinate column name in the \code{data}.}

\item{wt}{The weight of spatial coordination and the weight of the non-spatial attribute is 1.
If wt is 0,now it equal to som model.}

\item{grid}{A grid for the codebook vectors: see \code{geosomgrid}.}

\item{normalize}{(optional)Boolean, indicating whether non-spatial feature data should
be normal standardization,default is \code{True}.}

\item{...}{Other arguments passed to \code{kohonen::supersom()},see \code{?kohonen::supersom()}.}
}
\value{
An object of class "kohonen" with components.
}
\description{
Implementation of spatially-aware self-Organizing maps(GeoSOM) model model
based on \code{kohonen::supersom()}.
}
\examples{
data(pmc)
set.seed(2004)
g = geosom(data = pmc, coords = c("centroidx","centroidy"),
wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
}
\author{
Wenbo Lv \email{lyu.geosocial@gmail.com}
}
