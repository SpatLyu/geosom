#' @title Spatially-aware Self-Organizing Maps(GeoSOM) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Implementation of spatially-aware self-Organizing maps(GeoSOM) model model
#' based on `kohonen::supersom()`.
#'
#' @param data A data.frame or tibble
#' @param coords The coordinate column name in the `data`.
#' @param wt The weight of spatial coordination and the weight of the non-spatial attribute is 1.
#' If wt is 0,now it equal to som model.
#' @param grid A grid for the codebook vectors: see `geosomgrid`.
#' @param normalize (optional)Boolean, indicating whether non-spatial feature data should
#' be normal standardization,default is `True`.
#' @param ... Other arguments passed to `kohonen::supersom()`,see `?kohonen::supersom()`.
#'
#' @return An object of class "kohonen" with components.
#' @importFrom kohonen supersom
#' @export
#'
#' @examples
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
geosom = \(data,coords,wt,grid,normalize = TRUE,...) {
  geodata = data.matrix(data[,(names(data) %in% coords)])
  data = data.matrix(data[,!(names(data) %in% coords)])
  spatial.weight = c(1, wt)
  if (normalize) {
    data = scale(data)
  }

  if (wt == 0) {
    res_som = kohonen::supersom(list(data), grid,
                                normalizeDataLayers=FALSE, ...)
  } else {
    res_som = kohonen::supersom(list(data, geodata), grid,
                                user.weights = spatial.weight,
                                normalizeDataLayers = FALSE, ...)
  }
  return(res_som)
}

#' @title GeoSOM-grid related functions
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Set up a grid of units, of a specified size and topology.Wrapper of `kohonen::somgrid`.
#'
#' @param xdim X dimensions of the grid
#' @param ydim Y dimensions of the grid.
#' @param topo (optional)Choose between a `hexagonal` or `rectangular` topology,default is `hexagonal`.
#' @param neighbourhood.fct (optional)Choose between `bubble` and `gaussian` neighbourhoods when training a
#' GeoSOM."bubble" or "gaussian"(default).
#' @param toroidal (optional)Logical, whether the grid is `toroidal` or not.Default is `FALSE`.
#'
#' @return An object of class "somgrid", with elements pts, and the input arguments to the function.
#' @importFrom kohonen somgrid
#' @export
geosomgrid = \(xdim,ydim,topo = "hexagonal",
               neighbourhood.fct = "gaussian",
               toroidal = FALSE) {
  kohonen::somgrid(xdim,ydim,topo,neighbourhood.fct,toroidal)
}
