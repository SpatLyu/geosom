#' @title Best param for geosom model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining the best parameter for the geosom model
#'
#' @param data A data.frame or tibble
#' @param coords The coordinate column name in the `data`.
#' @param wt The weight vector of spatial coordination.
#' @param xdim X dimensions of the grid, a numeric vector.
#' @param ydim Y dimensions of the grid, a numeric vector.
#' @param topo (optional) Default use `hexagonal` and `rectangular`.
#' @param neighbourhood.fct (optional) Default use `bubble` and `gaussian` neighbourhoods together
#' when training a GeoSOM model.
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param ... (optional) Other arguments passed to `geosom()`.
#'
#' @return A list with the optimal parameter in the provided parameter combination and
#' the corresponding error.
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply
#' @importFrom kohonen supersom
#' @importFrom tidyr crossing
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols arrange desc
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
#' \dontrun{
#' data(pmc)
#' set.seed(2004)
#' geosom_bestparam(data = pmc, coords = c("centroidx","centroidy"),
#' wt = seq(0.1,5,by = 0.1),xdim = 3:10, ydim = 3:10,cores = 6)
#' }
geosom_bestparam = \(data,coords,wt,xdim,ydim,
                     topo = c("rectangular", "hexagonal"),
                     neighbourhood.fct = c("bubble", "gaussian"),
                     cores = 1,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  coordf = data.matrix(data[,(names(data) %in% coords)])
  feadf = data.matrix(data[,!(names(data) %in% coords)])
  paradf = tidyr::crossing("wt" = wt,
                           "xdim" = xdim,
                           "ydim" = ydim,
                           "topo" = topo,
                           "neighbourhoodfct" = neighbourhood.fct)
  parak = split(paradf, seq_len(nrow(paradf)))

  geosom_est = \(data,geodata,wt,grid,normalize = TRUE,...) {
    spatial.weight = c(1, wt)
    if (normalize) {
      data = scale(data)
    }

    if (wt == 0) {
      res_som = kohonen::supersom(list(data), grid,
                                  normalizeDataLayers = FALSE, ...)
    } else {
      res_som = kohonen::supersom(list(data, geodata), grid,
                                  user.weights = spatial.weight,
                                  normalizeDataLayers = FALSE, ...)
    }
    return(res_som)
  }

  calcul_geosom = \(paramg){
    wt = paramg[[1]]
    grid = geosomgrid(xdim = paramg[[2]],
                      ydim = paramg[[3]],
                      topo = paramg[[4]],
                      neighbourhood.fct = paramg[[5]])
    gsom = geosom_est(feadf,coordf,wt,grid,...)
    gp = geosom_quality(gsom)
    gerr = c(gp$err.quant,gp$err.varratio,gp$err.topo,gp$err.kaski)
    names(gerr) = c('err_quant','err_varratio','err_topo','err_kaski')
    return(gerr)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('geosomgrid','geosom_quality'))
    out_g = parallel::parLapply(cores,parak,calcul_geosom)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_geosom)
  }

  out_g = dplyr::bind_cols(paradf,out_g) %>%
    dplyr::arrange(dplyr::desc(err_kaski)) %>%
    dplyr::arrange(dplyr::desc(err_quant)) %>%
    dplyr::arrange(dplyr::desc(err_topo)) %>%
    dplyr::arrange(err_varratio)
  return(as.list(out_g[1,]))
}
