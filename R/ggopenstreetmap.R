#' Plot OpenStreetMap Tiles On 'ggplot2'
#'
#' @param obj A `sf` object.
#' @param zoomin Additional zoom level.
#' @examples
#' library(ggplot2)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +
#'   annotation_osm(nc) +
#'   geom_sf(aes(fill = AREA), alpha = 0.5)
#' @export
annotation_osm <- function(obj, zoomin = +2) {
  bbox <- sf::st_bbox(obj)
  width_or_height <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
  zoom <- sum(width_or_height < 360 / 2^(0:19)) - 1 + zoom

  xy <- bbox2xy(bbox, zoom)
  xy2gg(xy$x, xy$y, zoom)
}

sec <- function(x) {
  1 / cos(x)
}

bbox2xy <- function(bbox, zoom) {
  n <- 2^zoom

  lat_deg <- bbox[c("xmin", "xmax")] + 180
  x <- (n * lat_deg) %/% 360
  lon_rad <- bbox[c("ymin", "ymax")] * pi / 180
  y <- (n * (1 - log(tan(lon_rad) + sec(lon_rad)) / pi)) %/% 2

  unique(expand.grid(x = seq(x[1], x[2]), y = seq(y[1], y[2])))
}


xy2urls <- function(x, y, zoom) {
  abc <- sample(c("a", "b", "c"), length(x), replace = TRUE)
  purrr::pmap(
    list(abc = abc, x = x, y = y),
    function(abc, x, y) {
      glue::glue("https://{abc}.tile.openstreetmap.org/{zoom}/{x}/{y}.png")
    }
  )
}


xy2pngs <- function(x, y, zoom) {
  urls <- xy2urls(x, y, zoom)
  pngs <- purrr::map(urls, httr::GET)

  purrr:::walk(pngs, httr::stop_for_status)

  purrr::map(pngs, httr::content)
}

xy2gg <- function(x, y, zoom) {
  pngs <- xy2pngs(x, y, zoom = zoom)
  positions <- purrr::map2(x, y, xy2lonlats, zoom = zoom)
  purrr::map2(pngs, positions,
              ~ ggplot2::annotation_raster(.x, xmin = .y$xmin, ymin = .y$ymin, xmax = .y$xmax, ymax = .y$ymax,
                                           interpolate = TRUE))
}

xy2lonlat <- function(x, y, zoom) {
  n <- 2^zoom
  lon_deg = x / n * 360.0 - 180.0
  lat_rad = atan(sinh(pi * (1 - 2 * y / n)))
  lat_deg = lat_rad * 180.0 / pi
  list(x = lon_deg, y = lat_deg)
}

xy2lonlats <- function(x, y, zoom) {
  nw_corner <- xy2lonlat(x, y, zoom)
  se_corner <- xy2lonlat(x + 1, y + 1, zoom)
  list(xmin = nw_corner$x, ymin = se_corner$y, xmax = se_corner$x, ymax = nw_corner$y)
}
