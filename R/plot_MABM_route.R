#' Plot MABM route and, if requested, bat calls detected along the route.
#'
#' This function plots a MABM route (from a shapefile created with \code{\link{MABM_route}})
#' and, if requested (with the \code{bats} argument), bat calls (by species identification)
#' detected along the route. To work properly this function requires an internet connection.
#'
#' With the \code{bats} argument, the user can specify whether all detected calls should be
#' displayed or only those that meet (or do not meet) a user-specified criterion for a "good"
#' georeferenced call. This criterion is specified as the number of seconds between a detected
#' call and the closest (in time) GPS fix.
#'
#' Species classifications can be turned off using \code{spp = FALSE}.
#'
#' By default, every third bat detection (when plotted) is labeled with its associated time
#' stamp and the route is shaded by time (minutes) elapsed from the first GPS fix to help
#' visualize the route.  These can be suppressed with \code{labels = FALSE}.
#'
#' @param bad_gps integer indicating the absolute maximum number of seconds between a
#'  detected bat call and the nearest (in time) GPS location fix the user will consider as
#'  a "good" (usually interpreted "usable") georeference for a given bat call.  All calls
#'  greater than this value will be recorded as having a "bad" georeference. The actual
#'  time difference between a bat detection and the closest GPS location is noted in the
#'  output under the column heading 'GPS_diff'; positive (negative) values indicate the
#'  detection occurred after (before) the associated GPS location.
#' @param gps character string that gives user the option to specify the path (full or
#'  relative) to the GPS point shapefile rather than using a dialog box
#' @param plot_type character choice between a 'dynamic' map using \code{leaflet}
#'  (default) or a 'static' map using \code{ggmap} and \code{ggplot2}; the latter two
#'  packages are installed if necessary
#' @return an HTML widget object
#' @import leaflet
#' @importFrom htmltools htmlEscape
#' @export

plot_MABM_route <- function(bad_gps = 5, gps = NULL, plot_type = c("dynamic", "static")) {

  plot_type <- match.arg(plot_type)

  GPS_diff = t_seg = "." = NULL  # Variable "declaration" for R CMD check

  #Create a custom color scale to consistently display species, if requested
  bat_fills <- c("orange3", "orange3", "sienna", "red2", "forestgreen", "forestgreen",
                 "gray40", "gray40", "gray40", "gray40", "gray40", "gray40", "royalblue4",
                 "gold", "white")
  names(bat_fills) <- c("CORA", "COTO", "EPFU", "LABO", "LACI", "LANO", "MYAU",
                        "MYGR", "MYLE", "MYLU", "MYSE", "MYSO", "NYHU", "PESU",
                        "UNKN")
  sppPal <- colorFactor(palette = bat_fills, domain = names(bat_fills))

  ## Pull in GPS route (SavedRoute)
  if (is.null(gps)) {
    gps <- utils::choose.files(default = "*.shp",
                               caption = "Select GPS shapefile (e.g., 'SavedRoute.shp).",
                               multi = FALSE)
  } else {
    gps <- normalizePath(gps)
  }
  if (length(gps) == 0) stop("Function cancelled.  No GPS shapefile selected.")
  if (!is.character(gps) || !file.exists(gps)) stop("GPS file not specified correctly. Try again")

  ## Pull in call file automatically
  # Assumes GPS and call shapefiles are named using `MABM_route` function convention and
  # in the same directory
  # Extract file input directory
  #trunc <- sapply(gregexpr("/", gps), tail, 1)
  #in_dir <- substr(gps, 1, trunc)
  #calls <- tcltk::tk_choose.files(default = paste0(in_dir, "*.shp"),
  #                                caption = "Select bat call shapefile (e.g., 'Calls.shp').")
  calls <- sub("SavedRoute", "Calls", gps)

  # Split GPS file name
  path_gps <- gps %>% strsplit(., "\\\\") %>% unlist() %>% head(., -1) %>% paste(., collapse = "/")
  shape_gps <- gps %>% strsplit(., "\\\\") %>% unlist() %>% tail(., 1) %>% tools::file_path_sans_ext()

  # Split call file name
  path_calls <- calls %>% strsplit(., "\\\\") %>% unlist() %>% head(., -1) %>% paste(., collapse = "/")
  shape_calls <- calls %>% strsplit(., "\\\\") %>% unlist() %>% tail(., 1) %>% tools::file_path_sans_ext()

  # Read shapefile(s)
  gps <- rgdal::readOGR(path_gps, shape_gps, verbose = FALSE, stringsAsFactors = FALSE)
  calls <- rgdal::readOGR(path_calls, shape_calls, verbose = FALSE, stringsAsFactors = FALSE)

  # Add elapsed time to GPS data set
  gps@data <- gps@data %>%
    dplyr::mutate(t_seg = c(0, diff(lubridate::decimal_date(lubridate::ymd_hms(dt)))) * 525960, # elapsed time since last fix
                  t_elapsed = round(cumsum(t_seg), 1))

  if (plot_type == "dynamic") {

    # Add route fill depicting elapsed time
    # Emulate viridis palette for color blindness
    elapsedPal <- colorNumeric(palette = c("#440154FF", "#482173FF", "#433E85FF", "#38598CFF",
                                           "#2D708EFF", "#25858EFF", "#1E9B8AFF", "#2BB07FFF",
                                           "#51C56AFF", "#85D54AFF", "#C2DF23FF", "#FDE725FF"),
                               domain = gps@data$t_elapsed)

    # Make bat icon list
    batIcons <- makeBatIconList()

    # Create map
    p <- leaflet() %>%
      # Base map group
      addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
               group = "Terrain") %>%
      addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               group = "Aerial") %>%
      # Add GPS fixes and color with gradient
      addCircleMarkers(data = gps, radius = 4, stroke = F,
                       color = ~elapsedPal(t_elapsed),
                       popup = ~paste("Date:", format(as.Date(date), format = "%d %b %Y"), "<br>",
                                      "Time:", time, "<br>",
                                      "Elapsed time:", t_elapsed, "min <br>")) %>%
      # Add "good" (georeferenced) bat detections
      addMarkers(data = subset(calls, abs(GPS_diff) <= bad_gps), group = "Good GPS fix",
                 options = markerOptions(zIndexOffset = ~order, riseOnHover = TRUE),
                 popup = ~sprintf("<b>Species: %s</b><hr noshade size='1'/>
                                  Time: %s<br/>
                                  Nearest GPS fix: %s sec</br>
                                  ID probability: %3.0f%%",
                                  htmlEscape(spp), htmlEscape(time),
                                  htmlEscape(GPS_diff), htmlEscape(disc_prob * 100)),
                 icon = ~batIcons[spp])

    # Add "bad" (georeferenced) bat detections if present
    if (nrow(subset(calls, abs(GPS_diff) > bad_gps)) > 0) {
      p <- p %>%
        addMarkers(data = subset(calls, abs(GPS_diff) > bad_gps), group = "Bad GPS fix",
                   options = markerOptions(zIndexOffset = ~order, riseOnHover = TRUE),
                   popup = ~sprintf("<b>Species: %s</b><hr noshade size='1'/>
                                  Time: %s<br/>
                                  Nearest GPS fix: %s sec</br>
                                  ID probability: %3.0f%%",
                                    htmlEscape(spp), htmlEscape(time),
                                    htmlEscape(GPS_diff), htmlEscape(disc_prob * 100)),
                   icon = ~batIcons[spp])
    }

    # Add species legend and layer control
    p <- p %>%
      addLegend("topleft", pal = sppPal, values = calls@data$spp,
                title = "Species", opacity = 1) %>%
      addLegend("bottomleft", pal = elapsedPal, values = gps@data$t_elapsed,
                title = paste("Elapsed", "<br>", "time (min)"), opacity = 1) %>%
      addLayersControl(baseGroups = c("Terrain", "Aerial"),
                       overlayGroups = c("Good GPS fix", "Bad GPS fix"),
                       options = layersControlOptions(collapsed = FALSE))

  } else {

      if (!requireNamespace("ggplot2", quietly = TRUE)) {
          message("The ggplot2 package is needed for static maps; installing from CRAN.")
          install.packages("ggplot2")
      }
      if (!requireNamespace("ggmap", quietly = TRUE)) {
      message("The ggmap package is needed for static maps; installing from CRAN.")
      install.packages("ggmap")
      }

    gps_df <- data.frame(lat = sp::coordinates(gps)[, 2],
                         lon = sp::coordinates(gps)[, 1],
                         gps@data)
    call_df <- data.frame(lat = sp::coordinates(calls)[, 2],
                        lon = sp::coordinates(calls)[, 1],
                        calls@data)

    bb_summary <- apply(sp::coordinates(gps), 2, range)
    bb <- ggmap::make_bbox(bb_summary[, 1], bb_summary[, 2])
    bm <- ggmap::get_map(bb, maptype = "terrain")

    ggplot2::theme_set(ggplot2::theme_classic(base_size = 16))
    p <- ggmap::ggmap(bm, maprange = FALSE, extent = "device",
          base_layer = ggplot2::ggplot(ggplot2::aes(x = lon, y = lat), data = gps_df)) +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::geom_point(data = call_df, ggplot2::aes(fill = spp),
                            shape = 21, size = 2, color = "black") +
        ggplot2::scale_fill_manual("Species", values = bat_fills,
                        guide = ggplot2::guide_legend(title.hjust =0.5,
                                                      title.position = "top",
                                                      nrow = 2)) +
        ggplot2::theme(legend.position = "top",
                       legend.background = ggplot2::element_blank(),
                       legend.key.width = ggplot2::unit(0.02, units = "npc"))
  }

  return(p)

}

