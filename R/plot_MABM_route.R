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
#' @param type character string providing map theme. Reasonable choices include "terrain",
#'  "terrain-background", "satellite", "roadmap" (the default), and "hybrid" (google maps).
#' @param spp logical indicating whether detections should be categorized by species;
#'  defaults to TRUE.
#' @param bats character string indicating which, if any, bat calls to display along the
#'  MABM route.  Options available include "all" (default), "good", "bad", and "none".
#'  The distinction between "good" and "bad" calls depends on the user-specified
#'  \code{bad_gps} argument.
#' @param labels logical (default = TRUE) indicating whether every third detection
#'  should be labeled with a time stamp to assist in visualizing the route. Ignored if the
#'  user opts not to add detections to the route.
#' @param bad_gps integer indicating the absolute maximum number of seconds between a
#'  detected bat call and the nearest (in time) GPS location fix the user will consider as
#'  a "good" (usually interpreted "usable") georeference for a given bat call.  All calls
#'  greater than this value will be recorded as having a "bad" georeference. The actual
#'  time difference between a bat detection and the closest GPS location is noted in the
#'  output under the column heading 'GPS_diff'; positive (negative) values indicate the
#'  detection occurred after (before) the associated GPS location.
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @import ggplot2
#' @import ggmap
#' @importFrom dplyr "%>%"
#' @export

plot_MABM_route <- function(type = "roadmap", spp = TRUE,
                            bats = c("all", "good", "bad", "none"),
                            labels = TRUE, bad_gps = 5) {

    bats <- match.arg(bats)
    add_calls <- bats %in% c("all", "good", "bad")

    ## Set some graphical parameters
    # Setting theme for producing figures
    theme_set(theme_bw(base_size = 16))
    theme_update(panel.grid.minor = element_blank(),
                 panel.grid.major= element_blank(),
                 panel.border = element_blank(),
                 panel.background= element_blank(),
                 axis.line = element_line(color = "black"))

    #Create a custom color scale to consistently display species, if requested
    bat_fills <- c("orange3", "orange3", "sienna", "red2", "forestgreen", "forestgreen",
        "gray40", "gray40", "gray40", "gray40", "gray40", "gray40", "royalblue4",
        "gold", "white")
    names(bat_fills) <- c("CORA", "COTO", "EPFU", "LABO", "LACI", "LANO", "MYAU",
                          "MYGR", "MYLE", "MYLU", "MYSE", "MYSO", "NYHU", "PESU",
                          "UNKN")
    bat_fillScale <- scale_fill_manual(name = "Species", values = bat_fills,
                                       guide = guide_legend(override.aes=list(shape=22)))

    ## Pull in GPS route (SavedRoute)
    gps <- tcltk::tk_choose.files(default = "*.shp",
                           caption = "Select GPS shapefile (e.g., 'SavedRoute.shp).")

    ## Pull in call file, if requested
    if(add_calls) {
        # Extract file input directory
        trunc <- sapply(gregexpr("/", gps), tail, 1)
        in_dir <- substr(gps, 1, trunc)

        calls <- tcltk::tk_choose.files(default = paste0(in_dir, "*.shp"),
                                 caption = "Select bat call shapefile (e.g., 'Calls.shp').")
    }

    # Split GPS file name
    path_gps <- gps %>% strsplit(., "/") %>% unlist() %>% head(., -1) %>% paste(., collapse = "/")
    shape_gps <- gps %>% strsplit(., "/") %>% unlist() %>% tail(., 1) %>% tools::file_path_sans_ext()

    # Split call file name
    if (add_calls) {
        path_calls <- calls %>% strsplit(., "/") %>% unlist() %>% head(., -1) %>% paste(., collapse = "/")
        shape_calls <- calls %>% strsplit(., "/") %>% unlist() %>% tail(., 1) %>% tools::file_path_sans_ext()
    }

    # Read shapefile(s)
    gps <- rgdal::readOGR(path_gps, shape_gps, verbose = FALSE, stringsAsFactors = FALSE)
    if (add_calls) calls <- rgdal::readOGR(path_calls, shape_calls, verbose = FALSE, stringsAsFactors = FALSE)

    # Make data frame for plotting GPS route data
    plot_dat <- data.frame(gps@coords, gps@data)
    names(plot_dat)[1:2] <- c("lon", "lat") # rename coordinate fields

    # Add elapsed time
    plot_dat <- plyr::ddply(plot_dat, plyr::.(date), plyr::mutate,
                  t_seg = c(0, diff(lubridate::decimal_date(lubridate::ymd_hms(dt)))) * 525960, # elapsed time since last fix
                  t_elapsed = round(cumsum(t_seg), 1))

    ## Background map
    # Set bounding box of route for auto zoom calculation
    # May not always work perfectly but not too bad
    bbox <- make_bbox(lon, lat, plot_dat, f = 0.25)

    bg <- get_map(location = bbox, maptype=type, scale=2)
    bg <- ggmap(bg, extent="panel")

    p <- bg

    ## Add complete route to background map
    # We map the route using points so breaks in the GPS log are more apparent
    if (labels) {
        p <- p +
            geom_point(data = plot_dat, aes(x = lon, y = lat, colour = t_elapsed),
                       size = 2) +
            scale_colour_gradient("Elapsed time (minutes)", low = "#2166ac", high = "#b2182b",
                                  guide = guide_colorbar(direction = "horizontal",
                                                         title.position = "top"))
    } else {
        p <- p + geom_point(data = plot_dat, aes(x = lon, y = lat))
    }

    # Change axis labels and move legends
    p <- p + xlab("Longitude") + ylab("Latitude") +
        theme(legend.position = "top")

    ## If requested, add bat calls to the route
    # First, create data frame for plotting
    if (add_calls) {
        bat_dat <- data.frame(calls@coords, calls@data)
        names(bat_dat)[1:2] <- c("lon", "lat") # rename coordinate fields

        good_bats <- subset(bat_dat, abs(GPS_diff) <= bad_gps)
        bad_bats <- subset(bat_dat, abs(GPS_diff) > bad_gps)

        if (nrow(bad_bats) == 0) {
            cat(paste0("\nAll bat locations meet your criterion for a 'good' fix (GPS fix within ", bad_gps, " sec).\n"))
            cat("Showing all detected bat calls.\n\n")
            bats <- "good"
        }
    }

    if (bats == "bad") {

        # Add calls to plot
        if (spp) {
            p <- p +
                # Only points with "bad" GPS fixes
                geom_point(data = bad_bats, aes(fill = spp),
                           shape = 23, size = 8) +
                bat_fillScale
        } else {
            p <- p +
                geom_point(data = bad_bats,
                           shape = 23, fill = "grey50", size = 8)
        }

        p <- p +
            # Label "bad" GPS fixes with time difference
            geom_text(data = bad_bats,
                      aes(label = GPS_diff), size = 4)

        cat(paste0("\n", nrow(bad_bats), " bat call(s) meet your bad GPS fix criterion (>", bad_gps, " sec from closest GPS fix).\n\n"))
        print(bad_bats[, c("filename", "lat", "lon", "spp", "date", "time",
                           "call_id", "order", "GPS_diff")])
    }

    if (bats == "all") {

        # Label every 3rd call to avoid messy labels
        which_to_lab <- seq(1, nrow(bat_dat), 3)
        labels_dat <- bat_dat[which_to_lab, ]

        all_bats <- rbind(cbind(good_bats, bad = 0),
                          cbind(bad_bats, bad = 1))

        # Add calls to plot
        if (spp) {
            p <- p +
                geom_point(data = all_bats, aes(shape = as.factor(bad), fill = spp),
                           size = 8) +
                scale_shape_manual("", values = c(21, 23), guide = "none") +
                bat_fillScale
        } else {
            p <- p +
                geom_point(data = all_bats, aes(shape = as.factor(bad)), fill = "grey50", size = 8) +
                scale_shape_manual("", values = c(21, 23), guide = "none")
        }

        p <- p +
            # Label "bad" GPS fixes with time difference
            geom_text(data = subset(bat_dat, abs(GPS_diff) > bad_gps),
                      aes(label = GPS_diff), size = 4)

        if (labels) {
            # Label every 3rd point with time stamp
            p <- p + geom_text(data = labels_dat, aes(label = time), hjust = 0)
        }
    }

    if (bats == "good") {

        # Label every 3rd call to avoid messy labels
        which_to_lab <- seq(1, nrow(good_bats), 3)
        labels_dat <- good_bats[which_to_lab, ]

        # Add calls to plot
        if (spp) {
            p <- p +
                # Only points with "bad" GPS fixes
                geom_point(data = good_bats, aes(fill = spp),
                           shape = 21, size = 8) +
                bat_fillScale
        } else {
            p <- p +
                geom_point(data = good_bats,
                           shape = 21, fill = "grey50", size = 8)
        }

        # Add labels, if requested
        if (labels) {
            p <- p +
                # Label every 3rd point with time stamp
                geom_text(data = labels_dat, aes(label = time), hjust = 0)
        }
    }

    return(p)

}
