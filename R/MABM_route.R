#' MABM shapefile creation, preparation for database import, and audio file scrubbing.
#'
#' This function associates bat calls classified with Bat Call Identification (BCID;
#' \url{http::/www.batcallid.com}) software with simultaneously collected geographic
#' location information.  For this application, bat calls were collected with (usually) an
#' Anabat SD2 or the older Anabat SD1 (\url{https://www.titley-scientific.com}) and
#' georeference by integrating the Anabat data logger, PDA and an external GPS.  From this
#' association, it also creates multiple shapefiles and a *.csv for incorporation into the
#' MABM Access database.  If requested, suspected noise files can be scrubbed (moved) to a
#' new subdirectory.
#'
#'  If \code{for_import = TRUE}, the function will rearrange the columns to match the expected
#'  import format of the MABM database and replace \code{NA}s with blanks to play nicely with
#'  expected variable types.  This necessitates conversion of most (all?) columns to text
#'  strings (which is not very handy if you want to work with the output in R).  This argument
#'  is forced to \code{for_import = FALSE} if the user specifies \code{keep_output = TRUE}.
#'
#'  If \code{keep_output = TRUE}, the function creates a list containing (1) a \code{data.frame}
#'  ('final_calls') containing information on the bat detections, (2) a
#'  \code{\link[sp]{SpatialPointsDataFrame-class}} ('route_pt') of all GPS fixes along the route, (3) a
#'  \code{\link[sp]{SpatialLinesDataFrame-class}} of these GPS fixes, and (4) a
#'  \code{\link[sp]{SpatialPointsDataFrame-class}} ('call_pt') of all bat detections. This is handy if the
#'  user wants to conduct further investigations in R. To access this output, remember to
#'  assign the function output to an R object.
#'
#' @param route_name character string indicating the name used to file the created output.  Default (NULL)
#'  prompts the user to select from a list of all MABM routes.  This argument is useful primarily
#'  when processing data external to the MABM program (e.g., data collected along a non-MABM route)
#' @param scrub logical indicating whether Anabat files (ending with '#'), if present, identified
#'  as noise (i.e., not assigned an ID in BCID) should be scrubbed (moved) to a newly created
#'  'scrubbed' subdirectory (default = TRUE); non-scrubbed files are not moved
#' @param gps logical (default = TRUE) indicating whether the a GPS file is available to georeference
#'  the BCID classification file
#' @param for_import logical indicating whether the output *.csv file should be formatted for import
#'  into the MABM Access database (default = TRUE).  See details.
#' @param keep_output logical (default = FALSE) that creates a list containing potentially
#'  useful outputs.
#' @param overwrite logical (default = FALSE) indicating whether to overwrite the output
#'  directory and files, if they exist.
#' @param plot logical (default = FALSE) indicating whether to plot the processed route using
#'  \code{\link{plot_MABM_route}}
#' @import sp
#' @importFrom rgdal writeOGR
#' @export
MABM_route <- function(route_name = NULL, scrub = TRUE, gps = TRUE,
                       for_import = TRUE, keep_output = FALSE, overwrite = FALSE,
                       plot = FALSE) {

    lat = lon = filename = "." = call_id = NULL # Variable "declaration" for R CMD check

    # If keeping output (keep_output = TRUE), override for_import
    if (keep_output) for_import <- FALSE

    ## Prompt user to specify route name (site_Name in Access)
    # Load sites
    sites <- read.csv(system.file("extdata", "site_list.csv", package = "MABM"), header = TRUE,
                      stringsAsFactors = FALSE)
    colons <- ifelse(grepl("\\d", substr(sites$Site_abbr, nchar(sites$Site_abbr), nchar(sites$Site_abbr))),
                     ": ", "")
    menu_items <- sort(paste0(sites$Site, colons, sites$Site_desc, " (", sites$Site_abbr, ")"))
    if (is.null(route_name)) {
        route_name <- utils::select.list(menu_items, title="Choose the MABM route", multiple = FALSE, graphics = TRUE)
        # Drop the full location
        route_name <- gsub(".*\\(|\\)", "", route_name)
    }
    if (route_name == "") stop("You must select or provide a route name.")

    ## Retrieve GPS and BCID files
    ## Assumes BCID file is in same directory
    # Call file first
    calls <- utils::choose.files(default = "*.xls",
                                 caption = "Select BCID output .xls file with bat call information.",
                                 multi = FALSE)
    if (length(calls) == 0) stop("Function cancelled.  No BCID output file selected.")
    route_date <- substr(basename(calls), 1, 8)

    # Extract file input directory
    trunc <- sapply(gregexpr("\\\\", calls), tail, 1)
    in_dir <- substr(calls, 1, trunc)

    # Assume GPS text file in same directory and called `gps.txt`
    # Notify if this is so; prompt user to specify if `gps.txt` not found
    if (!gps) {
        GPS <- structure(list(lat = NA_real_, lon = NA_real_, alt_m = NA_real_,
                              date = structure(NA_real_, class = "Date"), time = NA_character_,
                              dt = structure(NA_real_, tzone = "UTC", class = c("POSIXct", "POSIXt")),
                              call_id = NA_integer_, order = NA_integer_),
                         .Names = c("lat","lon", "alt_m", "date", "time", "dt", "call_id", "order"),
                         row.names = 1L, class = "data.frame")
    } else if (file.exists(paste0(in_dir, "gps.txt"))) {
        message("\nUsing 'gps.txt' found with ", basename(calls), " as GPS source.\n")
        GPS <- paste0(in_dir, "gps.txt")
    } else {
        GPS <- utils::choose.files(default = paste0(in_dir, "*.txt"),
                                   caption = "File 'gps.txt' not found. Please select GPS text file.",
                                   multi = FALSE)
    }
    if (length(GPS) == 0) stop("Function cancelled. No GPS text file selected.",
                               "You can specify `gps = FALSE` if none exists.")

    if (gps) {
        ## Initial GPS text file read and division into columns
        # Get number of lines to skip at beginning of GPS files
        # Looks for line that contains location headers
        n_skip <- grep("Latitude", readr::read_lines(GPS))

        # Assign variable types and names; drop first and last columns
        if (length(n_skip) == 1) {
            GPS <- readr::read_fwf(GPS, readr::fwf_empty(GPS, skip = n_skip),
                                   col_types = "_ccncc_", skip = n_skip)
        } else {
            GPS_string <- readr::read_lines(GPS)
            GPS_start <- head(n_skip, -1) + 1 # Drop last entry
            GPS_end <- (grep("H R", GPS_string) - 2)[-1] # Two rows before start of datum info
            # Drop empty segments
            keep <- which(GPS_end >= GPS_start)
            GPS_start <- GPS_start[keep]
            GPS_end <- GPS_end[keep]
            if (length(GPS_start) == 1 & length(GPS_end) == 1) {
                keep <- seq(GPS_start, GPS_end)
            } else {
                keep <- do.call(c, mapply(seq, GPS_start, GPS_end))
            }
            GPS <- GPS_string[keep] %>% paste(., collapse = "\n")
            GPS <- readr::read_fwf(GPS, readr::fwf_empty(GPS),
                                   col_types = "_ccncc_")
        }
        names(GPS) <- c("lat", "lon", "alt_m", "date", "time")

        # Restructure data
        #    GPS <- GPS %>% mutate(lat = as.numeric(substring(lat, 2)),
        #                          lon = ifelse(substring(lon, 1, 1) == "W",
        #                                       as.numeric(substring(lon, 2)) * -1,
        #                                       as.numeric(substring(lon, 2))),
        #                          date = lubridate::ymd(date),
        #                          dt = lubridate::ymd_hms(paste(date, time)),
        #                          call_id = gsub(":", "", time)) %>%
        #        arrange(dt) %>% # Sort chronologically
        #        mutate(order = 1:nrow(GPS)) # Add order variable to facilitate QA/QC

        # GPS quality control
        # Sometimes GPS logs an incorrect date, although the time is correct
        GPS <- gps_QC(GPS)

        # Use until new dplyr is released
        GPS <- plyr::ddply(GPS, plyr::.(date), plyr::mutate, # Doesn't like dplyr's mutate
                           lat = as.numeric(substring(lat, 2)),
                           lon = ifelse(substring(lon, 1, 1) == "W",
                                        as.numeric(substring(lon, 2)) * -1,
                                        as.numeric(substring(lon, 2))),
                           date = lubridate::ymd(date),
                           dt = lubridate::ymd_hms(paste(date, time)),
                           call_id = as.integer(gsub(":", "", time)))
        GPS <- dplyr::arrange(GPS, dt)
        GPS$order <- 1:nrow(GPS)
    }

    ## Call .xls file read and formatting
    # Get start/end rows of relevant call information in BCID .xls file
    # Looks at first sheet in .xls file
    call_string <- do.call(paste0, readxl::read_excel(calls, sheet = 1, col_names = FALSE)) #concatenates all columns to simplify search
    call_start <- grep("FILENAME", call_string) + 1 # Row after column headers; we have to make our own
    # This may need some modification in future if format changes...
    call_end <- grep("IDENTIFICATION", call_string) - 2 # Two rows before identification summary

    # Read file
    # This is all very hack-ish until readxl can incorporate the cellranger package
    calls <- readxl::read_excel(calls, sheet = 1, col_names = FALSE)[call_start:call_end, 1:7]
    names(calls) <- c("filename", "spp", "spp_perc", "group", "group_perc", "tot_pulses", "disc_prob")

    # Get rid of some random tabs retained in the BCID .xls output
    calls <- plyr::colwise(function(x) gsub("\t", "", x, fixed = TRUE))(calls)

    # Replace blanks (i.e., "") with NAs
    calls[calls == ""] <- NA

    calls <- set_col_types(calls, c(rep("character", 2), "numeric", "character", "numeric",
                                  "integer", "numeric"))

    # Restructure data
    # transform filename to call ID
    calls <- dplyr::mutate(calls,
                           call_id = filename %>% substr(5, 11) %>%
                               gsub("[.]", "", .) %>%
                                as.integer())

    if (gps) {
        ## Associate (join) location data from GPS with bat calls
        # However, rather than joining/merging, which require identical matches,
        # we associate every call with the closest (absolute) GPS fix and record
        # the difference in time (most will be 0; i.e., exact matches to the second)

        # First, calculate difference in time (sec) between call and all GPS fixes
        time_diffs <- outer(calls$call_id, GPS$call_id, "-")
        # Get index of the closest GPS fix
        nearest_fix <- apply(time_diffs, 1, function(x) which.min(abs(x)))
        GPS_diff <- diag(time_diffs[, nearest_fix])

        calls <- cbind(calls, dplyr::select(GPS, -call_id)[nearest_fix, ],
                       GPS_diff = GPS_diff, row.names = NULL) %>%
            dplyr::arrange(order) # Ensure ordered chronologically
    } else {
        calls <- cbind(calls, dplyr::select(GPS, -call_id), GPS_diff = NA,
                            row.names = NULL)
    }

    ## Rearranges columns for importGPS into MABM MS Access database (column order
    ## specified in import specification file 'calls1'
    if (for_import) {
        import <- with(calls,
                     data.frame(lat, lon, alt_m, date, time, call_id, filename,
                                spp, spp_perc, group, group_perc, tot_pulses, disc_prob,
                                call_id, order, GPS_diff, site_name = route_name))
        # Replace NAs with blanks to play nicely with conversion in Access
        # To do so requires the conversion of all fields to text
        # NOTE: `time` is the time of the GPS fix, not the detection
        import <- sapply(import, as.character)
        import[is.na(import)] <- ""
    }

    ## Create output directory
    out_name <- paste(route_name, route_date, sep = "_")
    out_dir <- paste0(in_dir, out_name)

    if (!dir.exists(out_dir)) {
        dir.create(out_dir)
    } else if (!overwrite) {
        stop("The output directory exists and you elected not to overwrite it.")
    }

    # Write final bat call data file with associated GPS information
    csv_name <- paste0("Calls_", out_name, "_final.csv")
    if (for_import) write.csv(import, file = file.path(dirname(in_dir), csv_name), quote = FALSE)

    if (gps) {
        ## Create point shapefile of all GPS locations (SavedRoute)
        GPS_spdf <- GPS
        coordinates(GPS_spdf) <- ~ lon + lat
        proj4string(GPS_spdf) <- CRS("+proj=longlat +datum=WGS84")
        name <- paste0("SavedRoute_", out_name)
        writeOGR(GPS_spdf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

        ## Create line shapefile of the route (RouteLine)
        GPS_sldf <- Line(coordinates(GPS_spdf)) %>% list() %>% Lines(., ID = "route") %>%
            list() %>% SpatialLines()
        df <- data.frame(id = "route")
        rownames(df) <- "route"
        GPS_sldf <- SpatialLinesDataFrame(GPS_sldf, df)
        proj4string(GPS_sldf) <- proj4string(GPS_spdf)
        name <- paste0("RouteLine_", out_name)
        writeOGR(GPS_sldf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

        ## Create point shapefile of locations with bat calls (Calls)
        calls_spdf <- calls
        coordinates(calls_spdf) <- ~ lon + lat
        proj4string(calls_spdf) <- proj4string(GPS_spdf)
        name <- paste0("Calls_", out_name)
        writeOGR(calls_spdf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

        message("The folder '", out_name, "' has been created in\n'",
                in_dir, "'\n", "and contains the following:\n\n",
                "Shapefiles:\n",
                paste(list.files(path = out_dir, pattern=".shp$"), collapse = "\n"), "\n")
        if (for_import)
            message(csv_name, " is ready for MABM database import in ", dirname(in_dir))
    } else {
        message("The folder '", out_name, "' has been created in\n'",
                in_dir, "'\n", "and contains the following:\n\n",
                "Shapefiles:\n",
                "No GPS data identified. No shapefiles created.\n")
        if (for_import)
            message(csv_name, " is ready for MABM database import in ", dirname(in_dir))
    }

    if (scrub) {
        # Get list of all call files in directory
        all_calls <- grep("[0-9]{2}\\#", dir(in_dir), value = TRUE)

        # Get good calls (from call ID file)
        good_calls <- calls[, c("filename", "spp")]

        # Filter good calls from all calls
        bad_calls <- all_calls[!(all_calls %in% good_calls[, 1])]

        # Checking necessity of scrubbing before doing it!
        if (length(all_calls) == 0) {
            message("\nScrubbing summary:\nNo Anabat files detected in directory.  Scrubbing ignored.")
        } else if (length(bad_calls) == 0) {
            message("\nScrubbing summary:\nNo suspected noise files in directory.  Scrubbing ignored.")
        } else { # Yay, we get to scrub!!!
            # Create scrubbed directory
            scrub_dir <- paste0(in_dir, "scrubbed")
            dir.create(scrub_dir)

            # Move likely noise files
            sapply(bad_calls, move, in_dir = in_dir, out_dir = scrub_dir)
            message("\nScrubbing summary:\nMoved ", length(bad_calls),
                    " suspected noise files to:\n'", scrub_dir, "'")
        }
    }

    if (plot) {
        p <- plot_MABM_route(gps = list.files(path = out_dir,
                                              pattern="SavedRoute.*\\.shp",
                                              full.names = TRUE))
        print(p)
    }

    if (keep_output) return(list(final_calls = calls, route_pt = GPS_spdf,
                                 route_line = GPS_sldf, call_pt = calls_spdf))
}
