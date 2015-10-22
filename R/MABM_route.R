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
#' @param scrub logical indicating whether Anabat files (ending with '#'), if present, identified
#'  as noise (i.e., not assigned an ID in BCID) should be scrubbed (moved) to a newly created
#'  'scrubbed' subdirectory (default = TRUE); non-scrubbed files are not moved
#' @param route_name character string indicating the name used to file the created output.  Default (NULL)
#'  prompts the user to select from a list of all MABM routes.  This argument is particularly useful
#'  when processing data external to the MABM program (e.g., data collected along a non-MABM route)
#' @param for_import logical indicating whether the output *.csv file will be imported
#'  into the MABM Access database (default = TRUE).  See details.
#' @param keep_output logical (default = FALSE) that creates a list containing potentially
#'  useful outputs.
#' @import sp
#' @importFrom rgdal writeOGR
#' @export
MABM_route <- function(scrub = TRUE, route_name = NULL, for_import = TRUE, keep_output = FALSE) {

    # If keeping output (keep_output = TRUE), override for_import
    if (keep_output) for_import <- FALSE

    ## Prompt user to specify route name (site_Name in Access)
    # Load sites
    #sites <- read.csv("../Data/site.list.csv")
    sites <- read.csv(system.file("extdata", "site_list.csv", package = "MABM"), header = TRUE)
    menu_items <- sort(paste0(sites$Site, ": (", sites$Location, ")"))
    if (is.null(route_name)) {
        route_name <- tcltk::tk_select.list(menu_items, title="Choose the MABM route", multiple = FALSE)
        # Drop the full location
        route_name <- gsub(":.*$", "", route_name)
    }
    ## Retrieve GPS and BCID files
    ## Assumes BCID file is in same directory
    # GPS first
    gps <- tcltk::tk_choose.files(default = "*.txt",
                                  caption = "Select GPS text file.", multi = FALSE)

    # Extract file input directory
    trunc <- sapply(gregexpr("/", gps), tail, 1)
    in_dir <- substr(gps, 1, trunc)

    # Call file next
    calls <- tcltk::tk_choose.files(default = paste0(in_dir, "*.xls"),
                                    caption = "Select BCID output .xls file with bat call information.")

    ## Initial GPS text file read and division into columns
    # Get number of lines to skip at beginning of GPS files
    # Looks for line that contains location headers
    n_skip <- grep("Latitude", readr::read_lines(gps))

    # Assign variable types and names; drop first and last columns
    gps <- readr::read_fwf(gps, readr::fwf_empty(gps, skip = n_skip),
                           col_types = "_ccncc_", skip = n_skip)
    names(gps) <- c("lat", "lon", "alt_m", "date", "time")

    # Restructure data
#    gps <- gps %>% mutate(lat = as.numeric(substring(lat, 2)),
#                          lon = ifelse(substring(lon, 1, 1) == "W",
#                                       as.numeric(substring(lon, 2)) * -1,
#                                       as.numeric(substring(lon, 2))),
#                          date = lubridate::ymd(date),
#                          dt = lubridate::ymd_hms(paste(date, time)),
#                          call_id = gsub(":", "", time)) %>%
#        arrange(dt) %>% # Sort chronologically
#        mutate(order = 1:nrow(gps)) # Add order variable to facilitate QA/QC

    # GPS quality control
    # Sometimes GPS logs an incorrect date, although the time is correct
    gps <- gps_QC(gps)

    # Use until new dplyr is released
    gps <- plyr::ddply(gps, plyr::.(date), plyr::mutate, # Doesn't like dplyr's mutate
                       lat = as.numeric(substring(lat, 2)),
                       lon = ifelse(substring(lon, 1, 1) == "W",
                                    as.numeric(substring(lon, 2)) * -1,
                                    as.numeric(substring(lon, 2))),
                       date = lubridate::ymd(date),
                       dt = lubridate::ymd_hms(paste(date, time)),
                       call_id = as.integer(gsub(":", "", time)))
    gps <- dplyr::arrange(gps, dt)
    gps$order <- 1:nrow(gps)

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

    ## Associate (join) location data from GPS with bat calls
    # However, rather than joining/merging, which require identical matches,
    # we associate every call with the closest (absolute) GPS fix and record
    # the difference in time (most will be 0; i.e., exact matches to the second)

    # First, calculate difference in time (sec) between call and all GPS fixes
    time_diffs <- outer(calls$call_id, gps$call_id, "-")
    # Get index of the closest GPS fix
    nearest_fix <- apply(time_diffs, 1, function(x) which.min(abs(x)))
    gps_diff <- diag(time_diffs[, nearest_fix])

    calls <- cbind(calls, dplyr::select(gps, -call_id)[nearest_fix, ], GPS_diff = gps_diff) %>%
        dplyr::arrange(order) # Ensure ordered chronologically

    ## Rearranges columns for import into MABM MS Access database (column order
    ## specified in import specification file 'calls1'
    if (for_import) {
        import <- with(calls,
                     data.frame(lat, lon, alt_m, date, time, call_id, filename,
                                spp, spp_perc, group, group_perc, tot_pulses, disc_prob,
                                call_id, order, gps_diff, site_name = route_name))
        # Replace NAs with blanks to play nicely with conversion in Access
        # To do so requires the conversion of all fields to text
        # NOTE: `time` is the time of the GPS fix, not the detection
        import <- sapply(import, as.character)
        import[is.na(import)] <- ""
    }

    ## Create output directory
    route_date <- min(gps$date) %>% as.character() %>% gsub("-", "", .)
    out_name <- paste(route_name, route_date, sep = "_")
    out_dir <- paste0(in_dir, out_name)
    dir.create(out_dir)

    # Write final bat call data file with associated GPS information
    name <- paste0("Calls_", out_name, "_final.csv")
    write.csv(import, file = paste(out_dir, name, sep = "/"), quote = FALSE)

    ## Create point shapefile of all GPS locations (SavedRoute)
    gps_spdf <- gps
    coordinates(gps_spdf) <- ~ lon + lat
    proj4string(gps_spdf) <- CRS("+proj=longlat +datum=WGS84")
    name <- paste0("SavedRoute_", out_name)
    writeOGR(gps_spdf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

    ## Create line shapefile of the route (RouteLine)
    gps_sldf <- Line(coordinates(gps_spdf)) %>% list() %>% Lines(., ID = "route") %>%
        list() %>% SpatialLines()
    df <- data.frame(id = "route")
    rownames(df) <- "route"
    gps_sldf <- SpatialLinesDataFrame(gps_sldf, df)
    proj4string(gps_sldf) <- proj4string(gps_spdf)
    name <- paste0("RouteLine_", out_name)
    writeOGR(gps_sldf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

    ## Create point shapefile of locations with bat calls (Calls)
    calls_spdf <- calls
    coordinates(calls_spdf) <- ~ lon + lat
    proj4string(calls_spdf) <- proj4string(gps_spdf)
    name <- paste0("Calls_", out_name)
    writeOGR(calls_spdf, out_dir, name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

    cat(paste0("The folder '", out_name, "' has been created in\n'",
               in_dir, "'\n", "and contains the following:\n\n",
               "Shapefiles:\n",
               paste(list.files(path = out_dir, pattern=".shp$"), collapse = "\n"),
               "\n\nText files:\n",
               list.files(path = out_dir, pattern=".csv$")))

    if (scrub) {
        # Get list of all call files in directory
        all_calls <- grep("[0-9]{2}\\#", dir(in_dir), value = TRUE)

        # Get good calls (from call ID file)
        good_calls <- calls[, c("filename", "spp")]

        # Filter good calls from all calls
        bad_calls <- all_calls[!(all_calls %in% good_calls[, 1])]

        # Checking necessity of scrubbing before doing it!
        if (length(all_calls) == 0) {
            cat("\n\nScrubbing summary:\nNo Anabat files detected in directory.  Scrubbing ignored.")
        } else if (length(bad_calls) == 0) {
            cat("\n\nScrubbing summary:\nNo suspected noise files in directory.  Scrubbing ignored.")
        } else { # Yay, we get to scrub!!!
            # Create scrubbed directory
            scrub_dir <- paste0(in_dir, "scrubbed")
            dir.create(scrub_dir)

            # Move likely noise files
            sapply(bad_calls, move, in_dir = in_dir, out_dir = scrub_dir)
            cat(paste0("\n\nScrubbing summary:\nMoved ", length(bad_calls), " suspected noise files to:\n'",
                       scrub_dir, "'"))
        }
    }

    if (keep_output) return(list(final_calls = calls, route_pt = gps_spdf,
                                 route_line = gps_sldf, call_pt = calls_spdf))
}
