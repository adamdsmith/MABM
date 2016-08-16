#' Audio file scrubbing expected noise files one or more nights of recording.
#'
#' This function uses the classification output from Bat Call Identification (BCID;
#'  \url{http::/www.batcallid.com}) software to scrub (move) suspected noise files into
#'  a new subdirectory.  This function
#'  is designed to handle multiple nights of recordings in the BCID output.
#'
#' @param mult_folder logical (default = `TRUE`) indicating whether Anabat files can be
#'  found in separate nightly folders (use `TRUE`) or in the top-level directory (i.e.,
#'  the same directory as the BCID .xls file; use 'FALSE')
#' @param BCID optional character string to specify the path to the BCID classification
#'  output file; default (`NULL`) allows the user to navigate to the file via a dialog box
#' @export

scrub_noise <- function(mult_folder = TRUE, BCID = NULL) {

    # Confirm that user to specified an appropriate route name
    if (is.null(BCID)) {
        ## Retrieve BCID file
        calls <- utils::choose.files(default = "*.xls",
                                     caption = "Select BCID output .xls file with bat call information.",
                                     multi = FALSE)
        if (length(calls) == 0) stop("Function cancelled.  No BCID output file selected.")
    } else {
        calls <- BCID
    }

    if (!file.exists(calls) | !is.character(calls))
        stop("The file does not exist or the path was specified incorrectly.  Try again.")

    # Extract file input directory
    trunc <- sapply(gregexpr("\\\\", calls), tail, 1)
    in_dir <- substr(calls, 1, trunc)

    ## Call .xls file read and formatting
    # Get start/end rows of relevant call information in BCID .xls file
    # Looks at first sheet in .xls file
    call_string <- do.call(paste0, readxl::read_excel(calls, sheet = 1, col_names = FALSE)) #concatenates all columns to simplify search
    call_starts <- grep("FILENAME", call_string) + 1 # Row after column headers; we have to make our own
    # This may need some modification in future if format changes...
    call_ends <- grep("IDENTIFICATION", call_string) - 2 # Two rows before identification summary

    # Accommodate nights with no "valid" calls
    valid <- call_ends >= call_starts
    call_starts <- call_starts[valid]
    call_ends <- call_ends[valid]

    if (all(length(call_starts) == 1, length(call_ends) == 1)) {
        keep <- seq(call_starts, call_ends)
    } else {
        keep <- do.call(c, mapply(seq, call_starts, call_ends))
    }

    # Read file
    # This is all very hack-ish until readxl can incorporate the cellranger package
    calls <- readxl::read_excel(calls, sheet = 1, col_names = FALSE)[keep, 1:8]
    names(calls) <- c("filename", "spp", "spp_perc", "group", "group_perc",
                      "tot_pulses", "disc_prob", "start_night")

    # Get rid of some random tabs retained in the BCID .xls output
    calls <- plyr::colwise(function(x) gsub("\t", "", x, fixed = TRUE))(calls)
    calls <- plyr::colwise(function(x) gsub("\n", "", x, fixed = TRUE))(calls)

    # Replace blanks (i.e., "") with NAs
    calls[calls == ""] <- NA

    calls <- set_col_types(calls, c(rep("character", 2), "numeric", "character", "numeric",
                                    "integer", "numeric", "character"))

    ## Create output (scrubbed) directory
    scrub_dir <- paste0(in_dir, "scrubbed")
    if (!dir.exists(scrub_dir)) dir.create(scrub_dir)

    nights <- unique(calls$start_night)

    cat("\nSCRUBBING SUMMARY:\n\n")

    if (mult_folder) {

        # Get all nights, including those with all noise based on BCID
        nights <- list.files(path = in_dir, pattern="^\\d{8}$")

        if (length(nights) == 0)
            stop("No nightly folders detected. Perhaps you meant to use `mult_folder = FALSE`?")

        # Go through the directories
        invisible(lapply(nights, function(x) {

            tmp_calls <- calls[calls$start_night == x, ]

            #Get list of all call files in directory
            all_calls <- grep("[0-9]{2}\\#", dir(paste0(in_dir, x)), value = TRUE)

            # Get good calls (from call ID file)
            good_calls <- tmp_calls[, "filename"]

            # Filter good calls from all calls
            bad_calls <- all_calls[!(all_calls %in% good_calls)]

            # Checking necessity of scrubbing before doing it!
            if (length(all_calls) == 0) {
                cat(x, "--", paste0("No Anabat files detected.\n",
                                    "Check the appropriateness of the `mult_folder` argument.\n",
                                    "Scrubbing ignored.\n\n"))
            } else if (length(bad_calls) == 0) {
                scrub_txt <- paste(x, "-- No suspected noise files in directory.  Scrubbing ignored.\n\n")
                cat(scrub_txt, file = paste0(scrub_dir, "/_scrubbing_report_",
                                             format(Sys.Date(), format = "%d_%b_%Y"), ".txt"),
                    append = TRUE)
                cat(scrub_txt)
            } else { # Yay, we get to scrub!!!
                # Move likely noise files
                sapply(bad_calls, move, in_dir = paste0(in_dir, x), out_dir = scrub_dir)
                scrub_txt <- paste(x, "-- Retained", length(good_calls), "call files; scrubbed",
                                   length(bad_calls), "suspected noise files.\n\n")
                cat(scrub_txt, file = paste0(scrub_dir, "/_scrubbing_report_",
                                             format(Sys.Date(), format = "%d_%b_%Y"), ".txt"),
                      append = TRUE)
                cat(scrub_txt)

            }
        }))

    } else {
        # Get list of all call files in directory
        all_calls <- grep("[0-9]{2}\\#", dir(in_dir), value = TRUE)

        # Get good calls (from call ID file)
        good_calls <- calls[, "filename"]

        # Filter good calls from all calls
        bad_calls <- all_calls[!(all_calls %in% good_calls)]

        # Checking necessity of scrubbing before doing it!
        if (length(all_calls) == 0) {
            cat(paste0("No Anabat files detected in directory.\n",
                      "Check the appropriateness of the `mult_folder` argument.\n",
                      "Scrubbing ignored.\n\n"))

        } else if (length(bad_calls) == 0) {
            scrub_txt <- paste("No suspected noise files in directory.  Scrubbing ignored.\n\n")
            cat(scrub_txt, file = paste0(in_dir, "/_scrubbing_report_",
                                         format(Sys.Date(), format = "%d_%b_%Y"), ".txt"))
            cat(scrub_txt)
        } else { # Yay, we get to scrub!!!
            # Move likely noise files
            sapply(bad_calls, move, in_dir = in_dir, out_dir = scrub_dir)
            scrub_txt <- paste("Retained", length(good_calls), "call files; scrubbed",
                               length(bad_calls), "suspected noise files.\n\n")
            cat(scrub_txt, file = paste0(scrub_dir, "/_scrubbing_report_",
                                         format(Sys.Date(), format = "%d_%b_%Y"), ".txt"))
            cat(scrub_txt)

        }
    }

    # Remove scrubbed folder if it wasn't used
    if (length(grep("[0-9]{2}\\#", dir(scrub_dir))) == 0) unlink(scrub_dir, recursive = TRUE)

}
