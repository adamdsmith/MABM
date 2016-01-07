#' Audio file scrubbing expected noise files one or more nights of recording.
#'
#' This function uses the classification output from Bat Call Identification (BCID;
#'  \url{http::/www.batcallid.com}) software to scrub (move) suspected noise files into
#'  a new subdirectory.  This function
#'  is designed to handle multiple nights of recordings in the BCID output.
#'
#' @param calls optional character string to specify the path to the BCID classification
#'  output file; default (`NULL`) allows the user to navigate to the file via a dialog box
#' @export

scrub_noise <- function(calls = NULL) {

    # Confirm that user to specified an appropriate route name
    if (is.null(calls)) {
        ## Retrieve BCID file
        calls <- tcltk::tk_choose.files(default = "*.xls",
                                        caption = "Select BCID output .xls file with bat call information.", multi = FALSE)
    }

    if (!file.exists(calls) | !is.character(calls))
        stop("The file does not exist or the path was specified incorrectly.  Try again.")

    # Extract file input directory
    trunc <- sapply(gregexpr("/", calls), tail, 1)
    in_dir <- substr(calls, 1, trunc)

    ## Call .xls file read and formatting
    # Get start/end rows of relevant call information in BCID .xls file
    # Looks at first sheet in .xls file
    call_string <- do.call(paste0, readxl::read_excel(calls, sheet = 1, col_names = FALSE)) #concatenates all columns to simplify search
    call_starts <- grep("FILENAME", call_string) + 1 # Row after column headers; we have to make our own
    # This may need some modification in future if format changes...
    call_ends <- grep("IDENTIFICATION", call_string) - 2 # Two rows before identification summary
    keep <- sequence(call_starts, call_ends)
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
    dir.create(scrub_dir)

    nights <- unique(calls$start_night)

    cat("\nSCRUBBING SUMMARY:\n\n")
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
            cat(x, "-- No Anabat files detected in directory.  Scrubbing ignored.\n\n")
        } else if (length(bad_calls) == 0) {
            cat(x, "-- No suspected noise files in directory.  Scrubbing ignored.\n\n")
        } else { # Yay, we get to scrub!!!
            # Move likely noise files
            sapply(bad_calls, move, in_dir = paste0(in_dir, x), out_dir = scrub_dir)
            cat(paste0(x, " -- Moved ", length(bad_calls), " suspected noise files to:\n'",
                       scrub_dir, "'\n\n"))
        }

    }))

}
