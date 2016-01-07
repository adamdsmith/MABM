#' Compare bat call classification between EchoClass 3.0 and BCID 2.7
#'
#' This function facilitates the comparison of bat call classification between EchoClass and
#' BCID software.  It prompts the user to identify the output .xls files from EchoClass 3.0
#' and BCID 2.7, assuming they result from the classification of the same suite of Anabat call
#' files.  It outputs a new .xls file ("ComEchoBCID") containing the comparison, sorted by
#' classification agreement, to the same directory as the EchoClass and BCID inputs.
#'
#' This function can be used to compare calls recorded over multiple nights.  In these cases,
#' BCID treats each calendar date separately, resulting in a slightly more complicated
#' spreadsheet.  EchoClass avoids this complication by producing a single list of call
#' classifications, sorted by date, in its .xls output.  For scrubbing call files associated
#' with a single or multiple nights of recording, see \code{\link{scrub_noise}}.
#'
#' @export

id_compare <- function() {

    EC_spp = BCID_spp = agree = filename = NULL # Variable "declaration" for R CMD check

    inst_pkg("xlsx")

    ## EchoClass .xls file read and formatting
    ECcalls <- tcltk::tk_choose.files(default = "*.xls",
                                      caption = "Select EchoClass output .xls file with bat call information.")

    # Get start/end rows of relevant call information in EchoClass .xls file
    EC_string <- do.call(paste0, readxl::read_excel(ECcalls, sheet = 1, col_names = FALSE))
    EC_start <- grep("File Name", EC_string) + 1 # Row after column headers; we have to make our own
    EC_end <- length(EC_string)

    # Read relevant chunks of .xls file
    # This is all very hack-ish until readxl can incorporate the cellranger package
    EC_calls <- readxl::read_excel(ECcalls, sheet = 1)[EC_start:EC_end, ]
    # Get rid of blanks in variable names
    names(EC_calls) <- gsub(" ", "_", names(EC_calls))
    EC_calls <- with(EC_calls, data.frame(filename = File_Name, EC_spp = Prominent_Species,
                                          stringsAsFactors = FALSE))

    ## Remove EchoClass noise files
    EC_calls <- subset(EC_calls, EC_spp != "Noise")
    # Replace "Unknown" with "UNKN" to match BCID
    EC_calls$EC_spp <- gsub("Unknown", "UNKN", EC_calls$EC_spp)

    # Extract EC file input directory (assumes BCID is here too)
    trunc <- sapply(gregexpr("/", ECcalls), tail, 1)
    in_dir <- substr(ECcalls, 1, trunc)

    ## BCID .xls file read and formatting
    BCIDcalls <- tcltk::tk_choose.files(default = paste0(in_dir, "*.xls"),
                                        caption = "Select BCID output .xls file with bat call information.")

    # Get start/end rows of relevant call information in BCID .xls file
    BCID_string <- do.call(paste0, readxl::read_excel(BCIDcalls, sheet = 1, col_names = FALSE)) #concatenates all columns to simplify search
    BCID_start <- grep("FILENAME", BCID_string) + 1 # Row after column headers; we have to make our own
    # This may need some modification in future if format changes...
    BCID_end <- grep("IDENTIFICATION", BCID_string) - 2 # Two rows before identification summary
    keep <- sequence(BCID_start, BCID_end)

    # Read relevant chunks of .xls file
    # This is all very hack-ish until readxl can incorporate the cellranger package
    BCID_calls <- readxl::read_excel(BCIDcalls, sheet = 1, col_names = FALSE)[keep, 1:2]
    names(BCID_calls) <- c("filename", "BCID_spp")

    # Get rid of some random retained tab characters
    BCID_calls <- plyr::colwise(function(x) gsub("\t", "", x, fixed = TRUE))(BCID_calls)


    ## Join BCID with EchoClass
    comparison <- dplyr::left_join(EC_calls, BCID_calls)

    ## Classifications match?
    comparison$agree <- comparison$EC_spp == comparison$BCID_spp

    ## Sort them according to DR's specifications
    comparison <- dplyr::arrange(comparison, -agree, EC_spp, BCID_spp, filename)


    ## Save it
    xlsx::write.xlsx(comparison, paste0(in_dir, "ComEchoBCID.xls"),
                     sheetName = "EchoClass vs. BCID", row.names=FALSE)

}
