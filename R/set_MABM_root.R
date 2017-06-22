#' Interactively set the root directory of all MABM data
#'
#' This function allows you to specify the root directory containing all MABM routes and
#'  associated data, significantly easing the navigation burden when processing BCID output
#'  using `MABM_route`.  Set this once at the beginning of a session (see example).
#'
#' @param dir character string indicating the base directory containing MABM station
#'  related data (i.e., MABM route directories with call files, shapefiles, and annual reports).
#' @export
#' @examples
#' # Browse to specify MABM root directory
#' set_MABM_root()
#'
#' # Speficy MABM root directory directly
#' set_MABM_root("C:/MABM")

set_MABM_root <- function(dir = NULL) {
    if (is.null(dir)) {
        dir <- choose.dir("C:/", caption = "Select MABM root folder.")
        if (is.na(dir)) stop("No MABM root directory identified.")
        options(MABM_home = dir)
    } else {
        if (!dir.exists(dir)) stop("The specified directory does not exist.")
        options(MABM_home = normalizePath(dir))
    }
}
