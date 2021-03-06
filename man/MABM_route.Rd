% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MABM_route.R
\name{MABM_route}
\alias{MABM_route}
\title{MABM shapefile creation, preparation for database import, and audio file scrubbing.}
\usage{
MABM_route(route_name = NULL, scrub = TRUE, gps = TRUE,
  for_import = TRUE, keep_output = FALSE, overwrite = FALSE,
  plot = FALSE)
}
\arguments{
\item{route_name}{character string indicating the name used to file the created output.  Default (NULL)
prompts the user to select from a list of all MABM routes.  This argument is useful primarily
when processing data external to the MABM program (e.g., data collected along a non-MABM route)}

\item{scrub}{logical indicating whether Anabat files (ending with '#'), if present, identified
as noise (i.e., not assigned an ID in BCID) should be scrubbed (moved) to a newly created
'scrubbed' subdirectory (default = TRUE); non-scrubbed files are not moved}

\item{gps}{logical (default = TRUE) indicating whether the a GPS file is available to georeference
the BCID classification file}

\item{for_import}{logical indicating whether the output *.csv file should be formatted for import
into the MABM Access database (default = TRUE).  See details.}

\item{keep_output}{logical (default = FALSE) that creates a list containing potentially
useful outputs.}

\item{overwrite}{logical (default = FALSE) indicating whether to overwrite the output
directory and files, if they exist.}

\item{plot}{logical (default = FALSE) indicating whether to plot the processed route using
\code{\link{plot_MABM_route}}}
}
\description{
This function associates bat calls classified with Bat Call Identification (BCID;
\url{http::/www.batcallid.com}) software with simultaneously collected geographic
location information.  For this application, bat calls were collected with (usually) an
Anabat SD2 or the older Anabat SD1 (\url{https://www.titley-scientific.com}) and
georeference by integrating the Anabat data logger, PDA and an external GPS.  From this
association, it also creates multiple shapefiles and a *.csv for incorporation into the
MABM Access database.  If requested, suspected noise files can be scrubbed (moved) to a
new subdirectory.
}
\details{
If \code{for_import = TRUE}, the function will rearrange the columns to match the expected
 import format of the MABM database and replace \code{NA}s with blanks to play nicely with
 expected variable types.  This necessitates conversion of most (all?) columns to text
 strings (which is not very handy if you want to work with the output in R).  This argument
 is forced to \code{for_import = FALSE} if the user specifies \code{keep_output = TRUE}.

If \code{keep_output = TRUE}, the function creates a list containing (1) a \code{data.frame}
 ('final_calls') containing information on the bat detections, (2) a
 \code{\link[sf]{sf}} ('route_pt') of all GPS fixes along the route, (3) a
 \code{\link[sf]{sf}} of these GPS fixes, and (4) a
 \code{\link[sf]{sf}} ('call_pt') of all bat detections. This is handy if the
 user wants to conduct further investigations in R. To access this output, remember to
 assign the function output to an R object.
}
