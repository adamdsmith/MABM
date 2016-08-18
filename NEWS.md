# MABM 0.5.3

* Addressed a few rare issues related to the format of the GPS text file, including multiple starts/stops within the file (#6) and multiple nights (#7).
* Fix bug that reset time elapsed since start of route if the survey spanned midnight.
* Changed bat icons (#8) that display in `plot_MABM_route()`; they're much prettier now. :)
* When calling `MABM_route`, changed order of file selection.  BCID classification file is now selected first and `gps.txt` in same directory is used by default, if found.  If not, user is prompted to select appropriate GPS file. (#9)
* Changed from `tcltk` to `utils` for dialog boxes as the `tcltk` boxes sometimes got 'trapped' in the background (#5)

# MABM 0.5.2

* Added argument `mult_folder` to `scrub_noise` to accommodate typically different file structure between single night and multiple nights of recording, see `?scrub_noise`.

# MABM 0.5.1

* Changed function `passive_scrub` to `scrub_noise` as it works generally with data collected from mobile, active, or passive recording.

# MABM 0.5

* Added function (`passive_scrub`) use the classification output from Bat Call Identification (BCID) software to scrub suspected noise files into a new subdirectory.  This function can accomodated multiple nights of recording in the BCID output. (#4)

# MABM 0.4.1

* Allow specification of custom `route_name` argument in `MABM_route`.

# MABM 0.4

* Add function `id_compare` to facilitate the comparison of bat call classification between EchoClass and BCID software. (#3)

# MABM 0.3

* Add option in `MABM_route` to scrub Anabat call files identified as noise by BCID (based on user paramters) into a separate directory. (#1)

# MABM 0.2

* Large overhaul of functionality of `plot_MABM_route`.
