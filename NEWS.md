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
