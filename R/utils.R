inst_pkg <- function(pkgs) {
    pkg_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkg_miss) > 0) install.packages(pkg_miss)
}

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

gps_QC <- function(gps) {

    # Create temporary data frame
    tmp <- data.frame(date = strptime(gps$date, format = "%Y-%m-%d"),
                      time = lubridate::hms(gps$time))
    tmp$start_date <- tmp$date - lubridate::days(ifelse(lubridate::hour(tmp$time) < 13, 1, 0))

    ## Check for multiple (valid) start nights in GPS file...
    # If so, prompt user to make a decision (bad GPS dates, i.e., 1969-12-31)
    # are ignored at this point.
    start_dates <- unique(tmp$start_date)
    start_dates <- start_dates[!grepl(c("1969|1970"), as.character(start_dates))]

    if (length(start_dates) > 1) {
        use_date <- utils::select.list(start_dates,
                                       title="\nMultiple start dates detected in GPS text file.  Choose one.",
                                       multiple = FALSE)
        keep_range <- range(which(tmp$start_date == use_date))
        tmp <- tmp[keep_range[1]:keep_range[2],]
        gps <- gps[keep_range[1]:keep_range[2],]
    }

    # Assume most fixes are correct, so we can get correct year from modal date
    surv_year <- Mode(lubridate::year(tmp$date))

    # Which rows are valid (i.e., collected in modal survey year)
    good_dates <- which(lubridate::year(tmp$date) == surv_year)

    # If survey spanned two dates (i.e., went past midnight), extract start date (minimum)
    start_date <- min(tmp[good_dates,]$date)

    # Fix records with bad date (i.e., did not occur during modal year)
    fix_dates <- gps[-good_dates, ]
    fix_dates <- mutate(fix_dates,
                               date = ifelse(as.numeric(substr(time, 1, 2)) >= 12,
                                             as.character(start_date),
                                             as.character(start_date + lubridate::days(1))))

    # Now, put them back like a good guest
    gps[-good_dates, ] <- fix_dates

    # Expunge rows with missing lat/lon, time, or date info
    # Should capture extra blank lines in the GPS file as well
    anyNA <- apply(gps[, c("lat", "lon", "date", "time")], 1, function(row) any(is.na(row)))
    gps <- gps[!anyNA, ]

    return(gps)

}

makeBatIconList <- function(w = 38, h = 21, anchX = 20, anchY = 20) {
    iconList(CORA = makeIcon(system.file("icons", "Corynorhinus.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             COTO = makeIcon(system.file("icons", "Corynorhinus.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             EPFU = makeIcon(system.file("icons", "EPFU.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             LABO = makeIcon(system.file("icons", "LABO.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             LACI = makeIcon(system.file("icons", "LACI_LANO.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             LANO = makeIcon(system.file("icons", "LACI_LANO.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYAU = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYGR = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYLE = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYLU = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYSE = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             MYSO = makeIcon(system.file("icons", "Myotis.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             NYHU = makeIcon(system.file("icons", "NYHU.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             PESU = makeIcon(system.file("icons", "PESU.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY),
             UNKN = makeIcon(system.file("icons", "UNKN.png", package = "MABM"),
                             iconWidth = w, iconHeight = h,
                             iconAnchorX = anchX, iconAnchorY = anchY))
}

set_col_types <- function(obj, types){
    obj <- as.data.frame(obj)
    for (i in 1:length(obj)){
        FUN <- switch(types[i],
                      character = as.character,
                      numeric = as.numeric,
                      integer = as.integer)
        obj[,i] <- FUN(obj[,i])
    }
    obj
}

move <- function(file, in_dir, out_dir) {
    file.rename(paste(in_dir, file, sep = "/"), paste(out_dir, file, sep = "/"))
}

sequence <- function(start, end) {
    mat <- cbind(start, end)
    unlist(apply(mat, 1, function(x) seq(x[1], x[2], 1)))
}

clean_tabs <- function(x) gsub("[\t\n]", "", x)
yesno <- function() {
    ans <- substr(readline(prompt="Would you like to set a MABM root directory (y/n)?"), 1L, 1L)
    return(tolower(ans))
