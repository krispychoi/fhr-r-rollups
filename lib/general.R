#######################################################################
###  
###  Functions for working with other parts of the FHR payload, 
###  not covered in other lib files.
###  
#######################################################################


## Retrieves the sequence of valid version updates from the supplied FHR 
## days list.
## 
## Returns a vector mapping dates that saw version updates to the full version
## number updated to on that day, ordered chronologically. 
## If there were multiple updates on the day, this is the last update on the day. 
## 
## If no valid updates were found across the input days, returns NULL.
updateValues <- function(days) {
    if(length(days) == 0) return(NULL)
    updates <- unlist(lapply(days, function(d) {
        vu <- d$org.mozilla.appInfo.versions
        ## Handle different field versions. 
        vu <- if(identical(vu[["_v"]], 1)) vu$version else vu$appVersion
        if(length(vu) == 0) return(NULL)
        ## If there are multiple versions, use the latest one. 
        if(length(vu) > 1) vu <- vu[[length(vu)]]
        ## Simple validity check.
        if(is.na(vu) || !grepl("^(\\d+)\\.", vu)) return(NULL)
        vu
    }))
    if(length(updates) == 0) return(NULL)
    updates[order(names(updates))]
}


## Determines which browser version the profile was on on each date.
## 
## Input is a vector of dates to tag, a vector in the format 
## returned by updateValues(), mapping update dates to versions updated to, 
## and the current version. 
##
## Returns a named vector the same length mapping each date to the browser 
## version in effect on that date. Assumptions are as follows:
## - update days are assigned the new version 
## - dates on which the version could not be determined, in particular those 
##   prior to the first recorded update, are assigned NA
## - a profile is assumed to stay on a version until the next update date
## - if no updates are recorded in the profile, every date is assumed to be
##   on the current version. Otherwise, the current version is ignored.
versionOnDate <- function(dates, updates, currentversion) {
    if(length(dates) == 0) return(NULL)
    ## If no updates are present, all days are on the current version, if any. 
    vers <- if(length(updates) == 0) {
        if(identical(currentversion, "missing")) currentversion <- NA
        rep(if(identical(currentversion, "missing")) NA else currentversion, 
            length(dates))
    } else {
        ## Otherwise assign versions to dates based on updates as cut points.
        ## Add an artificial final date past the range so that the final group 
        ## gets assigned.
        bks <- c(as.Date(names(updates)), as.Date(max(dates)) + 1)
        labs <- as.vector(updates)
        as.character(cut(as.Date(dates), bks, labs))
    }
    setNames(vers, dates)
}

