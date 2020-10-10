# Test script localy

wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'

wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])

library(rgdal)
library(downloader)
library(ggplot2)
library(mapdata)
library(geojsonio)
library(ggmap)
library(ggrepel)

# EMIS-R Libraries

library("rasterVis")
library("XML")
library("RCurl")
library("bitops")
library("lattice")
library("latticeExtra")
#library("RColorBrewer")
library("maps")
library("maptools")
library("wq")
#library("xtable")
library("zoo")
#library("jsonlite")
#require(xtable)

# Script for Wekeo environment
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

bbox<-paste(xmin,ymin,xmax,ymax,sep=",")

#FUNCTION DATA EXTRACTION
#=========================================================================

mpaextract<-function (name = "EMIS_A_CHLA", resolution = "4km", gmis_wcst_url = gmis_wcst_url, startdate = "2005-09", enddate = "2005-10", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{

        xmin <- extent(mpa)@xmin - 0.1
        xmax <- extent(mpa)@xmax + 0.1
        ymin <- extent(mpa)@ymin - 0.1
        ymax <- extent(mpa)@ymax + 0.1
        imgs <- NA


        imgs <- getgmisdataseries(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url,
                startdate = startdate, enddate = enddate, xmin, xmax, ymin, ymax)

        return(imgs)
}


#FUNCTION getGMISdataseries
#=========================================================================

getgmisdataseries<-function (name = "GMIS_A_CHLA", resolution = "4km", gmis_wcst_url = gmis_wcst_url, startdate = "2005-09", enddate = "2005-10", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{
    checkparameter <- data.frame(name = FALSE, resolution = FALSE, 
        startdate = FALSE, enddate = FALSE, bbox = FALSE)

        checkparameter$resolution <- TRUE

	data_gmis<-read_gmis_wcst_data(resolution, gmis_wcst_url)

        idvar <- grep(name, data_gmis$shortname)
        if (length(idvar) == 0) {
            checkparameter$name <- FALSE
            print("Variable name do not exist on GMIS")
        }
        else {
            checkparameter$name <- TRUE
        }


    n0 <- nchar(startdate) == 7
    n1 <- substr(startdate, 1, 1) %in% 1:2
    n2 <- substr(startdate, 2, 2) %in% c(9, 0)
    n3 <- substr(startdate, 3, 3) %in% 0:9
    n4 <- substr(startdate, 4, 4) %in% 0:9
    n5 <- substr(startdate, 5, 5) == "-"
    n6 <- substr(startdate, 6, 6) %in% c(0, 1)
    n7 <- substr(startdate, 7, 7) %in% 0:9
    if (!(n0 & n1 & n2 & n3 & n4 & n5 & n6 & n7)) {
        checkparameter$startdate <- FALSE
        print("Start date format is not correct (YYYY-MM)")
    }
    else {
        checkparameter$startdate <- TRUE
    }
    n0 <- nchar(enddate) == 7
    n1 <- substr(enddate, 1, 1) %in% 1:2
    n2 <- substr(enddate, 2, 2) %in% c(9, 0)
    n3 <- substr(enddate, 3, 3) %in% 0:9
    n4 <- substr(enddate, 4, 4) %in% 0:9
    n5 <- substr(enddate, 5, 5) == "-"
    n6 <- substr(enddate, 6, 6) %in% c(0, 1)
    n7 <- substr(enddate, 7, 7) %in% 0:9
    if (!(n0 & n1 & n2 & n3 & n4 & n5 & n6 & n7)) {
        checkparameter$enddate <- FALSE
        print("End date format is not correct (YYYY-MM)")
    }
    else {
        checkparameter$enddate <- TRUE
    }
    n1 <- is.numeric(xmin)
    n2 <- length(xmin) == 1
    n3 <- is.numeric(xmax)
    n4 <- length(xmax) == 1
    n5 <- is.numeric(ymin)
    n6 <- length(ymin) == 1
    n7 <- is.numeric(ymax)
    n8 <- length(ymax) == 1
    n9 <- xmin < xmax
    n10 <- ymin < ymax
    if (!(n1 & n2 & n3 & n4 & n5 & n6 & n7 & n8)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not correct (1 numeric vector for xmin, xmax, ymin and ymax)")
    }
    else {
        checkparameter$bbox <- TRUE
    }
    if (!(n9 & n10)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not ordered correctly (xmin<xmax AND ymin<ymax)")
    }
    if (apply(checkparameter, 1, sum) == 5) {
        mindate <- strptime(paste(data_gmis$startdate[idvar], 
            "15", sep = "-"), "%Y-%m-%d")
        maxdate <- strptime(paste(data_gmis$enddate[idvar], "15", 
            sep = "-"), "%Y-%m-%d")
        askedstartdate <- strptime(paste(startdate, "15", sep = "-"), 
            "%Y-%m-%d")
        if ((mindate <= askedstartdate) & (askedstartdate <= 
            maxdate)) {
            checkparameter$startdate <- TRUE
        }
        else {
            checkparameter$startdate <- FALSE
            print(paste(name, "is not available on GMIS for the", 
                askedstartdate))
        }
        askedenddate <- strptime(paste(enddate, "15", sep = "-"), 
            "%Y-%m-%d")
        if ((mindate <= askedenddate) & (askedenddate <= maxdate)) {
            checkparameter$enddate <- TRUE
        }
        else {
            checkparameter$enddate <- FALSE
            print(paste(name, "is not available on GMIS for the", 
                askedenddate))
        }
        if ((askedenddate >= askedstartdate)) {
            checkparameter$enddate <- TRUE
        }
        else {
            checkparameter$enddate <- FALSE
            print(paste(startdate, "is older than", enddate))
        }
        bboxgmis <- unlist(strsplit(data_gmis$bbox[idvar], " "))
        xmingmis <- as.numeric(bboxgmis[1])
        xmaxgmis <- as.numeric(bboxgmis[3])
        ymingmis <- as.numeric(bboxgmis[2])
        ymaxgmis <- as.numeric(bboxgmis[4])
        n1 <- xmingmis <= xmin
        n2 <- xmax <= xmaxgmis
        n3 <- ymingmis <= ymin
        n4 <- ymax <= ymaxgmis
        if (n1 & n2 & n3 & n4) {
            checkparameter$bbox <- TRUE
        }
        else {
            checkparameter$bbox <- FALSE
            print(paste("The selected area is not strickly inside the spatial extent of", 
                name, "in gmis"))
        }
    }
    if (apply(checkparameter, 1, sum) == 5) {
        startyear <- as.numeric(substr(startdate, 1, 4))
        startmonth <- as.numeric(substr(startdate, 6, 7))
        endyear <- as.numeric(substr(enddate, 1, 4))
        endmonth <- as.numeric(substr(enddate, 6, 7))
        if (startyear < endyear) {
            timeindex1 <- paste(startyear, sprintf("%02d", startmonth:12), 
                sep = "-")
            timelist <- expand.grid(1:12, (startyear + 1):(endyear - 
                1))
            timeindex2 <- paste(timelist[, 2], sprintf("%02d", 
                timelist[, 1]), sep = "-")
            timeindex3 <- paste(endyear, sprintf("%02d", 1:endmonth), 
                sep = "-")
            if ((endyear - startyear) == 1) {
                timeindex <- c(timeindex1, timeindex3)
            }
            else {
                timeindex <- c(timeindex1, timeindex2, timeindex3)
            }
        }
        else {
            timeindex <- paste(startyear, sprintf("%02d", startmonth:endmonth), 
                sep = "-")
        }
        pb <- txtProgressBar(min = 0, max = length(timeindex))
        for (i in 1:length(timeindex)) {
            if (i <= 1) {
                imgs <- getgmisdata(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url,
                  date = timeindex[i], xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax)
                names(imgs) <- timeindex[i]
            }
            else {
                img <- getgmisdata(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url,
                  date = timeindex[i], xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax)
                names(img) <- timeindex[i]
                imgs <- stack(imgs, img)
            }
            setTxtProgressBar(pb, i)
        }
        close(pb)
        return(imgs)
    }
}


#FUNCTION getGMISdataseries
#=========================================================================

getgmisdataseries<-function (name = "GMIS_A_CHLA", resolution = "4km", gmis_wcst_url = gmis_wcst_url, startdate = "2005-09", enddate = "2005-10", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{
    checkparameter <- data.frame(name = FALSE, resolution = FALSE, 
        startdate = FALSE, enddate = FALSE, bbox = FALSE)

        checkparameter$resolution <- TRUE

	data_gmis<-read_gmis_wcst_data(resolution, gmis_wcst_url)

        idvar <- grep(name, data_gmis$shortname)
        if (length(idvar) == 0) {
            checkparameter$name <- FALSE
            print("Variable name do not exist on GMIS")
        }
        else {
            checkparameter$name <- TRUE
        }


    n0 <- nchar(startdate) == 7
    n1 <- substr(startdate, 1, 1) %in% 1:2
    n2 <- substr(startdate, 2, 2) %in% c(9, 0)
    n3 <- substr(startdate, 3, 3) %in% 0:9
    n4 <- substr(startdate, 4, 4) %in% 0:9
    n5 <- substr(startdate, 5, 5) == "-"
    n6 <- substr(startdate, 6, 6) %in% c(0, 1)
    n7 <- substr(startdate, 7, 7) %in% 0:9
    if (!(n0 & n1 & n2 & n3 & n4 & n5 & n6 & n7)) {
        checkparameter$startdate <- FALSE
        print("Start date format is not correct (YYYY-MM)")
    }
    else {
        checkparameter$startdate <- TRUE
    }
    n0 <- nchar(enddate) == 7
    n1 <- substr(enddate, 1, 1) %in% 1:2
    n2 <- substr(enddate, 2, 2) %in% c(9, 0)
    n3 <- substr(enddate, 3, 3) %in% 0:9
    n4 <- substr(enddate, 4, 4) %in% 0:9
    n5 <- substr(enddate, 5, 5) == "-"
    n6 <- substr(enddate, 6, 6) %in% c(0, 1)
    n7 <- substr(enddate, 7, 7) %in% 0:9
    if (!(n0 & n1 & n2 & n3 & n4 & n5 & n6 & n7)) {
        checkparameter$enddate <- FALSE
        print("End date format is not correct (YYYY-MM)")
    }
    else {
        checkparameter$enddate <- TRUE
    }
    n1 <- is.numeric(xmin)
    n2 <- length(xmin) == 1
    n3 <- is.numeric(xmax)
    n4 <- length(xmax) == 1
    n5 <- is.numeric(ymin)
    n6 <- length(ymin) == 1
    n7 <- is.numeric(ymax)
    n8 <- length(ymax) == 1
    n9 <- xmin < xmax
    n10 <- ymin < ymax
    if (!(n1 & n2 & n3 & n4 & n5 & n6 & n7 & n8)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not correct (1 numeric vector for xmin, xmax, ymin and ymax)")
    }
    else {
        checkparameter$bbox <- TRUE
    }
    if (!(n9 & n10)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not ordered correctly (xmin<xmax AND ymin<ymax)")
    }
    if (apply(checkparameter, 1, sum) == 5) {
        mindate <- strptime(paste(data_gmis$startdate[idvar], 
            "15", sep = "-"), "%Y-%m-%d")
        maxdate <- strptime(paste(data_gmis$enddate[idvar], "15", 
            sep = "-"), "%Y-%m-%d")
        askedstartdate <- strptime(paste(startdate, "15", sep = "-"), 
            "%Y-%m-%d")
        if ((mindate <= askedstartdate) & (askedstartdate <= 
            maxdate)) {
            checkparameter$startdate <- TRUE
        }
        else {
            checkparameter$startdate <- FALSE
            print(paste(name, "is not available on GMIS for the", 
                askedstartdate))
        }
        askedenddate <- strptime(paste(enddate, "15", sep = "-"), 
            "%Y-%m-%d")
        if ((mindate <= askedenddate) & (askedenddate <= maxdate)) {
            checkparameter$enddate <- TRUE
        }
        else {
            checkparameter$enddate <- FALSE
            print(paste(name, "is not available on GMIS for the", 
                askedenddate))
        }
        if ((askedenddate >= askedstartdate)) {
            checkparameter$enddate <- TRUE
        }
        else {
            checkparameter$enddate <- FALSE
            print(paste(startdate, "is older than", enddate))
        }
        bboxgmis <- unlist(strsplit(data_gmis$bbox[idvar], " "))
        xmingmis <- as.numeric(bboxgmis[1])
        xmaxgmis <- as.numeric(bboxgmis[3])
        ymingmis <- as.numeric(bboxgmis[2])
        ymaxgmis <- as.numeric(bboxgmis[4])
        n1 <- xmingmis <= xmin
        n2 <- xmax <= xmaxgmis
        n3 <- ymingmis <= ymin
        n4 <- ymax <= ymaxgmis
        if (n1 & n2 & n3 & n4) {
            checkparameter$bbox <- TRUE
        }
        else {
            checkparameter$bbox <- FALSE
            print(paste("The selected area is not strickly inside the spatial extent of", 
                name, "in gmis"))
        }
    }
    if (apply(checkparameter, 1, sum) == 5) {
        startyear <- as.numeric(substr(startdate, 1, 4))
        startmonth <- as.numeric(substr(startdate, 6, 7))
        endyear <- as.numeric(substr(enddate, 1, 4))
        endmonth <- as.numeric(substr(enddate, 6, 7))
        if (startyear < endyear) {
            timeindex1 <- paste(startyear, sprintf("%02d", startmonth:12), 
                sep = "-")
            timelist <- expand.grid(1:12, (startyear + 1):(endyear - 
                1))
            timeindex2 <- paste(timelist[, 2], sprintf("%02d", 
                timelist[, 1]), sep = "-")
            timeindex3 <- paste(endyear, sprintf("%02d", 1:endmonth), 
                sep = "-")
            if ((endyear - startyear) == 1) {
                timeindex <- c(timeindex1, timeindex3)
            }
            else {
                timeindex <- c(timeindex1, timeindex2, timeindex3)
            }
        }
        else {
            timeindex <- paste(startyear, sprintf("%02d", startmonth:endmonth), 
                sep = "-")
        }
        pb <- txtProgressBar(min = 0, max = length(timeindex))
        for (i in 1:length(timeindex)) {
            if (i <= 1) {
                imgs <- getgmisdata(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url,
                  date = timeindex[i], xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax)
                names(imgs) <- timeindex[i]
            }
            else {
                img <- getgmisdata(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url,
                  date = timeindex[i], xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax)
                names(img) <- timeindex[i]
                imgs <- stack(imgs, img)
            }
            setTxtProgressBar(pb, i)
        }
        close(pb)
        return(imgs)
    }
}


#FUNCTION getGMISdata
#====================================================================

getgmisdata<-function (name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, date = startdate, xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{
    checkparameter <- data.frame(name = FALSE, resolution = FALSE, 
        date = FALSE, bbox = FALSE)

        checkparameter$resolution <- TRUE

        data_gmis<-read_gmis_wcst_data(resolution, gmis_wcst_url)
     
        idvar <- grep(name, data_gmis$shortname)
        if (length(idvar) == 0) {
            checkparameter$name <- FALSE
            print("Variable name do not exist on GMIS")
        }
        else {
            checkparameter$name <- TRUE
        }

    n0 <- nchar(date) == 7
    n1 <- substr(date, 1, 1) %in% 1:2
    n2 <- substr(date, 2, 2) %in% c(9, 0)
    n3 <- substr(date, 3, 3) %in% 0:9
    n4 <- substr(date, 4, 4) %in% 0:9
    n5 <- substr(date, 5, 5) == "-"
    n6 <- substr(date, 6, 6) %in% c(0, 1)
    n7 <- substr(date, 7, 7) %in% 0:9
    if (!(n0 & n1 & n2 & n3 & n4 & n5 & n6 & n7)) {
        checkparameter$date <- FALSE
        print("Date format is not correct (YYYY-MM)")
    }
   else {
        checkparameter$date <- TRUE
    }
    n1 <- is.numeric(xmin)
    n2 <- length(xmin) == 1
    n3 <- is.numeric(xmax)
    n4 <- length(xmax) == 1
    n5 <- is.numeric(ymin)
    n6 <- length(ymin) == 1
    n7 <- is.numeric(ymax)
    n8 <- length(ymax) == 1
    n9 <- xmin < xmax
    n10 <- ymin < ymax
    if (!(n1 & n2 & n3 & n4 & n5 & n6 & n7 & n8)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not correct (1 numeric vector for xmin, xmax, ymin and ymax)")
    }
    else {
        checkparameter$bbox <- TRUE
    }
    if (!(n9 & n10)) {
        checkparameter$bbox <- FALSE
        print("Spatial limits are not ordered correctly (xmin<xmax AND ymin<ymax)")
    }
    if (apply(checkparameter, 1, sum) == 4) {
        mindate <- strptime(paste(data_gmis$startdate[idvar], 
            "15", sep = "-"), "%Y-%m-%d")
        maxdate <- strptime(paste(data_gmis$enddate[idvar], "15", 
            sep = "-"), "%Y-%m-%d")
        askeddate <- strptime(paste(date, "15", sep = "-"), "%Y-%m-%d")
        if ((mindate <= askeddate) & (askeddate <= maxdate)) {
            checkparameter$date <- TRUE
        }
        else {
            checkparameter$date <- FALSE
            print(paste(name, "is not available on GMIS for the", 
                date))
        }
        bboxgmis <- unlist(strsplit(data_gmis$bbox[idvar], " "))
        xmingmis <- as.numeric(bboxgmis[1])
        xmaxgmis <- as.numeric(bboxgmis[3])
        ymingmis <- as.numeric(bboxgmis[2])
        ymaxgmis <- as.numeric(bboxgmis[4])
        n1 <- xmingmis <= xmin
        n2 <- xmax <= xmaxgmis
        n3 <- ymingmis <= ymin
        n4 <- ymax <= ymaxgmis
        if (n1 & n2 & n3 & n4) {
            checkparameter$bbox <- TRUE
        }
        else {
            checkparameter$bbox <- FALSE
            print(paste("The selected area is not strickly inside the spatial extent of", 
                name, "in GMIS"))
        }
    }
    if (apply(checkparameter, 1, sum) == 4) {
        bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
        con <- paste(gmis_wcst_url, "?TIME=", date, "&service=wcs&version=1.0.0&request=getcoverage&coverage=", name, "&crs=EPSG:4326&BBOX=", bbox, "&format=image/tiff&interpolation=nearest",  sep = "")
        nomfich <- paste(name, date, "img.tiff", sep = "_")
        nomfich <- tempfile(nomfich)
        download(con, nomfich, quiet = TRUE, mode = "wb")
        img <- raster(nomfich)
        img[img == 0] <- NA
        if (length(grep("log", data_gmis$unit[idvar], ignore.case = TRUE)) > 
            0) {
            img <- 10^img
        }
        names(img) <- paste(name, date)
		print(con)
        return(img)
    }
}

#FUNCTION read_GMIS_wcst_data
#=========================================================================

read_gmis_wcst_data<-function (resolution = resolution, gmis_wcst_url = gmis_wcst_url) 
{
    if (url.exists(gmis_wcst_url)) {
        doc <- xmlTreeParse(gmis_wcst_url, isURL = TRUE, encoding = "latin1")
        r <- xmlRoot(doc)
        nbvar <- xmlSize(r[[3]])
        carac <- data.frame(source = rep(unlist(r[[1]][[2]])[3], 
            nbvar), resolution, name = NA, shortname = NA, unit = NA, 
            bbox = NA, startdate = NA, enddate = NA)
        for (i in 1:nbvar) {
            carac$unit[i] <- unlist(r[[3]][[i]][[2]])[3]
            carac$shortname[i] <- unlist(r[[3]][[i]][[3]])[3]
            carac$name[i] <- unlist(r[[3]][[i]][[4]])[3]
            carac$bbox[i] <- gsub("\n", ",", paste(unlist(r[[3]][[i]][[5]])[5], 
                unlist(r[[3]][[i]][[5]])[9], collapse = "\n"))
            tps <- unlist(r[[3]][[i]][[5]])
            tps <- as.vector(tps[names(tps) == "children.timePosition.children.text.value"])
            tps <- strptime(paste(tps, "15", sep = "-"), "%Y-%m-%d")
            carac$startdate[i] <- format(min(tps, na.rm = T), 
                "%Y-%m")
            carac$enddate[i] <- format(max(tps, na.rm = T), "%Y-%m")
        }
    }
    else {
        print("no internet connection or GMIS server down")
        carac <- data.frame(source = NA, name = NA, shortname = NA, 
            unit = NA, bbox = NA, time = NA)
    }
    return(carac)
}


#EXTRACTION of the MODIST SST
#================================================================
name <- "GMIS_T_SST"
resolution <- "4km"
startdate <- "2001-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "oC"
logscale <- FALSE

mpa_sst<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)

#EXTRACTION of the PRIMARY PRODUCTION
#================================================================
name <- "GMIS_A_PPR"
resolution <- "4km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "mg m-2 day-1"
logscale <- FALSE

mpa_ppr<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)

#EXTRACTION of the SALINITY
#================================================================
name <- "GMIS_C_SALI"
resolution <- "28km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- ".0001"
logscale <- FALSE

mpa_salinity<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)

#EXTRACTION of the CHLA
#================================================================
name <- "GMIS_A_CHLA"
resolution <- "4km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "mg.m-3"
logscale <- TRUE

mpa_chla<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)

#EXTRACTION of the KD443
#================================================================
name <- "GMIS_A_K490"
resolution <- "4km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "m^-1"
logscale <- TRUE

mpa_kd443<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)


#EXTRACTION of the Surface oxygene
#================================================================
name <- "GMIS_O_OXYG"
resolution <- "28km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "mmol m-3"
logscale <- FALSE

mpa_oxygene<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)


#EXTRACTION of the Surface bbp
#================================================================
name <- "GMIS_A_BBP"
resolution <- "4km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "m^-1"
logscale <- FALSE

mpa_bbp<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)

#EXTRACTION of the Surface adg
#================================================================
name <- "GMIS_A_ADG"
resolution <- "4km"
startdate <- "1998-01"
enddate <- "2018-12"
gmis_wcst_url <- "http://mapserver.marine-analyst.eu/webservices/4km/wcs-t"
unite <- "m^-1"
logscale <- FALSE

mpa_adg<-mpaextract(name = name, resolution = resolution, gmis_wcst_url = gmis_wcst_url, startdate = startdate, enddate =enddate, xmin = extent(mpa)@xmin-1, xmax = extent(mpa)@xmax+1, ymin = extent(mpa)@ymin-1, ymax = extent(mpa)@ymax+1)
