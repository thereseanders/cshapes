cshp <- function(date=NA) {
		
	# check input
  	if (!is.na(date) && !inherits(date, "Date")) {
    	stop("date is not of type Date")
  	}
  
  	if (!is.na(date) && (date < as.Date("1946-1-1") | date > as.Date("2008-6-30"))) {
    	stop("Specified date is out of range")
  	}
	
	# where to look for the dataset
	path <- paste(system.file(package = "cshapes"), "shp/cshapes.shp", sep="/")
	
	# load the dataset
	cshp.full <- readShapePoly(path, proj4string=CRS("+proj=longlat +ellps=WGS84"), IDvar="FEATUREID")
	
	if (is.na(date)) {
		cshp.full
	} else {
		startdate <- as.Date(paste(cshp.full$COWSYEAR, cshp.full$COWSMONTH, cshp.full$COWSDAY, sep="-"))
		enddate <- as.Date(paste(cshp.full$COWEYEAR, cshp.full$COWEMONTH, cshp.full$COWEDAY, sep="-"))
		cshp.part <- cshp.full[startdate < date & enddate >= date,]
		cshp.part
	}	
}

