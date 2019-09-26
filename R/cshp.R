cshp <- function(date=NA, useGW=TRUE, simplify=FALSE, tolerance = 0.05) {
		
	# check input
  	if (!is.na(date) && !inherits(date, "Date")) {
    	stop("date is not of type Date")
  	}
  
  	if (!is.na(date) && (date < as.Date("1946-1-1") | date > as.Date("2016-6-30"))) {
    	stop("Specified date is out of range")
  	}
	
	# where to look for the dataset
	path <- paste(system.file(package = "cshapes"), "shp/cshapes.shp", sep="/")
	
	# load the dataset
	cshp.full <- st_read(path)
	
	if (simplify == T) {
	  cshp.full <- st_simplify(cshp.full, dTolerance = tolerance)
	}

	if (is.na(date)) {
		cshp.full
	} else if (useGW) {
	  	cshp.full <- cshp.full[cshp.full$GWCODE>=0,]
		startdate <- as.Date(paste(cshp.full$GWSYEAR, cshp.full$GWSMONTH, cshp.full$GWSDAY, sep="-"))
		enddate <- as.Date(paste(cshp.full$GWEYEAR, cshp.full$GWEMONTH, cshp.full$GWEDAY, sep="-"))
		cshp.part <- cshp.full[startdate <= date & enddate >= date,]
		cshp.part
	} else {
		cshp.full <- cshp.full[cshp.full$COWCODE>=0,]
		startdate <- as.Date(paste(cshp.full$COWSYEAR, cshp.full$COWSMONTH, cshp.full$COWSDAY, sep="-"))
		enddate <- as.Date(paste(cshp.full$COWEYEAR, cshp.full$COWEMONTH, cshp.full$COWEDAY, sep="-"))
		cshp.part <- cshp.full[startdate <= date & enddate >= date,]
		cshp.part
	}	

}

