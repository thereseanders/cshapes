cshp <- function() {
	
	# where to look for the dataset
	path <- paste(system.file(package = "cshapes"), "shp/cshapes.shp", sep="/")
	
	# load the dataset
	cshp.full <- readShapePoly(path, proj4string=CRS("+proj=longlat +ellps=WGS84"), IDvar="FEATUREID")
	cshp.full  
}

