distmatrix <- function(date, type="mindist", tolerance=0.1, useGW=T) {

  # check input
  if (!inherits(date, "Date")) {
    stop("date is not of type Date.")
  }
  
  if (date < as.Date("1946-1-1") | date > as.Date("2008-6-30")) {
    stop("Specified date is out of range")
  }
  
  if (!(type %in% c("mindist", "mindist", "capdist", "centdist"))) {
  	stop("Wrong type argument. Possible values: mindist, capdist, centdist")
  }
  
  if (tolerance<0) {
  	stop("Tolerance must be >=0")
  }
    
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))  
  cd <- .jnew("CountryDistancer", system.file(package = "cshapes"), year, month, day, tolerance, useGW)
  
  # minimum distance
  if (type=="mindist") {
  	dmat <- .jcall(cd, "getMinDistMatrix", returnSig="[D")
  }
      
  # capital distance
  if (type=="capdist") {
  	dmat <- .jcall(cd, "getCapDistMatrix", returnSig="[D")
  }
  
  # centroid distance
  if (type=="centdist") {
  	dmat <- .jcall(cd, "getCentroidDistMatrix", returnSig="[D") 
  }
 	
  dimension <- .jcall(cd, "getDimension", returnSig="I")
  resultmatrix <- matrix(dmat, nrow=dimension, ncol=dimension)
  dimlabels <- .jcall(cd, "getCtrcodes", returnSig="[D")
  colnames(resultmatrix) <- dimlabels
  rownames(resultmatrix) <- dimlabels
  resultmatrix
}

