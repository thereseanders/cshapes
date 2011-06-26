cshapes2yearly <- function(cshp, vars, useGW=T) {

  if (length(vars)==0) stop("Empty list of variables")

  if (useGW) {
  	cshp <- cshp[cshp$GWCODE>=0,]
    startday <- cshp$GWSDAY
    startmonth <- cshp$GWSMONTH
    startyear <- cshp$GWSYEAR    
    startdate <- as.Date(paste(cshp$GWSYEAR, cshp$GWSMONTH, cshp$GWSDAY, sep="-"))
    
    endday <- cshp$GWEDAY
    endmonth <- cshp$GWEMONTH
    endyear <- cshp$GWEYEAR    
    enddate <- as.Date(paste(cshp$GWEYEAR, cshp$GWEMONTH, cshp$GWEDAY, sep="-"))
  } else {
    cshp <- cshp[cshp$COWCODE>=0,]
    startday <- cshp$COWSDAY
    startmonth <- cshp$COWSMONTH
    startyear <- cshp$COWSYEAR    
    startdate <- as.Date(paste(cshp$COWSYEAR, cshp$COWSMONTH, cshp$COWSDAY, sep="-"))
    
    endday <- cshp$COWEDAY
    endmonth <- cshp$COWEMONTH
    endyear <- cshp$COWEYEAR    
    enddate <- as.Date(paste(cshp$COWEYEAR, cshp$COWEMONTH, cshp$COWEDAY, sep="-"))
  }
  
  ctrcodename <- ifelse(useGW, "GWCODE", "COWCODE") 
  
  data.startyear <- min(startyear)
  data.endyear <- max(endyear)
  
  # create empty data frame, oversized
  # need to remove unused lines later
  # reason: difficult to grow data frames on the fly
  
  years <- seq(from=data.startyear, to=data.endyear, by=1)
  countries <- sort(unique(as.data.frame(cshp)[,ctrcodename]))
  allcountries <- rep(countries, each=length(years))
  allyears <- rep(years, times=length(countries))
  res.frame <- data.frame(allcountries, allyears)
  names(res.frame) <- c("ctrcode", "year") 
  
  # we create a vector to keep track of the line use
  
  touched <- rep(FALSE, nrow(res.frame))
  
  # iterate through cshapes polygons
  for (p in 1:length(startyear)) {
    polygon <- as.data.frame(cshp)[p,]
    ctrcode <- ifelse(useGW, polygon$GWCODE, polygon$COWCODE)
    firstyear <- ifelse(startmonth[p]<=7, startyear[p], startyear[p]+1)
    lastyear <- ifelse(endmonth[p]>=6, endyear[p], endyear[p]-1)
    for (field in vars) {
      for (year in firstyear:lastyear) {
        res.frame[res.frame$ctrcode==ctrcode & res.frame$year==year,field] <- polygon[,field]
        touched[res.frame$ctrcode==ctrcode & res.frame$year==year] <- TRUE
      }
    }
  }
  
  # return only the used lines in the data frame
  res.frame[touched,]
}
