\name{cshapes2yearly}
\alias{cshapes2yearly}
\title{Convert CShapes data frame to a country-year format}
\description{
  This function converts the original cshapes data frame to a panel structure with country-year observations.
  The user has to specify the list of variables from CShapes to be included in the new data frame. These can be variables already included in CShapes, but will (in most cases) 
  be user-computed variables appended to the CShapes data frame.
}
\usage{
  cshapes2yearly(cshp, vars, useGW=T)
}
\arguments{
  \item{cshp}{A SpatialPolygonsDataFrame in CShapes format. In almost all cases, this
  will the original cshapes frame with some variables appended. These variables can be computed outside R 
  (e.g.) in a GIS and using this function, can be converted to a convenient format for statistical analysis.}
  \item{vars}{A character vector with the variable names to be included in the new frame (besides country code and year).}
  \item{useGW}{Boolean argument specifying the system membership coding. TRUE (Default): Gleditsch and Ward (1999). FALSE: Correlates of War.}
}
\value{
  A data frame, with country-year observations. For each year, the function will use
  the CShapes polygon that was active on June 30 of the respective year. The columns in the
  data frame are "ctrcode" for the country code (either COW or Gleditsch&Ward, according to the \code{useGW} parameter setting), and "year".
}
\examples{
# Convert the standard cshapes dataset
# to a country-year format
cshp <- cshp()
cshp.yearly <- cshapes2yearly(cshp, vars=c("AREA", "CAPNAME"))
}
\author{Nils B. Weidmann}