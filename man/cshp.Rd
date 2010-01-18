\name{cshp}
\alias{cshp}
\title{Access the CShapes dataset in R}
\description{
  The \code{cshp} function makes the cshapes dataset available for usage in R. If no date is given, it returns a \code{SpatialPolygonsDataFrame} with the complete CShapes dataset. If specified, the date is used to create a snapshot of the dataset, containing all cshapes polygons active at the given date.
}
\usage{
  cshp(date=NA)
}
\arguments{
  \item{date}{The date for which the cshapes polygons should be extracted. 
  This argument must be of type Date and must be in the range 1/1/1946 - 30/6/2008. If omitted, the complete dataset is returned.}
}
\value{
  A \code{SpatialPolygonsDataFrame}, containing the complete CShapes dataset, or the CShapes snapshot for the specified date.
}
\examples{
# Retrieve the dataset
cshp.data <- cshp()

# Get summary statistics
summary(cshp.data)

# Extract Switzerland 
switzerland <- cshp.data[cshp.data$COWCODE==225,]

# Plot Switzerland
plot(switzerland)

# Extract a snapshot of cshapes as of June 30, 2002
cshp.2002 <- cshp(date=as.Date("2002-6-30"))
}
\author{Nils B. Weidmann}
\seealso{
\code{\link[sp]{SpatialPolygonsDataFrame}}
}