\name{cshp}
\alias{cshp}
\title{Access the CShapes dataset in R}
\description{
  The cshapes package makes the full dataset available for usage in R. This function returns a \code{SpatialPolygonsDataFrame} with the complete cshapes dataset.
}
\usage{
  cshp()
}
\value{
  A \code{SpatialPolygonsDataFrame}, containing the complete CShapes dataset.
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
}
\author{Nils B. Weidmann}
\seealso{
\code{\link[sp]{SpatialPolygonsDataFrame}}
}