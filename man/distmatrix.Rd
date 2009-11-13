\name{distmatrix}
\alias{distmatrix}
\title{Compute distance matrices on the CShapes dataset}
\description{
  This function computes a distance matrix for the given date. 
  It selects all the active CShapes polygons, determines their distances and 
  outputs a distance matrix in kilometers. The function can compute different types of distance matrices, 
  specified by the "type" parameter: (i) capital distances, and (ii) centroid distances, and (iii) minimum distances
  between polygons. The latter computation is very expensive if polygons have many nodes. For that reason, the function
  simplifies the country polygons according to the Douglas-Peucker algorithm (\url{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm}), 
  which eliminates points from the polygons and speeds up computation. The \code{tolerance} parameter specifies the tolerance for the simplification;
  a value of 0 disables it.
}
\usage{
  distmatrix(date, type="mindist", tolerance=0.1, useGW=T)
}
\arguments{
  \item{date}{The date for which the distance matrix should be computed. 
  This argument must be of type Date and must be in the range 1/1/1946 - 30/6/2008.}
  \item{type}{Specifies the type of distance matrix: \code{capdist} for capital distances, \code{centdist} for centroid distances, and \code{mindist} for minimum distances.}
  \item{useGW}{Boolean argument specifying the system membership coding. TRUE (Default): Gleditsch and Ward (1999). FALSE: Correlates of War.}
  \item{tolerance}{Tolerance for polygon simplification according the the Douglas-Peucker algorithm. Only used for mindist computation (type="mindist").}	
}
\value{
  A quadratic weights matrix, with the row and column labels containing 
  the country identifiers in the specified coding system (COW or G&W).
}
\examples{
# Compute a matrix of minimum distances 
# for the international system on 1/1/2008
# using the Correlates of War list and the default accuracy
wmat <- distmatrix(as.Date("2008-1-1"), type="capdist", useGW=FALSE)

# For model estimation, our matrix should contain 
# the inverted distances
wmat <- 1/wmat

# Fix the values along the diagonale
diag(wmat) <- 0 
}
\author{Nils B. Weidmann}