\name{distlist}
\alias{distlist}
\title{Compute distance lists on the CShapes dataset}
\description{
  This function computes a distance list for the given date. 
  It selects all the active CShapes polygons, determines their distances and 
  outputs a distance list. A distance list is a list of dyads of countries and the distances between them.
  This list is returned as a data frame with three columns:
  \enumerate{
  \item ccode1 -- country 1's code in the coding system specified by the \code{useGW} parameter
  \item ccode2 -- country 2's code in the coding system specified by the \code{useGW} parameter
  \item {capdist, centdist, mindist} -- distance between country 1 and country 2 in km, where distance can be 
    either capital distance, centroid distance or minimum distance, as specified by the \code{type} parameter.
    The latter computation is very expensive if polygons have many nodes. For that reason, the function
    simplifies the country polygons according to the Douglas-Peucker algorithm (\url{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm}), 
  which eliminates points from the polygons and speeds up computation. The \code{tolerance} parameter specifies the tolerance for the simplification; a value of 0 disables it.
  }
  Note that the function returns directed dyads. For example, if there is a country with code 1 and a country with code 2, the 
  resulting data frame contains the dyads (1,2), (2,1), (1,1) and (2,2). It is easy to extract the directed dyads from this data frame (see example below). 
}
\usage{
  distlist(date, type="mindist", tolerance=0.1, useGW=T)
}
\arguments{
  \item{date}{The date for which the distance list should be computed. 
  This argument must be of type Date and must be in the range 1/1/1946 - end of the dataset.}
  \item{type}{Specifies the type of distance list: \code{capdist} for capital distances, \code{centdist} for centroid distances, and \code{mindist} for minimum distances.}
  \item{useGW}{Boolean argument specifying the system membership coding. TRUE (Default): Gleditsch and Ward (1999). FALSE: Correlates of War.}
  \item{tolerance}{Tolerance for polygon simplification according the the Douglas-Peucker algorithm. Only used for mindist computation (type="mindist").}	
}
\value{
  A distance list with three columns, the first two of which contain the identifiers for the
  two countries in the dyad, and the third one containing the distance between the two countries.
}
\examples{
# Compute a list of minimum distances 
# for the international system on 1/1/1946
# using the Correlates of War list and the default accuracy
\dontrun{dl <- distlist(as.Date("1946-1-1"), type="capdist", tolerance=0.5, useGW=FALSE)}

# we eliminate duplicate dyads
\dontrun{dl <- subset(dl, ccode1 < ccode2)}
}
\author{Nils B. Weidmann}