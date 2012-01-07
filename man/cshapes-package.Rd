\name{cshapes-package}
\alias{cshapes-package}
\alias{cshapes}
\docType{package}
\title{CShapes Dataset and Utilities}
\description{
R Package for CShapes, a GIS dataset of country boundaries (1946-2012). Includes functions for data extraction and the computation of weights matrices.}
\details{
The \code{cshapes} package facilitates the use of CShapes from R. CShapes is a GIS dataset of historical country boundaries (1946-2012) and compatible with two country lists (Gleditsch and Ward 1999 and Correlates of War, see references below). In particular, the package enables access to the 
dataset directly, as well as distance computations on country polygons for specific points in time. Access to the dataset from within R is done using the 
\code{\link{cshp}} function. Two functions exist to compute minimum-, capital- and centroid distances between countries: the \code{\link{distmatrix}} function returns these as a matrix (convenient for many spatial statistical applications), and the \code{\link{distlist}} function returns a list of dyadic distances (distances between country pairs).
See the examples given in the documentation of the functions in this package. The main cshapes website is located at \url{http://nils.weidmann.ws/projects/cshapes} and contains additional documentation and examples for the dataset and the R package.

For more information on the CShapes dataset, see Weidmann, Nils B., Doreen Kuse and Kristian Skrede Gleditsch. 2010. "The Geography of the International System: The CShapes Dataset." International Interactions 36(1).

The associated R package is introduced in Weidmann, Nils B. and Kristian Skrede Gleditsch. 2010. "Mapping and Measuring Country Shapes: The cshapes Package." R Journal 2(1). Available online at \url{http://journal.r-project.org/archive/2010-1/RJournal_2010-1_Weidmann+Skrede~Gleditsch.pdf}.
}


\author{
Nils B. Weidmann <nils.weidmann@uni-konstanz.de>, Doreen Kuse <doreen.kuse@gmail.com>, Kristian S. Gleditsch <ksg@essex.ac.uk>

Maintainer: Nils B. Weidmann <nils.weidmann@uni-konstanz.de>, Kristian S. Gleditsch <ksg@essex.ac.uk>

}
\references{
Correlates of War Project. 2011. "State System Membership List, v2011.1." Online, \url{http://correlatesofwar.org}.

Gleditsch, Kristian S. & Michael D. Ward. 1999. "Interstate System Membership: A Revised List of the Independent States since 1816." International Interactions 25: 393-413.
Online, \url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.

}

\keyword{ package }

