\name{cshapes-package}
\alias{cshapes-package}
\alias{cshapes}
\docType{package}
\title{CShapes Dataset and Utilities}
\description{
R Package for CShapes, a GIS dataset of country boundaries (1946-2008). Includes functions for data extraction and the computation of weights matrices.}
\details{
The cshapes R package facilitates the use of CShapes, a GIS dataset of country boundaries. In particular, the package enables access to the 
dataset directly, as well as distance computations on country polygons for specific points in time. Access to the dataset from within R is done using the 
\code{\link{cshp}} function. The distmatrix function can be used to compute minimum-, capital- and centroid distances between countries. 
See the examples given in the documentation of the functions in this package.

Implementation: The distance matrix computation is implemented in Java using the JAMA, JTS and JUMP libraries, using the rJava package to invoke the computation from within R. Warning: If the Java process
receives an interrupt from outside R, this will cause R to abort.
}


\author{
Nils B. Weidmann <nils.weidmann@gmail.com>, Doreen Kuse <doreen.kuse@gmail.com>, Kristian S. Gleditsch <ksg@essex.ac.uk>

Maintainer: Nils B. Weidmann <nils.weidmann@gmail.com>, Kristian S. Gleditsch <ksg@essex.ac.uk>

}
\references{
Correlates of War Project. 2008. "State System Membership List, v2008.1." Online, \url{http://correlatesofwar.org}.

Gleditsch, Kristian S. & Michael D. Ward. 1999. "Interstate System Membership: A Revised List of the Independent States since 1816." International Interactions 25: 393-413.
Online, \url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.

Java Matrix Package JAMA, v. 1.0.2 (\url{http://math.nist.gov/javanumerics/jama/})

Java Topology Suite JTS, v. 1.7.2 (\url{http://www.vividsolutions.com/jts/jtshome.htm})

Java Unified Mapping Platform JUMP, v. 1.2 (\url{http://www.vividsolutions.com/jump/})
}

\keyword{ package }

