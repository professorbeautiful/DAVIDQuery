\name{AffyProbesetList}
\alias{AffyProbesetList}
\title{ Retrieve Affy probeset IDs from DAVID.}
\description{
For a given Affymetrix microarray chip, retrieve Affy probeset IDs from DAVID.  Optionally,  a menu is used to pick the chip name.
}
\usage{
AffyProbesetList(chipname = NULL, menu = TRUE, verbose=FALSE)
}
\arguments{
  \item{chipname}{ Full name or regular expression. }
  \item{menu}{ Select chipname from a menu (default=TRUE) }
  \item{verbose}{ Print a bit of tracing information along the way (default=FALSE) . }
}
\details{
First, DAVID's table of chip names is retrieved.  When the user selects or specifies one of the names, the associated file of probeset names is retrieved.  If \code{chipname} is a regular expression, then the menu (if requested) is subsetted accordingly.
}
\value{
Character vector of probeset names.}
\author{ Roger Day }
\examples{
head(AffyProbesetList("Human Genome U133 Plus 2.0", menu=FALSE, verbose=TRUE))
\dontrun{
length(AffyProbesetList("133|95"))
}
}
\note{ Use with caution. The returned file is not guaranteed to be correct. In the example above, with the chip \code{"Human Genome U133 Plus 2.0"}, the list returned includes 40907 probeset IDs on the chip (and no others), but appears to be missing 13768 others. 
}
\keyword{ database }
