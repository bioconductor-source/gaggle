\name{getHashMap}
\alias{getHashMap}
\title{Create an R variable from Most Recently Broadcast HashMap}
\description{After a hashMap is broadcast to the R goose, one additional step is required of the R
             user before it is available as a character list variable in R.  (This step may be eliminated in
             the future.)}
\usage{env <- getHashMap ()}
\arguments{
  }

\value{Returns an environment with a title, and then the usual keys and values.}

\seealso{
  \code{\link{getNameList}}.
  \code{\link{getCluster}}.
  \code{\link{getMatrix}}.
  \code{\link{getNetwork}}.
  }
\keyword{manip}

