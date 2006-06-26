\name{gaggleInit}
\alias{gaggleInit}
\title{Initialize the R Goose, registering with the Gaggle}
\description{Call this function to register the R
  Goose with an already-running Gaggle Boss; this makes all subsequent
  communication possible.  This function call will fail if a boss is not running.
  By default, this function looks for, and registers with, a boss on your current
  computer ('localhost').   Alternatively, you may supply the
  name of another computer where a boss is running.(See warning, below, about
  avoiding fully-qualified names.)
  This is useful if you wish to run R on a remote compute 
  server (perhaps with extra memory and speed), and connect it 
  to a gaggle running on your desktop.
}
\usage{gaggleInit (bossHost = 'localhost')}

\arguments{
  \item{bossHost}{the computer ('host') on which your GaggleBoss is running. \emph{For some mysterious
reason, in my testing I have found that  the hostname must NOT be fully qualified.  Simple, single 
names like 'trickster'and 'hazel' work reliably, but their fully-qualified counterparts --
'trickster.systemsbiology.org' and 'hazel.systemsbiology.net' -- do not.}}
  }


\value{Returns NULL.}

\seealso{
  \code{\link{broadcast}}.
  \code{\link{getNameList}}.
  \code{\link{getMatrix}}.
  \code{\link{getNetwork}}.
  \code{\link{getCluster}}.
  \code{\link{getHashMap}}.
  }

\keyword{manip}

