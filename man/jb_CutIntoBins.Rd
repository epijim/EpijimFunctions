% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jb_CutIntoBins.R
\name{jb_CutIntoBins}
\alias{jb_CutIntoBins}
\title{Make sparklines from a variable}
\usage{
jb_CutIntoBins(variable, time = "potato", name = "line", bins = 10,
  width = 6)
}
\arguments{
\item{time}{This is the x axis. If only one variable is given, this isn't needed.}

\item{name}{The name to give this sparkline. I define as a command to keep the main text where you insert it tidy.}

\item{bins}{Number of bins or bars in the histogram. Defaults to 10.}

\item{width}{The width of the histogram. Defaults to 6.}
}
\description{
Sparkline histograms form an R variable
}
\examples{
jb_CutIntoBins()
}
\keyword{latex}

