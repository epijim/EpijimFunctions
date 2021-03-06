% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jb_pushoverr.R
\name{jb_pushfinished}
\alias{jb_pushfinished}
\title{Send push notifications to your phone from R when script completes}
\usage{
jb_pushfinished(message = "Script completed at ")
}
\arguments{
\item{message}{the message you wish to add. Time stamp is appended after.}
}
\description{
Token is a text file in working directory. Pushover account required.
}
\examples{
Read in token
token <- readChar("token",
                 file.info("token")$size) # shrink to character length
set_pushover_app(
  token=token,
  user="umTsDu9PSNCSf92NXEY21jUsvz3faT")
  jb_pushfinished() # at end of file
}
\keyword{iphone}

