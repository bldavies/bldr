% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na2zero.R
\name{na2zero}
\alias{na2zero}
\title{Replace NAs with zeros}
\usage{
na2zero(x)
}
\arguments{
\item{x}{Input object.}
}
\description{
\code{na2zero} replaces NAs with zeros.
}
\examples{
sapply(list(1, 2, NA, 4), na2zero)

}
