% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_one2one.R
\name{is_one2one}
\alias{is_one2one}
\title{Test for one-to-one correspondence between vectors}
\usage{
is_one2one(x, y, na.rm = T)
}
\arguments{
\item{x, y}{Vectors of equal length.}

\item{na.rm}{Should NA values be ignored?
Default is TRUE.}
}
\description{
\code{is_one2one} tests whether there is a one-to-one map from the values
in a vector \code{x} to the corresponding values in a vector \code{y}.
}
