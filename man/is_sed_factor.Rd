% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{is_sed_factor}
\alias{is_sed_factor}
\title{Test if the object is a \code{SED_factor}}
\usage{
is_sed_factor(x)
}
\arguments{
\item{x}{An object}
}
\value{
A logical value. \code{TRUE} for SED factor matrix.
}
\description{
Returns \code{TRUE} for SED factor matrix, and \code{FALSE} for all other objects, including lists.
}
\examples{
is_sed_factor(x)
}
