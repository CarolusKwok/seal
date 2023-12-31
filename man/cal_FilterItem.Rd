% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_FilterItem.R
\name{cal_FilterItem}
\alias{cal_FilterItem}
\title{Filter items based on properties of said item}
\usage{
cal_FilterItem(SED, data = NULL, ...)
}
\arguments{
\item{SED}{SED}

\item{data}{The name of dataset to be converted, in \code{character}. By default (\code{NULL}), all datasets will be converted.}

\item{...}{An expression that returns a logical value, that terms are defined by properties set in \code{sed_item} of the selected \code{data}. Only item with an evaluation of \code{TRUE} will be kept. For more information, please check \code{dplyr::filter}, which this function is based on.}
}
\value{
SED
}
\description{
Note that \code{...} expressions must be comprehensible in the dataset. Non-comprehensible expressions will return an error.
}
\examples{
cal_FilterItem(SED, data = NULL, taxonomic_group == "Civet")
}
