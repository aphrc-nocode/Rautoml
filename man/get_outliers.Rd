% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_outliers}
\alias{get_outliers}
\title{Identify outliers}
\usage{
get_outliers(df, var)
}
\arguments{
\item{x}{numeric variable}
}
\value{
a vector of outlier values.
}
\description{
Identify outlier values of a numeric variable.
}
\details{
Uses \code{boxplot} function to identify outliers.
}
