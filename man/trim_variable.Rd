% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/econometrics.R
\name{trim_variable}
\alias{trim_variable}
\title{Trim Variable}
\usage{
trim_variable(x, left_trim, right_trim)
}
\arguments{
\item{x}{a vector to be trimmed.}

\item{left_trim}{percentage of observations to trim from right of the variable's distribution.}

\item{right_trim}{percentage of observations to trim from left of the variable's distribution.}
}
\value{
vector
}
\description{
If defined, trims data based on left and right trim level / interval for use
by the summarize_data function.
}
\author{
Michael David Gill
}

