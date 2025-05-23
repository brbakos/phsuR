% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-holidays.R
\name{compute_victoria_day}
\alias{compute_victoria_day}
\title{Compute Victoria Day}
\usage{
compute_victoria_day(calendar_year)
}
\arguments{
\item{calendar_year}{An integer representing the calendar year.}
}
\value{
A \code{Date} object representing Victoria Day.
}
\description{
This function computes Victoria Day for a given calendar year.
Victoria Day is defined as the Monday immediately preceding May 25.
}
\examples{
compute_victoria_day(2025)
}
