% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anonymize.R
\name{select_sensitive}
\alias{select_sensitive}
\title{Select sensitive columns}
\usage{
select_sensitive(data)
}
\arguments{
\item{data}{The MOAS or a MOAS generated file.}
}
\value{
tibble
}
\description{
This function will select all the columns from
a MOAS-like data.frame that are known to include
possible sensitive information.
}
\examples{
dt <- data.frame(ID = 1:3,
    Birth_Date = c("1997-01-12", "1984-01-01", "1953-09-16"),
   Comment = c("", "Has leukemia", "is on diazepam"),
   CVLT_A = c(19, 25, 29),
   stringsAsFactors = FALSE)
select_sensitive(dt)
}
