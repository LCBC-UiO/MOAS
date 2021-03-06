% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbs-funcs.R
\name{dbs_add}
\alias{dbs_add}
\title{Add DBS data to data frame}
\usage{
dbs_add(data, file_path, match_file = "data-raw/Blood_data/DBS_MOAS_match.tsv")
}
\arguments{
\item{data}{a MOAs--like data frame the data should be added
to column-wise}

\item{file_path}{path to DBS excel-file}

\item{match_file}{path to the match-file for DBS}
}
\value{
tibble
}
\description{
calls [\code{dbs_get}] to get DBS information
from file and add it to the MOAS-like dta frame
}
\examples{
\dontrun{
dbs_add(MOAS_testing,
       file_path = "path/to/file",
       match_file = "path/to/MOAS/data-raw/Blood_data/DBS_MOAS_match.tsv")
 }
}
\concept{MOAS add-funcions}
\concept{dbs-funcions}
