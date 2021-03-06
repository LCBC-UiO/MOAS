% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epigenetic-funcs.R
\name{epigen_get}
\alias{epigen_get}
\title{Get epigenetic data}
\usage{
epigen_get(
  file_path,
  match_path = "path/to/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
  debug = FALSE
)
}
\arguments{
\item{file_path}{path to excel file containing epigenetic
data}

\item{match_path}{path to file that matches genetic ID to
CrossProject_ID}

\item{debug}{logical, if debugging informaition should be added.
Defaults to FALSE}
}
\description{
Get epigenetic data
}
\seealso{
Other epigen-functions: 
\code{\link{epigen_add}()}

Other MOAS get-functions: 
\code{\link{dbs_get}()}
}
\concept{MOAS get-functions}
\concept{epigen-functions}
