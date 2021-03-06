% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_dups.R
\name{fix_dups}
\alias{fix_dups}
\title{NA replacement in duplicated columns}
\usage{
fix_dups(data, suffix, remove = T)
}
\arguments{
\item{data}{A data.frame or tibble.}

\item{suffix}{A single string with column suffix to look for.}

\item{remove}{logical. If column with suffix are to be removed.}
}
\value{
a data frame with added/replaced age and timepoint variables.
}
\description{
\code{fix_dups} creates a data.frame where duplicated columns
with a specific suffix are row-wise gone through, and where there
is NA in the original column, the suffixed column's data is
placed there instead.
}
\examples{
\dontrun{
fix_dups(data, suffix="YY")
}

}
