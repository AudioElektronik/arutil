% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_duplicate.R
\name{remove_duplicate}
\alias{remove_duplicate}
\title{Removing duplicates from a data.frame}
\usage{
remove_duplicate(frame)
}
\description{
Used for piping a data.frame variant with possibly duplicate elements.
Returned frame does not have duplicate elements. Duplicate means, more than
one row having identical values for each column.
}
\examples{
x <- data.frame(x = c(1,1,2), y = c('a', 'a', 'b')) \%>\%
 remove_duplicate()

}

