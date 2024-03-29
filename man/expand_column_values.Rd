% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance.r
\name{expand_column_values}
\alias{expand_column_values}
\title{expand_column_values}
\usage{
expand_column_values(column, values, index_i, index_j)
}
\arguments{
\item{column}{Name of column to be expanded}

\item{values}{Actual \code{vector} of data to expand}

\item{index_i}{\code{vector} of indexes to expand \code{column}}

\item{index_j}{Second \code{vector} of indexes to expand \code{column}}
}
\value{
A list
}
\description{
Creates extra columns to store in a list
}
\details{
Given a \code{vector} of values, create repeated values of it according to two index variables.
}
\examples{

thedf <- data.frame(
ID=rep(LETTERS[1:3], length.out=10),
x=sample(10),
y=sample(10),
extra1=sample(letters, size=10),
extra2=sample(letters, size=10),
extra3=sample(10),
extra4=Sys.time() + 1:10
)
distancethreshold:::expand_column_values('extra1', thedf$extra1, index_i=c(1, 3), index_j=c(2, 4))
distancethreshold:::expand_column_values('extra2', thedf$extra2, index_i=c(1, 3), index_j=c(2, 4))
distancethreshold:::expand_column_values('extra3', thedf$extra3, index_i=c(1, 3), index_j=c(2, 4))
distancethreshold:::expand_column_values('extra4', thedf$extra4, index_i=c(1, 3), index_j=c(2, 4))
}
