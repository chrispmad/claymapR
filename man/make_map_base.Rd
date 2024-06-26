% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_map_base.R
\name{make_map_base}
\alias{make_map_base}
\title{Make map 'base'}
\usage{
make_map_base(shapes, buffer = 5)
}
\arguments{
\item{shapes}{polygons to be mapped}

\item{buffer}{How far should the 'base' extend from the polygons to be mapped, as percentage of largest X or Y dimension?}
}
\value{
An sf polygon the represents the 'base' shape
}
\description{
Make map 'base'
}
\examples{
\dontrun
}
