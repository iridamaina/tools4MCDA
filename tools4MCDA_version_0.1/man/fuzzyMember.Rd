% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzyMember.R
\name{fuzzyMember}
\alias{fuzzyMember}
\title{Normalization of a data object}
\usage{
fuzzyMember(x, class = F)
}
\arguments{
\item{x}{The data object can be a vector, matrix, dataframe, Shapefile, or
RasterLayer object.}

\item{class}{If missing, the class is numeric, otherwise must be defined.
Other options are Shapefile and RasterLayer objects.}
}
\value{
A data object with values in [0,1].
}
\description{
It normalizes a data object x to [0,1] using the formula
(x-xmin)/(xmax-xmin).
}
\examples{

library(raster)
library(tools4MCDA)
#-- Example 1 --#
x <- rnorm(100)
fuzzyMember(x)

#-- Example 2 --#
v1 <- rnorm(10)
v2 <- rpois(10,lambda=2)
v3 <- rexp(10)
x <- data.frame(v1,v2,v3)
fuzzyMember(x$v1)
apply(x, 2, fuzzyMember)

#-- Example 3 --#
r <- raster(ncol=5, nrow=5)
r <- init(r, rnorm)
rFM <- fuzzyMember(r, "raster")
par(mfrow=c(2,1))
plot(r)
plot(rFM)
}
\author{
D. Politikos
}
