% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_raster.R
\name{sample_raster}
\alias{sample_raster}
\title{Extract values from a raster in a data frame with longitude, latitude}
\usage{
sample_raster(raster_FPI = r_cdist, df_grid = sample, LON = "x", LAT = "y")
}
\arguments{
\item{raster_FPI}{A raster with values to extract}

\item{df_grid}{The dataframe with geographical coordinates that the raster_FPI will be extracted.}

\item{LON}{Name of Longitude field in df_grid in "". The longitude should be in a geographical coordinate system WGS84}

\item{LAT}{Name of Latitude field in df_grid in "". The longitude should be in a geographical coordinate system WGS84}
}
\value{
Return of a data.frame with the values of the raster incorporated in the selected grid.
}
\description{
Sample values and include in a data.frame (grid) with geographical coordinates.
}
\examples{
library(tools4MCDA)
library(dplyr)

#load data
data("dfcut2")
data("cdist_r")

#select a sub-area
GSA<-20

#create GRID for sample
dfcut2$lon<-as.numeric(dfcut2$x)
dfcut2$lat<-as.numeric(dfcut2$y)
r<-rasterFromXYZ(dfcut2[, c('lon', 'lat')])
crs(r) <- CRS('+init=EPSG:4326')
sample<-as.data.frame(dfcut2, xy=TRUE)
sample<-joinFPI_GSA(data_FPI=sample,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="x",LAT="y")
sample<-sample[sample$SMU_CODE \%in\% GSA,]

#plot initial data
r_cdist <- rasterFromXYZ(cdist_r[, c('x', 'y', 'cdist')])
crs(r_cdist) <- CRS('+init=EPSG:4326')
plot(r_cdist)
sf::sf_use_s2(FALSE)

#sample from raster
cdist_r_GSA20<-sample_raster(raster_FPI=r_cdist, df_grid=sample, LON="x", LAT="y")
cdist_r_GSA20 <- cdist_r_GSA20 \%>\% rename("cdist" = "layer")

#plot data after sample
cdist_r_GSA20 <- rasterFromXYZ(cdist_r_GSA20[, c('LON', 'LAT', 'cdist')])
crs(cdist_r_GSA20) <- CRS('+init=EPSG:4326')
plot(cdist_r_GSA20)
}
\author{
I. Maina
}
