#' Extract values from a raster in a data frame with longitude, latitude
#'
#' Sample values and include in a data.frame (grid) with geographical coordinates.
#'
#'
#' @param raster_FPI A raster with values to extract 
#' @param df_grid  The dataframe with geographical coordinates that the raster_FPI will be extracted.
#' @param LON Name of Longitude field in df_grid in "". The longitude should be in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in df_grid in "". The longitude should be in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame with the values of the raster incorporated in the selected grid.
#' @author I. Maina 
#' @examples
#'library(tools4MCDA)
#'library(dplyr)
#'
#'#load data
#'data("dfcut2")
#'data("cdist_r")
#'
#'#select a sub-area
#'GSA<-20
#'
#'#create GRID for sample
#'dfcut2$lon<-as.numeric(dfcut2$x)
#'dfcut2$lat<-as.numeric(dfcut2$y)
#'r<-rasterFromXYZ(dfcut2[, c('lon', 'lat')])
#'crs(r) <- CRS('+init=EPSG:4326')
#'sample<-as.data.frame(dfcut2, xy=TRUE)
#'sample<-joinFPI_GSA(data_FPI=sample,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="x",LAT="y")
#'sample<-sample[sample$SMU_CODE %in% GSA,]
#'
#'#plot initial data
#'r_cdist <- rasterFromXYZ(cdist_r[, c('x', 'y', 'cdist')])
#'crs(r_cdist) <- CRS('+init=EPSG:4326')
#'plot(r_cdist)
#'sf::sf_use_s2(FALSE)
#'
#'#sample from raster
#'cdist_r_GSA20<-sample_raster(raster_FPI=r_cdist, df_grid=sample, LON="x", LAT="y")
#'cdist_r_GSA20 <- cdist_r_GSA20 %>% rename("cdist" = "layer")
#'
#'#plot data after sample
#'cdist_r_GSA20 <- rasterFromXYZ(cdist_r_GSA20[, c('LON', 'LAT', 'cdist')])
#'crs(cdist_r_GSA20) <- CRS('+init=EPSG:4326')
#'plot(cdist_r_GSA20)
#' @export
sample_raster<-function (raster_FPI=r_cdist, df_grid=sample, LON="x", LAT="y") 
{
  library(sf)
  df_grid$LON <- df_grid[[LON]]
  df_grid$LAT <- df_grid[[LAT]]
  grid <- as.data.frame(df_grid) %>% st_as_sf(coords = c(LON, 
                                                         LAT), crs = 4326, remove = FALSE)
  raster_FPIt <- raster_FPI #* 1e+05
  library(terra)
  raster_FPIt <- rast(raster_FPIt)
  polyFPIt = as.polygons(raster_FPIt, trunc = TRUE)
  sfpolyFPIt <- sf::st_as_sf(polyFPIt, crs = 4326, remove = FALSE)
  FPI <- st_join(grid, left = FALSE, sfpolyFPIt[1])
  FPI <- as.data.frame(FPI)
  FPI$FPI <- FPI[[ncol(FPI)-1]]
  FPI <- FPI %>% dplyr::select(LON,LAT,FPI)
  FPI <- FPI %>% rename("layer" = "FPI")
  return(FPI)
}

