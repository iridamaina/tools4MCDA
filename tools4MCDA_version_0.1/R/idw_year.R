#' Perform spatial interpolation Inverse Distance weighted by year
#'
#' Perform spatial interpolation Inverse Distance weighted by year
#'
#'
#' @param data a dataframe with Activity index (i.e. a proxy of capacity Sum of L*GT) by registration port, geographical coordinates (x,y) and year 
#' @param parameter The field of the dataframe that will perform the interpolation
#' @param p power
#' @param R radious
#' @param N number of neighbors
#' @param method method of interpolation default is "Shepard"
#' @param dfcut a grid to include the interpolated values
#'  
#' @return Return rasters with interpolated values
#' @author I. Maina 
#' @examples
#'library(tools4MCDA)
#'library(tidyr)
#'library(raster)
#'
#'data("fleet_reg_year")
#'data("dfcut2")
#'
#'#DEFINE COUNTRY, GSA, year_start, year_end
#'country<-"GRC"
#'GSA<-20
#'
#'#crop the area
#'dfcut2$lon<-as.numeric(dfcut2$x)
#'dfcut2$lat<-as.numeric(dfcut2$y)
#'dfcut2<-joinFPI_GSA(data_FPI=dfcut2,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="x",LAT="y")
#'dfcut2<-dfcut2[dfcut2$SMU_CODE %in% GSA,]
#'
#'#Activity Index (Ac) estimation
#'
#'# unite gear_type and vessel_length
#'fleet_reg_year<-unite(fleet_reg_year, col='key', c('MAIN_FISHING_GEAR', 'vessel_length'), sep='_', remove=FALSE)
#'# select country
#'fleet_reg_year<-fleet_reg_year[fleet_reg_year[1]==country,]
#'
#'#Preparing the data for Ac estimation loop by fishing gear and vessel length categories
#'fl<-"GNS_VL0006"
#'fleet<-fleet_reg_year[fleet_reg_year$key==fl,]
#'fleet<-fleet[fleet$COUNTRY==country,]
#'fleet$x<-as.numeric(fleet$LONGITUDE)
#'fleet$y<-as.numeric(fleet$LATITUDE)
#'fleet$SUM_LOA_GT<-as.numeric(fleet$SUM_LOA_GT)
#'fleet$YEAR<-as.integer(fleet$YEAR)
#'ye<-2019
#'
#'#IDW by year
#'Ac_year<-idw_year(data=fleet,field="SUM_LOA_GT",years=ye, p=2,R=2,N=15,method="Shepard",dfcut=dfcut2,crs_raster='+init=EPSG:4326')
#'
#'#plot
#'plot(stack(Ac_year))
#' @export
idw_year<-function(data,field="kg_km2",years=ye, p=2,R=2,N=15,method="Shepard",dfcut=dfcut,crs_raster='+init=EPSG:4326'){
  
p=p
R=R
N=N
method = method

data$field <-data[[field]]

r<-rasterFromXYZ(dfcut[, c('lon', 'lat')])
crs(r) <- CRS(paste(crs_raster))

p1<-NULL
rx1 <- list()
rx1<-NULL
i<-1
int4<-NULL   

for (yy in years){ 
  data1<-data[data$YEAR==yy,]
  int <- phylin::idw(data1$field, data1[, c("x","y")], dfcut[, c("lon","lat")], method, p, R, N)
  int3<-cbind(dfcut[, c("lon","lat")],int)
  int3$YEAR<-yy
  int4<-rbind(int4,int3)
  rx1[[i]] <- rasterize(dfcut[, c("lon","lat")], field = int$Z, r,update = TRUE)
  rx1[[i]]@file@name<-as.character(yy)
  i<-i+1
}
  return(rx1)
}

