#' Convert rasters to dataframe by year
#'
#' Convert rasters to dataframe by year
#'
#'
#' @param data_interpolation rasters to be converted in csv 
#' @param years a numeric object including the years
#' @param name name of the data to be extracted #not used
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
#'#export IDW by year to a data frame
#'SSF_ac<-export_to_csv5(data_interpolation=Ac_year,years=ye,name="idw_Ac")
#' @export

export_to_csv5<-function(data_interpolation=idw_rasters,years=ye,name="idw_rasters"){
  
  
  data_y<-NULL
  data_f<-NULL
  dataall<-NULL
  j<-1
    for (j in 1:length(years)){
    data_y<-data_interpolation[[j]]
    data_f<-as.data.frame( rasterToPoints(data_y) )
    data_f$year<-years[j]
    dataall<-rbind(data_f,dataall)
     }
 # }
  return(dataall)
    }
 
