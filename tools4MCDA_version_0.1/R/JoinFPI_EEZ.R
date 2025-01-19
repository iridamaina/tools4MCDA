#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with EEZ polygons to estimate the country
#'
#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with EEZ polygons to estimate the country.
#'
#' @importFrom stats stepfun
#'
#' @param EEZ_poly  The sf polygon of EEZ.
#' @param field_country The field that contains numerical information for the country using "" e.g. "Territory1" .
#' @param data_FPI  A data frame with FPI estimations. The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear:(values- SSF, GNS, GTR, LLS), year, vessel_length_cat (values-all, +++), quarter( values 1-4, or 0 for annual estimations)
#' @param LON Name of Longitude field in FDI table. The longitude should be in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FDI table. The longitude should be in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame of FPI with information by GSA.
#' @author I. Maina
#' @examples
#' #read MCDA result
#' mydata_r<-read.table(file="/data.csv", header=T, sep=";")
#' 
#' s_gns_06_q<- mydata_r
#' s_gns_06_q$VESSEL_LENGTH<-"VL0006"
#' s_gns_06_q$GEAR_TYPE<-"GNS"
#' s_gns_06_q$YEAR<-2018
#' 
#' #Read GSA polygons
#' GSA_poly_path<-"D:/data"
#' GSA_poly_layer_name<-"GSAs_simplified"
#' GSA_poly<- read_sf(dsn = GSA_poly_path, layer = GSA_poly_layer_name)
#' 
#' #Read EEZ polygons
#' EEZ_poly_path<-"D:/data"
#' EEZ_poly_layer_name<-"Greek_EEZ"
#' EEZ_poly<- read_sf(dsn = EEZ_poly_path, layer = EEZ_poly_layer_name)
#' 
#' #Read table g 
#' table_g_effort_path<-"D:/FDI Effort by country.csv"
#' table_g_effort<-read.table(file = table_g_effort_path, header = TRUE, sep= ",")
#'  
#' #Join FPI with GSA
#' FPId<-joinFPI_GSA(data_FPI=data,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="X_DD",LAT="Y_DD")
#' 
#' #Join FPI with EEZ
#' FPIe<-joinFPI_EEZ(data_FPI=FPIg,EEZ_poly = EEZ_poly,field_country="Territory1",LON="X_DD",LAT="Y_DD")
#' 
#' #Estimate Fishing Days from FPI, table g (GSA, country, mandatory fields)
#' result_s_gns_06_q<- Dsea_SSF(data=FPIe,Sub.region="Sub.region", Country="Country", FPI="gns06", table_g_effort=table_g_effort,vessel_length_cat="VESSEL_LENGTH",year="YEAR", LON="X_DD", LAT="Y_DD")
#' @export
joinFPI_EEZ <- function(data_FPI, EEZ_poly , field_country="Territory1", LON, LAT) {
  library(sf)
  FPI<-data_FPI
  FPI$LON<-FPI[[LON]]
  FPI$LAT<-FPI[[LAT]]
  FPI<- as.data.frame(FPI) %>% 
    st_as_sf(coords=c(LON,LAT), crs=4326, remove=FALSE)  
  
  # For POINTS that fall within CA_counties, adds ATTRIBUTES, retains ALL pts if left=TRUE, otherwise uses inner_join
  FPI <- st_join(FPI, left = FALSE, EEZ_poly[field_country]) # join points EEZ
  FPI$Country<-FPI$Territory1
  FPI<-as.data.frame(FPI)
  FPI<-FPI%>% 
    dplyr::select(-geometry)
  
  return(FPI)
}
