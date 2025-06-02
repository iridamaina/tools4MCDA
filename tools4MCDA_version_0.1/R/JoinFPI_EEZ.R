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
#'#Read_grid4MCDA (A reference grid for performing the analysis)
#'data("dfcut2")
#'#dfcut2<-read.csv2(file = paste0(general$input_data,"/mcda/grid_MEDBS_005.txt"), header = TRUE, sep= ",")
#'
#'##MASK the reference grid by country for performing the analysis
#'#Please upload a more refined polygon of your country's Exlusive Economic Zone or fishing area
#'
#'#Read EEZ polygons
#'EEZ_poly_path<-paste0("--PATH TO EEZ data--")
#'EEZ_poly_layer_name<-"EEZ_GRC"
#'EEZ_poly<- read_sf(dsn = EEZ_poly_path, layer = EEZ_poly_layer_name)
#'
#'#mask reference grid with EEZ and extract a csv
#'dfcut2_EEZ<-joinFPI_EEZ(data_FPI=dfcut2,EEZ_poly = EEZ_poly,field_country="TERRITORY1",LON="x",LAT="y")
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
