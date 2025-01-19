#' Convert the estimated landings weight or value for SSF expressed in tonnes or euro using Fishing Pressure Index and Landings Per Unit of Effort as a proxy in a grid cell of 0.5*0.5 decimal degrees (csquare) in order to inform Table H.
#'
#' Convert in csquare (0.5 * 0.5 dd) the combined outcome resulted based on FPI (estimated by MCDA), the LPUE and the Fisheries Dependent Information (FDI) data (Table A in the data call) to perform spatial estimations of landings weight or value and inform the Table H.
#' 
#' @param data_FPI  A data frame with Fishing effort estimations after running the function FDays_SSF. The above data frame should contain the fields: Fishing Effort, LON (longitude in WGS84), LAT (latitude in WGS84), gear (values: GNS, GTR, LLS), year, vessel_length_cat (values: "VL0006","VL0010","VL0612",), quarter (values: 1-4), GSA (a field including information for the Geographical Subarea in the following format: GSA20, Country in the following format : GRC)
#' @param ERS_poly  A polygon shapefile with ERS rectangle (csquare of 0.5*0.5) in an 'sf' format
#' @param Country  The field of Country included in data using "".
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LW Name of the field that contains the landings weight or value in the data table using "".
#'  
#' @return Return of a data.frame including landings weight or value by grid cell of 0.5*0.5 decimal degrees to be used for the creation of table H.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'
#'#load Data
#'data(FPIe_a)
#'data(ERS_poly)
#'
#'# Define country
#'FPIe_a$Country<-"GRC"
#'
#'# Estimate landings weight in a fine scale
#'result_q_catch<- landings_SSF(data=FPIe_a,Sub.region="Sub.region", Country="Country", 
#'                              FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                              quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                              metier="metier",parameter = "totwghtlandg",LON="LON",LAT="LAT")
#'
#'# Estimate landings weight by rectangle 0.5*0.5 decimal degrees
#'ERS_q_catch<-catch_ERSlevel(data_FPI=result_q_catch, ERS_poly=ERS_poly, parameter= "LW")
#' @export
catch_ERSlevel<-function (data_FPI, ERS_poly, LON="LON",LAT="LAT",parameter= "LW") 
{
  library(sf)
  FPI <- data_FPI
  FPI$LON <- FPI[[LON]]
  FPI$LAT <- FPI[[LAT]]
  FPI$parameter <- FPI[[parameter]]
  FPI <- as.data.frame(FPI) %>% st_as_sf(coords = c(LON, LAT), 
                                         crs = 4326, remove = FALSE)
  FPI <- st_join(FPI, left = FALSE, ERS_poly)
  #FPI$ERS_CD <- FPI$ERS_CD
  FPI <- as.data.frame(FPI)
  FPI <- FPI %>% dplyr::select(-geometry)
  FPI_ERS<-aggregate(data=FPI,cbind(parameter)~ Country + year+quarter+vessel_length+fishing_tech+gear_type+target_assemblage+metier+
                       Sub.region+csq_x+csq_y+cscode+CELLCODE+species, FUN=sum)
  
  return(FPI_ERS)
}
