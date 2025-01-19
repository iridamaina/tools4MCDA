#' Convert the estimated fishing effort for SSF expressed in days at sea using Fishing Pressure Index as a proxy in a grid cell of 0.5*0.5 decimal degrees (csquare) in order to inform Table I.
#'
#' Convert in csquare (0.5 * 0.5 dd) the combined Fishing Pressure Index (FPI- estimated by MCDA) with Fisheries Dependent Information (FDI) data (Table G in the data call) to perform spatial estimations of fishing effort (days at sea) and inform the Table I.
#' 
#' @param data_FPI  A data frame with Fishing effort estimations after running the function FDays_SSF. The above data frame should contain the fields: Fishing Effort, LON(longitude in WGS84), LAT(latitude in WGS84), gear:(values: GNS, GTR, LLS), year, vessel_length_cat (values: "VL0006","VL0010","VL0612",), quarter (values: 1-4), GSA (a field including information for the Geographical Subarea in the following format: GSA20, Country in the following format : GRC)
#' @param ERS_poly  A polygon shapefile with ERS rectangle (csquare of 0.5*0.5) in an 'sf' format
#' @param Country  The field of Country included in data using "".
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param FDays Name of the fishing days in the data table in "".
#'  
#' @return Return of a data.frame including Fishing days by grid cell of 0.5*0.5 decimal degrees to be used for the creation of table I.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)

#'#load Data
#'data(FPIe)
#'data(ERS_poly)
#'
#'# Define country
#'FPIe$Country<-"GRC"
#'
#'# Estimate Fishing Days in a fine scale
#'result_q<- FDays_SSF(data=FPIe,Sub.region="Sub.region", Country="Country", FPIc="FPI", 
#'                     table_g_effort=table_g_effort,vessel_length_cat="vessel_length",
#'                     year="year", quarter="quarter",gear="gear_type",fishing_tech="fishing_tech",
#'                     target_assemblage="target_assemblage", metier="metier",LON="LON", LAT="LAT")
#'
#'# Estimate Fishing Days by rectangle 0.5*0.5 decimal degrees
#'ERS_q<-Fdays_ERSlevel(data_FPI=result_q, ERS_poly=ERS_poly, LON="LON",LAT="LAT",parameter= "FDays")
#' @export
Fdays_ERSlevel<-function (data_FPI, ERS_poly, LON="LON",LAT="LAT",parameter= "FDays") 
{
  library(sf)
  FPI <- data_FPI
  FPI$LON <- FPI[[LON]]
  FPI$LAT <- FPI[[LAT]]
  FPI$parameter <- FPI[[parameter]]
  FPI <- as.data.frame(FPI) %>% st_as_sf(coords = c(LON, LAT), 
                                         crs = 4326, remove = FALSE)
  FPI <- st_join(FPI, left = FALSE, ERS_poly)
  FPI <- as.data.frame(FPI)
  FPI <- FPI %>% dplyr::select(-geometry)
  FPI_ERS<-aggregate(data=FPI,cbind(parameter)~ Country + year+quarter+vessel_length+fishing_tech+gear_type+target_assemblage+metier+
                       Sub.region+csq_x+csq_y+cscode+CELLCODE, FUN=sum)

  return(FPI_ERS)
}
