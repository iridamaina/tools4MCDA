#' Compare fishing effort in days at sea estimated using Fishing Pressure Index (from MCDA) as a proxy with table_A on landings estimations by GSA/gear/metier/quarter/fishing_tech/target_assemblage/year/country/species
#'
#' Use the produced information that combines Fishing effort estimations expressed in fishing days with Fishing Pressure Index (FPI- estimated by MCDA) to perform comparisons with Fisheries Dependent Information (FDI) data (Table A in the data call) by GSA/gear/metier/quarter/fishing_tech/target_assemblage/year/country/species
#'
#'
#' @param data  A data frame with FE estimations (e.g. by MCDA and FDI) and species LPUE produced after running the tools4MCDA::landings_SSF function. The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear (values: GNS, GTR, LLS), year, vessel_length_cat , quarter(values 1-4), GSA (a field including information for the Geographical Subarea in the following format: GSA20, Country in the following format : GRC), FE (estimated fishing effort or the Fishing Pressure Index by MCDA), LPUE(estimated landings per unit effort)
#' @param Sub.region  The field of GSAs included in data using "".
#' @param Country  The field of Country included in data using "".
#' @param FE The field in the FPI table that contains the fishing effort or the Fishing Pressure Index by MCDA using ""
#' @param LPUE The field in the FPI table that contains the Landings Per Unit Effort using ""
#' @param species The field in the FPI table that contains the species using ""
#' @param table_a The data frame of the FDI "catches" (landing weight, landing value, discard weight) by country.csv. The above table is based on Table A in the data call (Fisheries Dependent Information (FDI) data and contains information about landing weight and value of landings)
#' @param gear The field in FPI table that contains the gear using "". Values: SSF, GNS, GTR, LLS
#' @param year The field in FPI table that contains the year using "".
#' @param quarter The field in FPI table that contains the quarter using ""
#' @param metier The field in FPI table that contains the metier using ""
#' @param fishing_tech The field in FPI table that contains the fishing technique using ""
#' @param target_assemblage The field in FPI table that contains the target assemblage using ""
#' @param vessel_length_cat The field in FPI table that contains the vessel length category using "". Values: VL006, VL0010, VL0612
#' @param parameter The field from table A to be used for performing spatial estimations. Values: "totwghtlandg", "totvallandg"
#' @param LW The field in the FPI table that contains the total weight of landings or value by species using "". This is estimated from tools4MCDA::landings_SSF (NOT REVISE)
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The latitude should be in decimal degrees and in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame including landing weight or value.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'library(ggplot2)
#'
#'data(FPIe_a)
#'data(table_a)
#'
#'FPIe_a$Country<-"GRC"
#'
#'#Estimate catch from FPI, table a (GSA, country, mandatory fields)
#'result_q_catch<- landings_SSF(data=FPIe_a,Sub.region="Sub.region", Country="Country", 
#'                              FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                              quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                              metier="metier",parameter = "totwghtlandg",LON="LON",LAT="LAT")
#'
#'result_q_value<- landings_SSF(data=FPIe_a,Sub.region="Sub.region", Country="Country", 
#'                              FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                              quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                              metier="metier",parameter = "totvallandg",LON="LON",LAT="LAT")
#'
#'#check Landing weight result
#'check_weight<-check_landings(data=result_q_catch,Sub.region="Sub.region", Country="Country", 
#'                           FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                           quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                           metier="metier",LON="LON",LAT="LAT",parameter = "totwghtlandg", LW="LW")
#'
#'#check Landing value result
#'check_value<-check_landings(data=result_q_value,Sub.region="Sub.region", Country="Country", 
#'                            FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                            quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                          metier="metier",LON="LON",LAT="LAT",parameter = "totvallandg", LW="LW")
#'
#'
#' @export

check_landings <- function(data, Sub.region , Country , FE , LPUE, species, table_a, gear, year,quarter,
                           metier, fishing_tech, target_assemblage,vessel_length_cat, parameter= "totwghtlandg",LW,LON, LAT) {
  
  
  library(sf)
  library(tidyr)
  FPI<-data
  FPI$year<-FPI[[year]]
  FPI$quarter<-FPI[[quarter]]
  FPI$vessel_length_cat<-FPI[[vessel_length_cat]]
  FPI$FE<-FPI[[FE]]
  FPI$LPUE<-FPI[[LPUE]]
  FPI$LON<-FPI[[LON]]
  FPI$LAT<-FPI[[LAT]]
  FPI$Sub.region<-FPI[[Sub.region]]
  FPI$Country<-FPI[[Country]]
  FPI$proxyc<-(FPI$FE*FPI$LPUE)
  FPI$fishing_tech <- FPI[[fishing_tech]]
  FPI$target_assemblage <-FPI[[target_assemblage]]
  FPI$gear <- FPI[[gear]]
  FPI$metier <- FPI[[metier]]
   # parameter<- parameter
  FPI$quarter<-FPI[[quarter]]
  table_a$parameter<- table_a[[parameter]]
  FPI$LW<-FPI[[LW]]
  
  
  
  FPI<- as.data.frame(FPI) %>% 
    st_as_sf(coords=c(LON,LAT), crs=4326, remove=FALSE)  
  
  FPI2<-unite(FPI, col='joinFDI', c('Sub.region', 'Country', 'gear', 'year', 'vessel_length_cat', 'species','quarter','fishing_tech','metier','target_assemblage'), sep='_',remove = FALSE)
  table_a2<-unite(table_a, col='joinFDI', c('sub_region', 'country', 'gear_type', 'year', 'vessel_length', 'species', 'quarter','fishing_tech','metier','target_assemblage'), sep='_')
  
  # # total days as numeric
  table_a_n<-table_a2[table_a2$parameter!="NK",]
  table_a_n<-table_a_n[table_a_n$parameter!="",]
  
  table_a_n$parameter<-as.numeric(table_a_n$parameter)
  
  #aggreegate by joinFID
  table_a_t_n<-aggregate(parameter~joinFDI, FUN=sum, data=table_a_n)
  #show(table_a_t_n)
  pFPI2<-aggregate(LW~joinFDI, FUN=sum, data=FPI2)
  #show(pFPI2)
  #########merge FPI_FDI with table_g_effort
  FPI_FDI_m<-NULL
  FPI_FDI_m<-merge(x=pFPI2, y= table_a_t_n, by.x="joinFDI")
  
  #check field
  FPI_FDI_m$check<-(FPI_FDI_m$LW-FPI_FDI_m$parameter)
  
  FPI_FDI_m <- FPI_FDI_m %>% 
    rename("Landings_table_A"="LW",
           "Landings_estimated" ="parameter" ,
           "difference"="check" )
  FPI_FDI_m$landings_parameter<-parameter
  print("If <difference> values are zero no difference is occured between the two datasets")
  
  show(FPI_FDI_m)
  
  return(FPI_FDI_m)
}
