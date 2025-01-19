#' Converts the estimated spatial landings' weight and value in the format of table H of the FDI data call
#'
#' Using the predicted spatial landings' weight and value in a csquare of 0.5 * 0.5 decimal degrees cell size to export a dataframe in the format of spatial landings (Table H) of the FDI data call.
#'
#'
#' @param data The dataframe derived in ERS level and includes information of landings' weight and value
#' @param country The name of the field in the dataframe containing the country
#' @param year The name of the field in the dataframe containing the year
#' @param quarter The name of the field in the dataframe containing the quarter
#' @param vessel_length The name of the field in the dataframe containing the vessel length
#' @param fishing_tech The name of the field in the dataframe containing the fishing technique
#' @param gear_type The name of the field in the dataframe containing the gear type
#' @param target_assemblage The name of the field in the dataframe containing the target assemblage
#' @param mesh_size_range Value for mesh_size_range
#' @param metier The name of the field in the dataframe containing the metier
#' @param metier_7 Value for metier_7
#' @param supra_region Value for supra_region (default is "MBS")
#' @param sub_region The name of the field of the dataframe containing the sub region (GSA)
#' @param eez_indicator Value for eez_indicator
#' @param geo_indicator Value for geo_indicator
#' @param specon_tech Value for specon_tech
#' @param deep Value for deep
#' @param rectangle_type Value for rectangle_type
#' @param latitude The name of the field in the dataframe containing the latitude
#' @param longitude The name of the field in the dataframe containing the longitude
#' @param c_square Value for c_square
#' @param species The name of the field in the dataframe containing the species
#' @param totwghtlandg The name of the field in the dataframe containing the landing weight
#' @param totvallandg The name of the field in the dataframe containing the landing value
#' @param confidential Value for confidential
#'  
#' @return Return a dataframe in the format of table H (spatial landings) of the FDI data call.
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
#'result_q_value<- landings_SSF(data=FPIe_a,Sub.region="Sub.region", Country="Country", 
#'                              FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                              quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                              metier="metier",parameter = "totvallandg",LON="LON",LAT="LAT")
#'
#'# Estimate landings weight by rectangle 0.5*0.5 decimal degrees
#'ERS_q_catch<-catch_ERSlevel(data_FPI=result_q_catch, ERS_poly=ERS_poly, parameter= "LW")
#'ERS_q_value<-catch_ERSlevel(data_FPI=result_q_value, ERS_poly=ERS_poly, parameter= "LW")
#'
#'library(dbplyr)
#'ERS_q_value<-ERS_q_value %>% 
#'  dplyr::rename(
#'    parameter2 = parameter
#'  )
#'
#'ERS_q_catch_value<-cbind(ERS_q_catch,ERS_q_value[15])
#'
#'#create table H for the data call
#'tableH_landings_SSF<-create_tableH(data=ERS_q_catch_value,country="Country",	year="year",	quarter="quarter",	vessel_length="vessel_length",
#'                                   fishing_tech="fishing_tech", gear_type="gear_type",	target_assemblage="target_assemblage",
#'                                   mesh_size_range="NK",	metier="metier",	metier_7="NK",	supra_region="MBS",
#'                                   sub_region="Sub.region",	eez_indicator="NA",	geo_indicator="NK",	specon_tech="NK",
#'                                   deep="NA",	rectangle_type="05*05",	latitude="csq_y",	longitude="csq_x",	c_square="NA",species="species",
#'                                   totwghtlandg="parameter",totvallandg="parameter2",	confidential="N")
#' @export
create_tableH<-function (data=ERS_gns_06_q_catch,country="Country",	year="year",	quarter="quarter",	vessel_length="vessel_length",
                         fishing_tech="fishing_tech", gear_type="gear_type",	target_assemblage="target_assemblage",
                         mesh_size_range="NK",	metier="metier",	metier_7="NK",	supra_region="MBS",
                         sub_region="Sub.region",	eez_indicator="NA",	geo_indicator="NK",	specon_tech="NK",
                         deep="NA",	rectangle_type="05*05",	latitude="csq_y",	longitude="csq_x",c_square="NA", species="species",	
                         totwghtlandg="parameter",	totvallandg="parameter2",confidential="N")
{
  library(sf)
  FPI <- data
  tableI<-NULL
  tableI$country <- FPI[[country]]
  tableI$year <- FPI[[year]]
  tableI$quarter <- FPI[[quarter]]  
  tableI$vessel_length <- FPI[[vessel_length]]
  tableI$fishing_tech <- FPI[[fishing_tech]]
  tableI$gear_type <- FPI[[gear_type]]
  tableI$target_assemblage <-FPI[[target_assemblage]]
  tableI$mesh_size_range <- mesh_size_range
  tableI$metier <- FPI[[metier]]
  tableI$metier_7 <- 	metier_7
  tableI$supra_region <- supra_region
  tableI$sub_region <- FPI[[sub_region]]
  tableI$eez_indicator <- eez_indicator
  tableI$geo_indicator <- geo_indicator
  tableI$specon_tech <- specon_tech
  tableI$deep <- deep
  tableI$rectangle_type <- rectangle_type
  tableI$latitude <- FPI[[latitude]]
  tableI$longitude <- FPI[[longitude]]
  tableI$c_square <- c_square
  tableI$species<- FPI[[species]]
  tableI$totwghtlandg <- FPI[[totwghtlandg]]
  tableI$totvallandg <- FPI[[totvallandg]]
  tableI$confidential <- confidential
  
  tableH<-as.data.frame(tableI)
  return(tableH)
}
