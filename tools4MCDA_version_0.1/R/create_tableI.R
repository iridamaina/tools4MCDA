#' Converts the estimated spatial fishing effort in the format of table I of the FDI data call
#'
#' Using the predicted spatial fishing effort in a csquare of 0.5 * 0.5 decimal degrees cell size to export a dataframe in the format of spatial effort (Table I) of the FDI data call.
#'
#'
#' @param data The dataframe derived in ERS level and includes information of effort
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
#' @param sub_region The name of the field in the dataframe containing the sub region (GSA)
#' @param eez_indicator Value for eez_indicator
#' @param geo_indicator Value for geo_indicator
#' @param specon_tech Value for specon_tech
#' @param deep Value for deep
#' @param rectangle_type Value for rectangle_type
#' @param latitude The name of the field in the dataframe containing the latitude
#' @param longitude The name of the field in the dataframe containing the longitude
#' @param c_square Value for c_square
#' @param species The name of the field in the dataframe containing the species
#' @param totfishdays The name of the field in the dataframe containing the fishing days
#' @param confidential Value for confidential
#'  
#' @return Return a dataframe in the format of table I (spatial effort) of the FDI data call.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'
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
#'
#'#create table I in the data call format
#'tableI_seffort_SSF<-create_tableI(data=ERS_q,country="Country",	year="year",	quarter="quarter",	vessel_length="vessel_length",
#'                                  fishing_tech="fishing_tech", gear_type="gear_type",	target_assemblage="target_assemblage",
#'                                  mesh_size_range="NK",	metier="metier",	metier_7="NK",	supra_region="MBS",
#'                                  sub_region="Sub.region",	eez_indicator="NA",	geo_indicator="NK",	specon_tech="NK",
#'                                  deep="NA",	rectangle_type="05*05",	latitude="csq_y",	longitude="csq_x",	c_square="NA",	
#'                                  totfishdays="parameter",confidential="N")
#' @export
create_tableI<-function (data=ERS_gns_06_q,country="Country",	year="year",	quarter="quarter",	vessel_length="vessel_length",
                         fishing_tech="fishing_tech", gear_type="gear_type",	target_assemblage="target_assemblage",
                         mesh_size_range="NK",	metier="metier",	metier_7="NK",	supra_region="MBS",
                         sub_region="Sub.region",	eez_indicator="NA",	geo_indicator="NK",	specon_tech="NK",
                         deep="NA",	rectangle_type="05*05",	latitude="csq_y",	longitude="csq_x",	c_square="NA",	
                         totfishdays="parameter",	confidential="N")
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
  tableI$target_assemblage <- FPI[[target_assemblage]]
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
  tableI$totfishdays <- FPI[[totfishdays]]
  tableI$confidential <- confidential
 
  tableI<-as.data.frame(tableI)
  return(tableI)
}
