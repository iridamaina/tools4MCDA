#' Estimate fishing effort for SSF expressed in days at sea using Fishing Pressure Index as a proxy
#'
#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with Fisheries Dependent Information (FDI) data (Table G in the data call) to perform spatial estimations of fishing effort (days at sea).
#'
#' 
#' @param data  A data frame with FPI estimations (MCDA). The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear(values: GNS, GTR, LLS), year, vessel_length_cat (values: "VL0006","VL0612"), quarter(values: 1-4), GSA (a field including information for the Geographical Subarea in the following format: GSA20, Country in an Alpha 3 code format : GRC)
#' @param Sub.region  The field of GSAs included in data using "".
#' @param Country  The field of Country included in data using "".
#' @param FPIc  The field of Fishing pressure Index estimated by MCDA and included in data using "".
#' @param table_g_effort a dataframe similar to Table G (as in the FDI data call)
#' @param gear The field in FPI table that contains the gear using "".Values: SSF, GNS, GTR, LLS
#' @param metier The field in FPI table that contains the metier using "". Values should be the same as in table G 
#' @param year The field in FPI table that contains the year using "".
#' @param quarter The field in FPI table that contains the quarter using "". Values: 1-4.
#' @param vessel_length_cat The field in FPI table that contains the vessel length category using "". Values: VL006, VL0612
#' @param fishing_tech The field in FPI table that contains the fishing_tech using "". Values should be the same as in table G 
#' @param target_assemblage The field in FPI table that contains the target assemblage using "". Values should be the same as in table G 
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame including FPI and Fishing days by grid cell.
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
#' @export


FDays_SSF<-function (data, Sub.region, Country, FPIc, table_g_effort, gear, metier,
          year,quarter,vessel_length_cat,fishing_tech,target_assemblage, LON, LAT) 
{
 # library(sf)
  FPI <- data
  FPI$year <- FPI[[year]]
  FPI$vessel_length_cat <- FPI[[vessel_length_cat]]
  FPI$FPI <- FPI[[FPIc]]
  FPI$LON <- FPI[[LON]]
  FPI$LAT <- FPI[[LAT]]
  FPI$Sub.region <- FPI[[Sub.region]]
  FPI$Country <- FPI[[Country]]
  FPI$fishing_tech <- FPI[[fishing_tech]]
  FPI$target_assemblage <- FPI[[target_assemblage]]
  FPI$gear <- FPI[[gear]]
  FPI$metier <- FPI[[metier]]
  FPI$quarter <- FPI[[quarter]]
  FPI <- as.data.frame(FPI) %>% st_as_sf(coords = c(LON, LAT), 
                                         crs = 4326, remove = FALSE)
  library(tidyr)
  print("Please ensure that these rules are followed: 
        1) The fishing gears that are currently supported are GNS, GTR, LLS, 
        2) Value for <quarter> should be 1-4, 
        3) Information for the vessel legth category is included in the required format")
   
  FPI2<-unite(FPI, col = "joinFDI",c("Country","year","quarter",
                                     "vessel_length_cat", "fishing_tech","gear","target_assemblage","metier","Sub.region"),
  sep = "_",remove = FALSE)
  table_g_effort2 <- unite(table_g_effort, col = "joinFDI", c("country","year","quarter",
                             "vessel_length","fishing_tech","gear_type","target_assemblage","metier", "sub_region" ),sep = "_")                    
  
  table_g_effort_n <-table_g_effort2[table_g_effort2$totseadays != "NK", ]
  table_g_effort_n <-table_g_effort2[table_g_effort2$totseadays != "", ]
  table_g_effort_n$totseadays <- as.numeric(table_g_effort_n$totseadays)
  
  table_g_effort_t_n <- aggregate(totseadays ~ joinFDI, 
                                   FUN = sum, data = table_g_effort_n)
  FPI_FDI_m <- merge(x = FPI2, y = table_g_effort_t_n, by.x = "joinFDI")
  pFPI2 <- aggregate(FPI ~ joinFDI, FUN = sum, data = FPI_FDI_m)
  FPI_FDI_m_p <- merge(x = FPI_FDI_m, y = pFPI2, by = "joinFDI")
  FPI_FDI_m_p$FDays <- (FPI_FDI_m_p$totseadays * (FPI_FDI_m_p$FPI.x/FPI_FDI_m_p$FPI.y))
  FPI_FDI_m_p_df <- as.data.frame(FPI_FDI_m_p)
  return(FPI_FDI_m_p_df)
}
