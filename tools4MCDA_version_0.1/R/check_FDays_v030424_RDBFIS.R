#' Compare fishing effort in days at sea estimated using Fishing Pressure Index (from MCDA) as a proxy and table_G on fishing effort estimations by GSA/gear/metier/quarter/fishing_tech/target_assemblage/year/country
#'
#' Combine Fishing effort estimations expressed in fishing days with Fishing Pressure Index (FPI- estimated by MCDA) with Fisheries Dependent Information (FDI) data (Table G in the data call) to perform comparisons by of the outcomes by GSA/gear/metier/quarter/fishing_tech/target_assemblage/year/country.
#'
#'
#' @param data  A data frame with Fishing effort estimations in Days at Sea in fine spatial scale. The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear(values: GNS, GTR, LLS), year, vessel_length_cat (values:VL006, VL0010,VL0612), quarter( values 1-4), GSA (a field including information for the Geographical Subarea in the following format: GSA20), Country in the following format (GRC)++
#' @param Sub.region  The field of GSAs included in data using "".
#' @param Country  The field of Country included in data using "".
#' @param FDays The field that contains information expressed in days at sea in a fine scale included the data table using "". 
#' @param table_g_effort The data frame of the FDI Effort by country.csv. The above table is based on Table G in the data call (Fisheries Dependent Information (FDI) data)
#' @param gear The field in FPI table that contains the gear using "".Values: GNS, GTR, LLS
#' @param metier The field in FPI table that contains the metier using "".
#' @param fishing_tech The field in FPI table that contains the fishing technique using "".
#' @param target_assemblage The field in FPI table that contains the target assemblage using "".
#' @param year The field in FPI table that contains the year using "".
#' @param quarter The field in FPI table that contains the quarter using "".
#' @param vessel_length_cat The field in FPI table that contains the vessel length category using "". Values: VL006, VL0010, VL0612
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The latitude should be in decimal degrees and in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame including FPI and Fishing days by grid cell.
#' @author I. Maina
#' @examples
#' library(tools4MCDA)
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
#'result_q<- FDays_SSF(data=FPIe,Sub.region="Sub.region", Country="Country", FPIc="FPI", 
#'                     table_g_effort=table_g_effort,vessel_length_cat="vessel_length",
#'                     year="year", quarter="quarter",gear="gear_type",fishing_tech="fishing_tech",
#'                     target_assemblage="target_assemblage", metier="metier",LON="LON", LAT="LAT")
#'
#'#check Days at sea result
#'check_Fdays<-check_FDays(data=result_q,Sub.region="Sub.region",Country="Country",FDays="FDays",
#'                         table_g_effort=table_g_effort,gear="gear_type",year="year",quarter="quarter",
#'                         vessel_length_cat="vessel_length",fishing_tech="fishing_tech", target_assemblage="target_assemblage",
#'                         metier="metier",LON="LON",LAT="LAT")
#' @export

check_FDays<-function (data, Sub.region, Country, FDays, table_g_effort, gear, metier, fishing_tech,target_assemblage,
          year, quarter, vessel_length_cat, LON, LAT) 
{
  library(sf)
  FPI <- data
  FPI$year <- FPI[[year]]
  FPI$vessel_length_cat <- FPI[[vessel_length_cat]]
  FPI$FDays <- FPI[[FDays]]
  FPI$LON <- FPI[[LON]]
  FPI$LAT <- FPI[[LAT]]
  FPI$Sub.region <- FPI[[Sub.region]]
  FPI$metier <- FPI[[metier]]
  FPI$gear <- FPI[[gear]]
  FPI$fishing_tech <- FPI[[fishing_tech]]
  FPI$target_assemblage <-FPI[[target_assemblage]]
  FPI$quarter <- FPI[[quarter]]
  FPI$Country <- FPI[[Country]]
  FPI <- as.data.frame(FPI) %>% st_as_sf(coords = c(LON, LAT), 
                                         crs = 4326, remove = FALSE)

  library(tidyr)
  library("dplyr")
  #print("Please ensure that the above rules are followed: 1) The fishing gears that are currently supported are GNS, GTR, LLS, 2)Value for <quarter> should be 1-4, 3)Information for the vessel legth category is included in the required format")
  
  FPI2<-unite(FPI, col = "joinFDI",c("Country","year","quarter",
                                     "vessel_length_cat", "fishing_tech","gear","target_assemblage","metier","Sub.region"),
              sep = "_",remove = FALSE)
  table_g_effort2 <- unite(table_g_effort, col = "joinFDI", c("country","year","quarter",
                                                              "vessel_length","fishing_tech","gear_type","target_assemblage","metier", "sub_region"),sep = "_")                    
  
  table_g_effort_n <-table_g_effort2[table_g_effort2$totseadays != "NK", ]
  table_g_effort_n <-table_g_effort2[table_g_effort2$totseadays != "", ]
  table_g_effort_n$totseadays<- table_g_effort_n$totseadays
  
  
  table_g_effort_t_n <- aggregate(totseadays ~ joinFDI, 
                                  FUN = sum, data = table_g_effort_n)
  #show(table_g_effort_t_n)
  FPI2$FDays<-as.numeric(FPI2$FDays)
  pFPI2 <- aggregate(FDays ~ joinFDI, FUN = sum, data = FPI2)
  #show(pFPI2)
  FPI_FDI_m <- NULL
  FPI_FDI_m <- merge(x = pFPI2, y = table_g_effort_t_n, by.x = "joinFDI")
  FPI_FDI_m$check <- (FPI_FDI_m$FDays - FPI_FDI_m$totseadays)
  
  FPI_FDI_m <- FPI_FDI_m %>% 
    rename("FDays_table_G"="totseadays",
           "FDays_estimated" ="FDays" ,
           "difference"="check" )
  
  print("if the <difference> value is zero, no difference is occured between the two datasets.")
  show(FPI_FDI_m)
  return(FPI_FDI_m)
}

