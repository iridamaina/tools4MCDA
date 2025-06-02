#' Estimate landing weight or value using Fishing effort and landings per unit effort as a proxy
#'
#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with Fisheries Dependent Information (FDI) data (Table A in the data call) to perform spatial estimations of fishing effort (days at sea). Subsequently, this information is combined with landings per unit effort that can be estimated through predictive models. Finally all the above information is combined with table A from FDI data call to estimate landing weight and landing value in a fine spatial scale (e.g. ERS grid). The function can be used for discards as well if this information is provided instead of LPUE.
#'
#' @param data  A data frame with FE estimations (e.g. by MCDA and FDI) and species LPUE. The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear (values- SSF, GNS, GTR, LLS), year, vessel_length_cat , quarter( values 1-4, or 0 for annual estimations), GSA (a field including information for the Geographical Subarea in the following format: GSA20, Country in the following format : Greece), FE (estimated fishing effort), LPUE(estimated landings per unit effort)
#' @param Sub.region  The field of GSAs included in data using "".
#' @param Country  The field of Country included in data using "".
#' @param FE the field in the FPI table that contains the fishing effort or the Fishing Pressure Index estimated from MCDA using ""
#' @param LPUE the field in the FPI table that contains the landings per unit effort by species using "". The values should be normalized in a scale: 0-1
#' @param species the field in the FPI table that contains the species (in 3 alpha code) using ""
#' @param table_a The data frame of the FDI "catches" by country. The above table is based on Table A of the data call of Fisheries Dependent Information -FDI and contains information about landing weight and value of landings.
#' @param gear The field in FPI table that contains the gear using "".Values: GNS, GTR, LLS
#' @param year The field in FPI table that contains the year using "".
#' @param quarter The field in FPI table that contains the quarter using "".
#' @param metier The field in FPI table that contains the metier using "".
#' @param fishing_tech The field in FPI table that contains the fishing technique using "".
#' @param target_assemblage The field in FPI table that contains the target assemblage using "".
#' @param vessel_length_cat The field in FPI table that contains the vessel length category using "". Values: NK, VL006, VL0010, VL0612, VL1012, VL1218, VL1824, VL2440, VL40XX
#' @param parameter The field from table a to be used for performing spatial estimations. Values: "totwghtlandg", "totvallandg"
#' @param LON Name of Longitude field in FPI table using "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FPI table "". The longitude should be in decimal degrees and in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame including FPI and Fishing days by grid cell.
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
#'
#'result_q_value<- landings_SSF(data=FPIe_a,Sub.region="Sub.region", Country="Country", 
#'                              FE="FPI", LPUE="LPUE", species="species", table_a=table_a,gear="gear_type",year="year",
#'                              quarter="quarter", vessel_length_cat="vessel_length",fishing_tech ="fishing_tech",target_assemblage="target_assemblage",
#'                              metier="metier",parameter = "totvallandg",LON="LON",LAT="LAT")
#' @export
landings_SSF <- function(data, Sub.region , Country , FE , LPUE, species, table_a, gear, year,quarter,
                         metier, fishing_tech, target_assemblage,vessel_length_cat, parameter= "totwghtlandg",LON, LAT) {
  
  
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
  FPI$fishing_tech <- FPI[[fishing_tech]]
  FPI$gear <- FPI[[gear]]
  FPI$target_assemblage<- FPI$target_assemblage
  FPI$metier <- FPI[[metier]]
  FPI$proxyc<-(FPI$FE*FPI$LPUE)
 table_a$parameter<- table_a[[parameter]]
  
  FPI<- as.data.frame(FPI) %>% 
    st_as_sf(coords=c(LON,LAT), crs=4326, remove=FALSE)  
  
  print("Please ensure that these rules are followed: 
        1)The fishing gears that are currently supported are GNS, GTR, LLS, 
        2)Value for <quarter> should be 1-4, 
        3)Information for the vessel legth category is in the required format")
  print("LPUE can support predicted values based on spatiotemporal models of abundance by species. 
        In case that this information is not available please inset the value 1")
  
  FPI2<-unite(FPI, col='joinFDI', c('Country','year','quarter', 'vessel_length_cat','fishing_tech','gear','target_assemblage','metier','Sub.region', 
                                      'species',), sep='_',remove = FALSE)
  table_a2<-unite(table_a, col='joinFDI', c('country','year','quarter','vessel_length','fishing_tech','gear_type','target_assemblage','metier','sub_region', 
                                             'species'), sep='_')
  
  # # total days as numeric
  table_a_n<-table_a2[table_a2$parameter!="NK",]
    table_a_n<-table_a_n[table_a_n$parameter!="",]
    table_a_n$parameter<-as.numeric(table_a_n$parameter)
 
  #aggreegate by joinFID
  table_a_t_n<-aggregate(parameter~joinFDI, FUN=sum, data=table_a_n)
  
  #########merge FPI_FDI with table_g_effort
  FPI_FDI_m<-merge(x=FPI2, y=table_a_t_n, by.x="joinFDI")
  
  #estimate pLWc
    pFPI2<-aggregate(proxyc~joinFDI, FUN=sum, data=FPI_FDI_m)
  
  #########merge FPI_FDI_m with pFPI
  FPI_FDI_m_p<-merge(x=FPI_FDI_m, y=pFPI2, by="joinFDI")
  
  ####### main formula
  
  FPI_FDI_m_p$LW<-(FPI_FDI_m_p$parameter * (FPI_FDI_m_p$proxyc.x/FPI_FDI_m_p$proxyc.y))
  
  FPI_FDI_m_p_df<-as.data.frame(FPI_FDI_m_p)
  
  return(FPI_FDI_m_p_df)
}
