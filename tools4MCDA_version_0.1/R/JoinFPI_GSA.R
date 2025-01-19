#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with GSA polygons to estimate the Geographical Sub-Area.
#'
#' Combine Fishing Pressure Index (FPI- estimated by MCDA) with GSA polygons to estimate the Geographical Sub-Area.
#'
#' @importFrom stats stepfun
#'
#' @param GSA_poly  The sf polygon of GSAs.
#' @param field_GSA The field that contains numerical information for the GSA using "".
#' @param data_FPI  A data frame with FPI estimations. The above data frame should contain the fields: LON(longitude in WGS84), LAT(latitude in WGS84), gear (values- SSF, GNS, GTR, LLS), year, vessel_length_cat , quarter( values 1-4, or 0 for annual estimations)
#' @param LON Name of Longitude field in FDI table. The longitude should be in a geographical coordinate system WGS84
#' @param LAT Name of Latitude field in FDI table. The longitude should be in a geographical coordinate system WGS84
#'  
#' @return Return of a data.frame of FPI with information by GSA.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'library(sf)
#'
#'data("dfcut2")
#'data("GSA_poly")
#'
#'dfcut2<-joinFPI_GSA(data_FPI=dfcut2,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="x",LAT="y")
#' @export
joinFPI_GSA <- function(data_FPI, GSA_poly, field_GSA="SMU_CODE", LON, LAT) {
    
    library(sf)
    
    FPI<-data_FPI
    FPI$LON<-FPI[[LON]]
    FPI$LAT<-FPI[[LAT]]
    FPI<- as.data.frame(FPI) %>% 
      st_as_sf(coords=c(LON,LAT), crs=4326, remove=FALSE)  
    
    # For POINTS that fall within CA_counties, adds ATTRIBUTES, retains ALL pts if left=TRUE, otherwise uses inner_join
    FPI <- st_join(FPI, left = FALSE, GSA_poly[field_GSA]) # join points GSA
    FPI$Sub.region<-paste0("GSA",FPI$SMU_CODE)
    FPI<-as.data.frame(FPI)
    FPI<-FPI%>% 
      dplyr::select(-geometry)
    
    
    return(FPI)
  }
  