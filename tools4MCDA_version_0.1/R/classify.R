#' Classify a dataframe 
#'
#' Classify a dataframe based on scores and cuts
#'
#'
#' @param data_field dataframe to be classified 
#' @param cuts a numeric object including the values for the classification
#' @param scores a numeric object including the scores 
#'  
#' @return Return a dataframe with classified values
#' @author I. Maina 
#' @examples
#' library(tools4MCDA)
#'library(sf)
#'library(sfheaders)
#'library(ggplot2)
#'library(tidyr)
#'library(BAMMtools)
#'
#'data("fleet_reg_year")
#'
#'#Define year and country
#'ye<-2019
#'country="GRC"
#'
#'# unite gear_type and vessel_length
#'fleet_reg_year<-unite(fleet_reg_year, col='key', c('MAIN_FISHING_GEAR', 'vessel_length'), sep='_', remove=FALSE)
#'
#'#base map for bubble plots
#'poly<- map_data("world")
#'map_sf <- sfheaders::sf_polygon(obj = poly, x = "long", y = "lat", polygon_id = "group")
#'sf::st_crs(map_sf) <- 4326
#'
#'# classify values for plotting
#'fleet_reg_year<-fleet_reg_year[fleet_reg_year[1]==country,]
#'i<-"GTR_VL0612"
#'fleet<-fleet_reg_year[fleet_reg_year$key==i,]
#'classes<-as.data.frame(getJenksBreaks(as.numeric(fleet$SUM_LOA_GT),7))
#'colnames(classes)<-"classes"
#'classes$key<-i
#'cla3<-as.numeric(classes[,1])
#'cla3<-cla3[-7]
#'fl_yy<-fleet_reg_year[fleet_reg_year$key==i,]
#'fl_yy$kmcl<-classify(data_field=as.numeric(fl_yy[,"SUM_LOA_GT"]),cuts=cla3, scores=cla3)
#' @export
classify<-function (data_field, cuts=c(10,50,max(data$kg_km2)), scores=c(10,50,max(data$kg_km2))){
  s <- stepfun(cuts[-1], scores)
  u = s(data_field)
  return(u)
}
