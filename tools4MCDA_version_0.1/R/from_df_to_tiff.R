#' Convert a dataframe to Raster (TIFF)
#'
#' Convert a dataframe including geographical coordinates (longitude, latitude) and a field related to fishing effort, weight or value of landings to Raster (in TIFF format). This function was created to support analysis by quarter.
#'
#'
#' @param data The dataframe to be converted in Raster
#' @param quarter A numeric object including the quarters that data are available. For example c(1,2,3,4) .
#' @param parameter The field that will be included in the values of the raster. The field should be in "". For example "totfishdays".
#' @param LON The field of the dataframe including longitude values.The field should be in "".
#' @param LAT The field of the dataframe including latitude values.The field should be in "".
#' @param folder The folder path to save the derived tiff files.
#' @param r A raster object
#'  
#' @return Return rasters in tiff format.
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
#'#create table I in the data call format
#'tableI_seffort_SSF<-create_tableI(data=ERS_q,country="Country",	year="year",	quarter="quarter",	vessel_length="vessel_length",
#'                                  fishing_tech="fishing_tech", gear_type="gear_type",	target_assemblage="target_assemblage",
#'                                  mesh_size_range="NK",	metier="metier",	metier_7="NK",	supra_region="MBS",
#'                                  sub_region="Sub.region",	eez_indicator="NA",	geo_indicator="NK",	specon_tech="NK",
#'                                  deep="NA",	rectangle_type="05*05",	latitude="csq_y",	longitude="csq_x",	c_square="NA",	
#'                                  totfishdays="parameter",confidential="N")
#'
#'r1<-rasterFromXYZ(tableI_seffort_SSF[, c('longitude', 'latitude','totfishdays')])
#'
#'tableI_seffort_SSF_4p<-unite(tableI_seffort_SSF, col='key', c('year','vessel_length','fishing_tech', 'gear_type', 'target_assemblage', 'metier'), sep='_', remove=FALSE)
#'p<-"2019_VL0006_DFN_GNS_DEF_GNS_DEF_>0_0_0"
#'tableI_seffort_SSF_4p<-tableI_seffort_SSF_4p[tableI_seffort_SSF_4p$key=="2019_VL0006_DFN_GNS_DEF_GNS_DEF_>0_0_0",]
#'
#'# TABLE I: SPATIAL EFFORT
#'result_q_4p<-tableI_seffort_SSF_4p[tableI_seffort_SSF_4p$key==p,]
#'qu<-as.numeric(unique(result_q_4p$quarter))
#'from_df_to_tiff(data=result_q_4p,quarter=qu,parameter="totfishdays",LON="longitude", LAT="latitude",
#'                folder="<Your Path>",r=r1)
#' @export
from_df_to_tiff<-function(data=tableH_landings_SSF_4p,quarter=qu,parameter="",LON="longitude", LAT="latitude",
                          folder="C:/outcomes",
                           r=r){
  

  data$parameter<-data[[parameter]]
  data$LON<-data[[LON]]
  data$LAT<-data[[LAT]]
  
  rx1<-NULL
  #u<-1
  for (u in 1:length(quarter)){
    data_df1<-data[data$quarter==quarter[[u]],]
    rx1[[u]] <- rasterize(data_df1[, c("LON","LAT")], field = data_df1$parameter, r,update = TRUE)
    rx1[[u]]@file@name<-as.character(quarter)
    data_y<-rx1[[u]]
    crs(data_y) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    raster::writeRaster(data_y, filename=paste0(folder,"/",parameter,gsub(c(">","="),"",as.character(unique(data_df1$key))),"_",quarter[u],".tiff"),overwrite=TRUE)
   }
}

