#' Add a field related to Landings Per Unit of Effort (LPUE) in the dataframe derived based on MCDA
#'
#' Using the predicted species distribution to estimate a proxy of LPUE. The fields for LPUE by species are added to the dataframe derived from MCDA. The above is then used for estimating landings' weight and value.
#'
#'
#' @param data_FPI The dataframe derived by MCDA
#' @param spec An object including the species that will be included in the MCDA dataframe in 3 alpha code format. For example c("HKE","MUT")
#' @param folder The folder path that contains the species distribution outcomes (in a text format e.g. txt, csv)
#' @param name An object with the name of the files. For example c("hke.txt","mut.txt").
#' @param field The field which is containing the predicted values of species distribution that will be used as a proxy for LPUE. For example c("grid_code","grid_code")
#' @param LON The field containing longitude values.The field should be in "".
#' @param LAT The field containing latitude values.The field should be in "".
#' @param sep The separator
#'  
#' @return Return a dataframe including a field to be used as a proxy for LPUE and species.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'data(sample_FPIq)
#'data_with_LPUE<-add_LPUE(data_FPI=sample_FPIq,spec=c("HKE","MUT"),folder="_Your_folder_",name=c("hke.txt","mut.txt"), field=c("grid_code","grid_code"), LON="POINT_X", LAT="POINT_Y", sep="," )
#' @export
add_LPUE<-function (data_FPI=sample_FPIq,spec=c("HKE","MUT"),
                    folder="",
                    name=c("hke.txt","mut.txt"), field=c("grid_code","grid_code"), LON="POINT_X", LAT="POINT_Y", sep="," )
{
  s_r1<-NULL
  sample_spq<-NULL
  sp_sample<-NULL
  sp_sample_v2<-NULL
#i<-2
for (i in 1:length(spec)){
  s_data<-read.csv(file = paste0(folder,name[i]), header = TRUE, sep = sep)
  s_data$LON<-s_data[[LON]]
  s_data$LAT<-s_data[[LAT]]
  s_data$field<-s_data[[field[i]]]
s_r1 <- rasterFromXYZ(s_data[, c("LON", "LAT", "field")])
crs(s_r1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  
fpi_agreegate<-aggregate(FPI~LON+LAT,FUN=mean, data=data_FPI)
sp_sample<-sample_raster(raster_FPI=s_r1, df_grid=fpi_agreegate, LON="LON", LAT="LAT") 
sp_sample$species<-spec[i]
sp_sample$LPUE<-sp_sample$layer/max(sp_sample$layer)
sp_sample<-unite(sp_sample, col='key_sp', c('LON','LAT'), sep='_', remove=FALSE)
data_FPI<-unite(data_FPI, col='key_sp', c('LON','LAT'), sep='_', remove=FALSE)
sp_sample_v2<-merge(x = sp_sample[,c("key_sp","species","LPUE")], y =data_FPI , by = "key_sp", all.y = TRUE)
sample_spq<-rbind(sample_spq,sp_sample_v2)
}

  return(sample_spq)
}
