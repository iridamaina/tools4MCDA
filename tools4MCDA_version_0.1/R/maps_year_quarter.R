#' Mapping by quarter, year and fishing gear type
#'
#' Create plots presenting the spatial distribution of fishing effort, landings weight or value by year, quarter, fishing gear type, metier etc. 
#'
#'
#' @param data The dataframe to be plotted
#' @param quarter A numeric object including the quarters that data are available. For example c(1,2,3,4) .
#' @param parameter The field in the dataframe that will be plotted. The field should be in "". For example "totfishdays".
#' @param xlim A numeric object with the minimum and maximum longitude values to be plotted. For example c(19,24)
#' @param ylim A numeric object with the minimum and maximum latitude values to be plotted. For example c(36,40)
#' @param LON The field of the dataframe including longitude values.The field should be in "".
#' @param LAT The field of the dataframe including latitude values.The field should be in "".
#' @param ylab The label in the legend of the plot in "".
#' @param PlotTitle The title of the plot in "".
#' @param poly A coastline polygon 
#' @param poly A raster object 
#'  
#' @return Return maps by year, quarter etc.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'library(ggplot2)
#'library(raster)
#'library(rasterVis)
#'
#'data("sample_FPIq")
#'data("dfcut2")
#'poly<- map_data("world")
#'
#'#DEFINE COUNTRY, GSA, year_start, year_end
#'country<-"GRC"
#'GSA<-20
#'
#'#crop the area and create a raster
#'dfcut2$lon<-as.numeric(dfcut2$x)
#'dfcut2$lat<-as.numeric(dfcut2$y)
#'dfcut2<-joinFPI_GSA(data_FPI=dfcut2,GSA_poly = GSA_poly,field_GSA="SMU_CODE",LON="x",LAT="y")
#'dfcut2<-dfcut2[dfcut2$SMU_CODE %in% GSA,]
#'r<-rasterFromXYZ(dfcut2[, c('lon', 'lat')])
#'crs(r) <- CRS(paste('+init=EPSG:4326'))
#'
#'#preparing outcomes from MCDA
#'FPI_all<-unite(sample_FPIq, col='key', c('year','vessel_length','gear_type'), sep='_', remove=FALSE)
#'
#'#Select gear type and vessel length category
#'p<-"2019_VL0006_GNS"
#'result_q_4p<-FPI_all[FPI_all$key==p,]
#'
#'#estimate quarter  
#'qu<-order(as.numeric(unique(result_q_4p$quarter)), decreasing = TRUE)
#'
#'#Create maps
#'maps_year_quarter(data_df=result_q_4p,quarter=qu, parameter="FPI",xlim=c(min(result_q_4p$LON),max(result_q_4p$LON)),
#'                  ylim=c(min(result_q_4p$LAT),max(result_q_4p$LAT)), LON="LON", LAT="LAT",ylab="FPI",
#'                  PlotTitle=paste(unique(result_q_4p$key)), 
#'                  poly=poly, r=r)
#' @export

maps_year_quarter<-function(data_df,quarter=qu, parameter="",xlim=c(19,24),ylim=c(36,40), LON="longitude", LAT="latitude",
                            ylab="kg/km2",PlotTitle="HKE",poly=poly, r=r){

  data_df$parameter<-data_df[[parameter]]
  data_df$LON<-data_df[[LON]]
  data_df$LAT<-data_df[[LAT]]
  rx1<-NULL
   for (u in 1:length(quarter)){
    data_df1<-data_df[data_df$quarter==quarter[[u]],]
      rx1[[u]] <- rasterize(data_df1[, c("LON","LAT")], field = data_df1$parameter, r,update = TRUE)
  rx1[[u]]@file@name<-as.character(quarter)
  }
  
  cv <- stack(rx1)
names(cv)=quarter

new.labs<-as_labeller(function(string) paste(as.numeric(gsub('X','',names(cv)))), label_parsed)
idw_plot<-gplot(cv) +  
  geom_tile(aes(fill = value)) +
  scale_fill_viridis(option ="H", direction = 1,na.value="white", name=paste(ylab)) + 
  facet_wrap(~variable, ncol = 4,labeller= new.labs)+ 
  geom_polygon(data = poly, aes(x=long, y = lat, group = group), fill="grey") +
  coord_equal(xlim = xlim, ylim = ylim)+
  ggtitle(paste(PlotTitle))+
  theme_bw() 
return(idw_plot)
}
