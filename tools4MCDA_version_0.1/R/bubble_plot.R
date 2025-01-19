#' Create a bubble map by year
#'
#' Create a bubble map by year to plot the Activity index by port.
#'
#'
#' @param data a dataframe with the Activity index by port 
#' @param xlim limits on x axis
#' @param ylim limits on y axis
#' @param years column including years in the data
#' @param ncol number of columns of the plot
#' @param classes number of the classes or function
#' @param LegendTitle The title of the legend
#' @param PlotTitle The title of the plot
#' @param limits 
#' @param max_size maximum size of the bubble
#'  
#' @return Returns a plot 
#' @author I. Maina 
#' @examples
#'library(tools4MCDA)
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
#'#Bubble maps by year
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
#'fl_yy$kmcl<-as.numeric(tools4MCDA::classify(data_field=as.numeric(fl_yy[,"SUM_LOA_GT"]),cuts=cla3, scores=cla3))
#'fl_yy$x<-as.numeric(fl_yy$LONGITUDE)
#'fl_yy$y<-as.numeric(fl_yy$LATITUDE)
#'fl_yy$SUM_LOA_GT<-as.numeric(fl_yy$SUM_LOA_GT)
#'fl_yy<-fl_yy[fl_yy$YEAR==ye,]
#'
#'# bubble plot
#'bubble_plot_year(data=fl_yy,field='kmcl',map_sf=map_sf,
#'                 xlim=c(as.numeric(min(fl_yy$x)),as.numeric(max(fl_yy$x))),ylim=c(as.numeric(min(fl_yy$y)),as.numeric(max(fl_yy$y))), years=ye,
#'                 classes=cla3, ncol=1,
#'                 limits=(c(0,as.numeric(max(fl_yy[,"SUM_LOA_GT"])))),PlotTitle=paste0(i,fl_yy$YEAR),
#'                 LegendTitle=paste0(i,fl_yy$YEAR), max_size = 4)
#' @export
bubble_plot_year<-function(data,field,map_sf,xlim=c(19,24),ylim=c(36,40),years, ncol=3 , classes=waiver(),LegendTitle,PlotTitle, limits=NULL,max_size = 6){
  #### bubble plot by year
  library(ggplot2)
  library(dplyr)
  library(viridis)
 
  data$field <-as.numeric(data[[field]])

  
  #class(map_sf)
  test<-
    ggplot() +
    geom_sf(data=map_sf, fill="lightgrey")+
    coord_sf(xlim=xlim, ylim=ylim) +
    geom_point( data=(data%>%
                        filter(YEAR==min(years))%>%
                        filter(field!=0)), aes(x=x, y=y, size=field, color=field),show.legend=T) +
    scale_size_binned_area(max_size = max_size , breaks = classes, label= scales::comma, limits=limits)+
    scale_color_binned( type="viridis",breaks = classes, label= scales::comma, limits=limits)+
    guides(color=guide_legend(title=paste(LegendTitle)),size=guide_legend(title=paste(LegendTitle)))+
    theme_bw() + 
    facet_wrap(~YEAR,ncol=ncol)+
    ggtitle(paste(PlotTitle))+
    geom_point( data=(data%>%
                        filter(YEAR>=min(years))%>%
                        filter(field==0)), aes(x=x, y=y), size=0.5, shape=4, colour="grey30")
  
  return(test)
}

