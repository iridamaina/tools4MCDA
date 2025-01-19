#' Convert netcdf containing meteorological data to csv.
#'
#' Convert netcdf containing meteorological data to csv. Outcomes can be used in the function meteoCr.
#'
#'
#' @param nc_data The path that netcdf data are stored.
#' @return Return of a data.frame.
#' @author I. Maina
#' @examples
#'
#'
#'   # Example 
#'   library(ncdf4) # package for netcdf manipulation
#'   library(raster) # package for raster manipulation
#'   library(ggplot2) 
#'
#'   meteo2018_q<-MeteoNetcdfToCsv(nc_data = ".data.nc")
#' @export


MeteoNetcdfToCsv <- function(nc_data) {
  
  nc_data <- nc_open(nc_data)
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  dname= "10u"
  variabel = ncvar_get(nc_data,dname)
  lonlat = as.matrix(expand.grid(lon,lat))
  var_10u = as.vector(variabel)
  var10u<-as.data.frame(var_10u)
  datanya = data.frame(cbind(lonlat,t,var_10u))
  lonlatv2<-data.frame(cbind(datanya$Var1, datanya$Var2))
  
  
  #extract all variables
  dname= "10v"
  variabel = ncvar_get(nc_data,dname)
  var_10v = as.vector(variabel)
  var10v<-as.data.frame(var_10v)
  
  dname= "2d"
  variabel = ncvar_get(nc_data,dname)
  var_2d = as.vector(variabel)
  var2d<-as.data.frame(var_2d)
  
  dname= "2t"
  variabel = ncvar_get(nc_data,dname)
  var_2t = as.vector(variabel)
  var2t<-as.data.frame(var_2t)
  
  dname= "sst"
  variabel = ncvar_get(nc_data,dname)
  var_sst = as.vector(variabel)
  varsst<-as.data.frame(var_sst)
  
  dname= "sp"
  variabel = ncvar_get(nc_data,dname)
  var_sp = as.vector(variabel)
  varsp<-as.data.frame(var_sp)
  
  dname= "tp"
  variabel = ncvar_get(nc_data,dname)
  var_tp = as.vector(variabel)
  vartp<-as.data.frame(var_tp)
  
  
  ####################____copy lon lat n(8760) times (i.e. 1 year by 1-hour step)
  n <- max(t)+1 #
  lon_latv3<-do.call("rbind", replicate(n, lonlatv2, simplify = FALSE))
  ##################_________create time
  time3 <- rep(1:n, each = nrow(datanya)) 
  time4<- as.data.frame(time3)
  
  ###########_________cbind lon,lat,var,time
  metcsv<-cbind(lon_latv3,time4,var10u,var10v,var2t,var2d,varsst,varsp,vartp)
  
  ####################___________translate time
  timeDim        <- ncvar_get(nc_data, "time")
  # Put the time in a reader friendly format
  label.time <- names(nc_data$dim)[5]
  date.char <- utcal.nc(nc_data$dim[[label.time]]$units, nc_data$dim[[label.time]]$vals, type="s")
  date.POSIXlt <- strptime(date.char, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  timeDim        <- as.POSIXct(date.POSIXlt)
  library(lubridate)
  timer<-as.data.frame(timeDim)
  timer$mm<-month(timeDim)
  timer$yy<-year(timeDim)
  timer$hh<-hour(timeDim)
  timer$id<-rep(1:n) 
  
  
  ####______merge_all data with translated time
  metcsv_f<-merge(x=metcsv, y=timer,by.x=c("time3"),by.y=c("id"), all=F)
  
  
  ###############################################################
  ######################################################
  ###################_____estimate indicators#####
  
  
  ######################________________###########################
  
  #######estimate_windspeed_from vectors
  
  metcsv_f$ws<-sqrt(metcsv_f$var_10u^2+metcsv_f$var_10v^2)
  
  ####estimate days for taking the mean of SST by day to fill NAs 
  library(lubridate)
  metcsv_f$dd<-day(metcsv_f$timeDim)
  metcsv_f$yy_mm_dd<-paste(metcsv_f$yy,metcsv_f$mm,metcsv_f$dd)
  
  ####estimate new SST in celsious
  metcsv_f$var_2tC<- (metcsv_f$var_2t - 273.15)
  attach(metcsv_f)
  
  #####____keep only the necessary field to start 
  metcsv_f_nec <- data.frame("time3" = metcsv_f$time3, 
                             "longitude" = metcsv_f$X1,
                             "latitude" = metcsv_f$X2,
                             "mm" = metcsv_f$mm,
                             "yy"= metcsv_f$yy,
                             "dd"= metcsv_f$dd,
                             "hh"= metcsv_f$hh,
                             "ws"= metcsv_f$ws,
                             #"SSTC"= met2017_2018$var_sstC,
                             "tp"= metcsv_f$var_tp,
                             "t2"= metcsv_f$var_2tC)
  
  
  metcsv_f_nec$quarter<- ifelse(metcsv_f$mm<=3, 1,
                                ifelse((metcsv_f$mm>=4) & (metcsv_f$mm<=6), 2,
                                       ifelse((metcsv_f$mm>=7) & (metcsv_f$mm<=9), 3,
                                              ifelse((metcsv_f$mm>=10) & (metcsv_f$mm<=12), 4,
                                                     0))))
  
  return(metcsv_f_nec)
}
