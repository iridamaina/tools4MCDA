#' Convert monthly Chlorophyll-a netCDF dataset to a dataframe
#'
#' Using the monthly Aqua-MODIS Chlorophyll-a concentration netCDF datasets and convert to a dataframe. The dataframe can be produced for a certain area. The Clorophyll-a netCDF datasets are based on the level-3 products produced and archived by the NASA Ocean Biology Processing Group (NASA Ocean Biology Processing Group, 2024, Level-3 Ocean Color Data, NASA Ocean Biology Distributed Active Archive Center. https://oceancolor.gsfc.nasa.gov/l3) . 
#'
#'
#' @param file_path The file path for the netCDF monthly file for Aqua-MODIS Chlorophyll-a concentration. The netCDF file is in mapped-SMI format.  
#' @param lonmax Maximum longitude value of the selected area (in decimal degrees).
#' @param lonmin Minimum longitude value of the selected area (in decimal degrees).
#' @param latmax Maximum latitude value of the selected area (in decimal degrees).
#' @param latmin Minimum latitude value of the selected area (in decimal degrees).
#'  
#' @return Return a dataframe including longitude, latitude, chlorophyll-a value, year, month and quarter.
#' @author I. Maina
#' @examples
#'library(tools4MCDA)
#'
#'#Convert netCDF to dataframe for a certain geographical area
#'#Example for Greece
#'chl_df<-chl_nc_to_df(file_path="~.nc", lonmax=26, lonmin=18, latmax=43, latmin=33)
#' @export
chl_nc_to_df <- function(file_path, lonmax=26, lonmin=18, latmax=43, latmin=33) {
  
  nc <- nc_open(file_path)
  
  var <- nc$var[[1]]
  
  # Read the variables
  lon <- ncvar_get(nc, "lon")  
  lat <- ncvar_get(nc, "lat")  
  chlorophyll <- ncvar_get(nc, var[["name"]])  
  unit<-ncatt_get(nc,var,"units")$value
  
  # extract date information
  dateini<-ncatt_get(nc,0,"time_coverage_start")$value
  dateend<-ncatt_get(nc,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  
  # Close the NetCDF file
  nc_close(nc)
  
  # Check dimensions and reshape if necessary
  if (length(dim(chlorophyll)) == 3) {
    # Assuming chlorophyll data has dimensions (lon, lat, time)
    chlorophyll <- chlorophyll[,,1]  # Selecting the first time step (if applicable)
  }
  
  # Create a regular grid
  chl_matrix <- matrix(chlorophyll, nrow = length(lat), ncol = length(lon),byrow = TRUE)
  
  # Create a raster from the matrix
  chl_raster <- raster(chl_matrix, xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))
  
  #convert to a dataframe
  chl_df <- as.data.frame(rasterToPoints(chl_raster), stringsAsFactors = FALSE)
  
  #add date information to the dataframe
  chl_df$year<-as.numeric(year)
  chl_df$month<-as.numeric(month)
  chl_df$quarter<-ifelse(chl_df$month>=1 & chl_df$month<=3, 1,
                         ifelse(chl_df$month>=4 & chl_df$month<=6, 2,
                                ifelse(chl_df$month>=7 & chl_df$month<=9, 3,
                                       ifelse(chl_df$month>=10 & chl_df$month<=12, 3, NA ))))
  
  # select data from the study area taking out missing data
  chl_df<-subset(chl_df,x<=lonmax & x>=lonmin & y<=latmax & y>=latmin)
  names(chl_df)[names(chl_df) == 'layer'] <- 'chl'
  
  return(chl_df)
}

