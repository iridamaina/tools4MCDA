#' Increase the extent of a dataframe
#'
#' Add longitude and latitude to increase the extent of a dataframe based on a tolerance value. The dataframe should include the following fields: longitude, latitude, quarter (values: 1-4), year.
#'
#'
#' @param df a dataframe with the following fields: longitude, latitude, quarter (values: 1-4), year.
#' @param tolerance a number to increase the extent
#' @param LON the name of longitude field using ""
#' @param LAT the name of latitude field using ""
#' @param year the name of year field using ""
#' @param quarter the name of quarter field using ""
#'  
#' @return Returns a dataframe
#' @author I. Maina 
#' @examples
#'library(tools4MCDA)
#'data(chl_y_q)
#'chl_increase_extent<-incr_extent(df = chl_y_q, tolerance=2, LON="x", LAT="y", year="year", quarter="quarter")
#' @export
incr_extent<-function (df = chl_y_q, tolerance=2, LON="x", LAT="y", year="year", quarter="quarter") {
df$year<-df[[year]]
df$quarter<-df[[quarter]]
df$LON<-df[[LON]]
df$LAT<-df[[LAT]]

df<-unite(df, col='year_quarter', c('year','quarter'), sep='_', remove=FALSE)

  
dataB <- df[, colnames(df)]
dataB <- dataB[1:(2*length(unique(df$year_quarter))),]

for (d in 1:length(unique(df$year_quarter))){
  dataB$LON[d]<-(min(df$LON)-tolerance)
  dataB$LAT[d]<-(max(df$LAT)+tolerance)
  dataB$quarter[d]<-d
}

for (d in (length(unique(df$year_quarter))+1):(2*length(unique(df$year_quarter)))){
  dataB$LON[d]<-(max(df$LON)+tolerance)
  dataB$LAT[d]<-(min(df$LAT)-tolerance)
  dataB$quarter[d]<-d-4
}

df<-rbind(dataB,df)
df<-unite(df, col='year_quarter', c('year','quarter'), sep='_', remove=FALSE)

return(df)
}
