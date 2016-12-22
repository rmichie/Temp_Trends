Calculate.sdadm <- function(df, result_column_name, units_column_name, station_column_name, datetime_column_name, datetime_format) {
  # Description:
  # Calculates seven day average daily maximum
  #
  # This function takes 4 arguments:
  #  df                   = A data frame containing at minimum the columns representing a Station Identifier, Numeric results and a datetime
  #  result_column_name   = A character string specifying the name of the result column in df
  #  units_column_name    = A character string specifying the units (F or C) of the result in df
  #  station_column_name  = A character string specifying the name of the station column in df
  #  datetime_column_name = A character string specifying the name of the datetime column in df. datetime column should be in format "%m/%d/%Y %H:%M:%S"
  #  datetime_format      = A character string specifying the format of the datetime column. See the format argument of strptime for details
  #
  # Details:
  # Requires installation of libraries chron and reshape
  # 
  # Result column is coerced to class numeric.
  # 
  # Result values above 36 are treated as if they are Farenheit and are modified using the conversion equation from 
  # Farenheit to Celsius.
  # 
  # NA values are removed when taking daily maximums unless a day has no observed data in which case NA will be returned.
  # 
  # Value: 
  # An object of class data frame with columns:
  # date: class Date in format %Y-%m-%d
  # station_column_name: in same format and name as provided
  # SDADM: class numeric representing the calculated seven day average
  
  require(lubridate)
  require(reshape)
  require(zoo)
  
  ## TEST
  #tdata <- df.all[,c("Station_ID","date","Result","Unit")]
  #datetime_format <-"%Y-%m-%d %H:%M:%OS"
  
  tdata <- df[,c(station_column_name, datetime_column_name, result_column_name, units_column_name)]
  
  ## RENAME
  colnames(tdata)[1] <- "id"
  colnames(tdata)[2] <- "datetime"
  colnames(tdata)[3] <- "t"
  colnames(tdata)[4] <- "unit"
  
  ## convert F -> C
  tdata$t <- as.numeric(tdata$t)
  tdata$t_c <- ifelse(grepl('F', tdata$unit), round(((tdata$t-32)*5/9),1),tdata$t)
  tdata$unit <- NULL

  
  ## Create a vector of daily dates for grouping
  if (is.POSIXct(tdata$datetime)) {
    tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
  } else {
    tdata$datetime <- as.POSIXct(strptime(tdata$datetime, format = datetime_format))
    tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
  }
  
  
  #############################
  # tdata COLUMN NAMES
  # tdata[1] <- "id"
  # tdata[2] <- "datetime"
  # tdata[3] <- "t"
  # tdata[4] <- "t_c"
  # tdata[5] <- "date"
  #############################
  
  ####################################################################################################
  # This section inserts a dummy station "-99" into tdata with a sequence of -99s and NAs for the associated variables.
  # The -99 timeseries starts from the oldest date in the entire dataset and ends at the most recent date.
  # The purpose for the dummy data is to create a continous daily timeseries so 7DADM are not calculated
  # between breaks in days for the same station.
  
  datetime99<-as.character(seq(min(tdata$date),max(tdata$date),by=1))
  date99<- as.Date(seq(min(tdata$date),max(tdata$date),by=1))
  id99<-rep(unique(tdata$id),by=0,length.out=length(datetime99))
  t99<- rep(NA,by=0,length.out=length(datetime99))
  t_c99<-rep(NA,by=0,length.out=length(datetime99))
  
  dummy <-data.frame(cbind(id99, t99, t_c99))
  dummy<- cbind(dummy, datetime99, date99)
  
  colnames(dummy)[1] <- "id"
  colnames(dummy)[2] <- "t"
  colnames(dummy)[3] <- "t_c"
  colnames(dummy)[4] <- "datetime"
  colnames(dummy)[5] <- "date"
  
  dummy$t_c <- as.numeric(dummy$t_c)
  dummy$t <- as.numeric(dummy$t)
  
  tdata <-rbind(tdata,dummy)
  rm(dummy,date99,datetime99,id99,t_c99,t99)
  #############################################################################################
  
  ## Calculate daily maximums by station
  tmax<- tapply(tdata$t_c,list(tdata$date,tdata$id),function(x) {ifelse(all(is.na(x)),NA,max(x, na.rm = TRUE))})
  
  ## Calculate 7DADM
  if (length(tmax) < 7) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    sdadm<- rollapply(tmax,7,mean,na.rm=TRUE,fill=NA, align="right")
    sdadm<- round(sdadm,1)
  }
  
  ## Return data to long format and rename station column header
  if (all(is.na(sdadm))) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    datevector <-as.Date(rownames(tmax))
    sdadm <-data.frame(sdadm)
    sdadm <- cbind(datevector,sdadm)
    colnames(sdadm)[1] <- "date"
    sdadm.melt <- melt.data.frame(sdadm, id.var="date",variable_name = "id")
    colnames(sdadm.melt)[3] <- "sdadm"
    sdadm.melt$id <- gsub("X","",sdadm.melt$id,fixed=TRUE)
    sdadm.melt$id <- gsub(".","-",sdadm.melt$id,fixed=TRUE)
    #colnames(sdadm.melt)[2] <- station_column_name
    return(sdadm.melt)
  }
}