# This script screens the data queried from step 1

library(lubridate)
library(dplyr)
library(reshape2)

fun_dir <- 'C:/WorkSpace/GitHub/Temp_Trends'
data_query_dir <- 'F:/WorkSpace/MidCoast/Temp_Data/data_query/Siuslaw'

setwd(fun_dir)

source('funCalculateSDADM_narm.R')
source('funMultiplot.R')

options(stringsAsFactors = FALSE)

# QC Screens for data sufficiency  ---------------------------------------

setwd(data_query_dir)
fnames <- list.files(pattern = "^[0-9]{8}")
df_list <- list(6,7,8,9,10)
names(df_list) <- 6:10
df_list <- lapply(df_list, function(x) X <- NULL)

qc.results.1 <- NULL
qc.results.2 <- NULL
qc.results.3 <- NULL
qc.temp <- NULL

# loop through each Rdata file
for (i in 1:length(fnames)) {
  load(fnames[i])
  tmp <- df.all
  
  tmp$date <- as.POSIXct(strptime(tmp$Sampled, format = "%Y-%m-%d %H:%M:%OS"))
  tmp$month <- month(tmp$date)
  tmp$year <- year(tmp$date)
  tmp$day <- day(tmp$date)
  tmp$hour <- hour(tmp$date)
  
  # subset to data to the months of interest
  tmp <- tmp[tmp$month %in% c(6,7,8,9,10),]
  
  # QC Test #1 -------------------------------------------------------------
  # Must be at least one observation in a minimum of 22 hours during the day
  
  # First determine number of hours collected within each day
  qc.hr <- as.tbl(tmp) %>%
    group_by(HUC, Station_ID, Station_Description, month, year, day) %>%
    summarise(n = length(unique(hour)))
  qc.hr <- as.data.frame(qc.hr)
  
  # Isolate to days with 22 or more hours represented
  qc.hr$n_threshold <- '>= 22 hours'
  qc.hr$result <- ifelse(qc.hr$n >= 22,'pass','fail')
  
  qc.results.1 <- rbind(qc.results.1,qc.hr)
  
  qc.hr.p <- qc.hr[qc.hr$result == 'pass',]
  qc.hr.p$code <- paste(qc.hr.p$Station_ID, qc.hr.p$year, qc.hr.p$month, qc.hr.p$day)
  tmp$code <- paste(tmp$Station_ID, tmp$year, tmp$month, tmp$day)
  
  # subset to just days that pass QC test #1
  tmp <- tmp[tmp$code %in% qc.hr.p$code,]
  
  # QC Test #2 -------------------------------------------------------------
  # No more than one day for each monthly period without observations

  qc.dy <- as.data.frame(as.tbl(qc.hr.p) %>% 
                           group_by(HUC, Station_ID, Station_Description, year, month) %>% 
                           summarise(n = n()))
  #qc.dy$n_threshold <- ifelse(qc.dy$month %in% c(7,8,10), 30, 29)
  
  # No more than five days for each monthly period without observations - RM
  qc.dy$n_threshold <- ifelse(qc.dy$month %in% c(7,8,10), 24, 23)
  qc.dy$result <- ifelse(qc.dy$n >= qc.dy$n_threshold,'pass','fail')
  
  # just redoing this so threshold is clear
  qc.dy$n_threshold <- ifelse(qc.dy$month %in% c(7,8,10), '>= 30 days', '>= 29 days')
  
  qc.results.2 <- rbind(qc.results.2,qc.dy)
  
  qc.dy.p <- qc.dy[qc.dy$result == 'pass',]
  qc.dy.p$code <- paste(qc.dy.p$Station_ID, qc.dy.p$year, qc.dy.p$month)
  tmp$code <- paste(tmp$Station_ID, tmp$year, tmp$hour)
  
  # subset to just months that pass QC test #2
  tmp <- tmp[tmp$code %in% qc.dy.p$code,]
  
  # QC Test #3 -------------------------------------------------------------
  # There must be at least eight years of continuous hourly temperature data
  # for each monthly period
  
  qc.yr <- as.data.frame(as.tbl(qc.dy.p) %>% 
                           group_by(HUC, Station_ID, Station_Description, month) %>% 
                           summarise(n = n()))
  qc.yr$n_threshold  <- '>= 8 years'
  qc.yr$result <- ifelse(qc.yr$n >= 8,'pass','fail')
  qc.results.3 <- rbind(qc.results.3,qc.yr)
  
  qc.yr.p <- qc.yr[qc.yr$result == 'pass',]
  
  qc.yr.p$code <- paste(qc.yr.p$Station_ID, qc.yr.p$month)
  tmp$code <- paste(tmp$Station_ID, tmp$month)
  
  # subset to just stations that pass QC test #3
  tmp <- tmp[tmp$code %in% qc.yr.p$code,]
  
  for (j in 6:10) {
    tmp.month <- tmp[tmp$month == j,]
    df_list[as.character(j)][[1]] <- rbind(df_list[as.character(j)][[1]], tmp.month)
  }
  
  rm(list = grep('tmp',ls(), value = TRUE))
}  

# save the screened data to disk
lapply(df_list, function (x) {
  write.csv(x, file = paste0(unique(x$month),"_data_qcpass.csv"))
})

# save station summmary of station that pass screen to disk
data.pass <- bind_rows(df_list)
stns.summary <-unique(data.pass[c("Station_ID", "Station_Description", "HUC",
                   "DECIMAL_LAT","DECIMAL_LONG","DATUM","Database")])

stns.summary[c('ZDADM','ben_use','spawn_dates')] <- NA
write.csv(stns.summary,'qc_pass_station_summary.csv',row.names = FALSE)

save(df_list, file = 'data_all_lists.Rdata')

write.csv(qc.results.1,'qc_results_1.csv',row.names = FALSE)
write.csv(qc.results.2,'qc_results_2.csv',row.names = FALSE)
write.csv(qc.results.3,'qc_results_3.csv',row.names = FALSE)

qc.results.1 <- read.csv('qc_results_1.csv')
qc.results.2 <- read.csv('qc_results_2.csv')
qc.results.3 <- read.csv('qc_results_3.csv')

# Number of stations evaluated
length(unique(qc.results.1$Station_ID))

# Numer of stations that passed QC screen for trending
length(unique(qc.results.3[qc.results.3$result == 'pass',]$Station_ID))

