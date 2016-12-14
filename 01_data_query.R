library(dataRetrieval)
library(data.table)
library(plyr)
library(reshape)
library(RODBC)

options(stringsAsFactors = FALSE)

fun_dir <- 'C:/WorkSpace/GitHub/Temp_Trends'
data_query_dir <- 'F:/WorkSpace/MidCoast/Temp_Data/data_query'


# HUC 8
# 17100204 -- Siletz-Yaquina 
# 17100205 -- Alsea
# 17100206 -- Siuslaw
# 17100207 -- Siltcoos

HUClist <- c(17100204,17100205,17100206,17100207)

input <- list()
input$parms <- 'Temperature'
input$dates <- c("1999-06-01", "2015-10-31")
input$DQL <- "'A+', 'A', 'B'"
input$siteTypeLASAR <- "'Surface water'"
input$siteTypeNWIS <- "ST,ST-TS"


setwd(fun_dir)
source('funDataQuery.R')
source('funClean.R')
stations_wbd <- read.csv('station_wbd_12132016.csv',stringsAsFactors = FALSE)

for (i in 1:length(HUClist)) {
  
  # Get all the LASAR stations within each HUC8
  stationlist <- stations_wbd[stations_wbd$HUC8 %in% HUClist[i],]$STATION_KEY
  
  wqpData <- NULL
  nwisData <- NULL
  lasarData <- NULL
  
  wqpData <- tryCatch(wqpQuery(HUClist = HUClist[i],
                                inParms = input$parms,
                                luParms = parms,
                                startDate = input$dates[1],
                                endDate = input$dates[2]),
                       error = function(err) {err <- geterrmessage()})
  
  nwisData <- tryCatch(nwisQuery(HUClist = HUClist[i],
                                 inParms = input$parms,
                                 startDate = input$dates[1],
                                 endDate = input$dates[2],
                                 siteTypeCd = input$siteTypeNWIS),
                       error = function(err) {err <- geterrmessage()})
  
  lasarData <- lasarQuery(stationlist = stationlist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2],
                          DQL = input$DQL,
                          siteType = input$siteTypeLASAR)
  odbcCloseAll()
  
  if (is.data.frame(lasarData)) {lasarData$HUC <- HUClist[i]}
  
  if (!is.data.frame(nwisData)) {nwisData <- NULL}
  df.all <- combine(W=wqpData,L=lasarData,N=nwisData)
  
  setwd(data_query_dir)
  save(df.all, file=paste0(HUClist[i],'.Rdata'))
}
