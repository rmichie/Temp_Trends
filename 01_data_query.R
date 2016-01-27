library(dataRetrieval)
library(data.table)
library(plyr)
library(reshape)
library(RODBC)

options(stringsAsFactors = FALSE)

fun_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends'
data_query_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends/data_query'

# Middle Fork Willamette (17090001)
# Coast Fork Willamette (17090002)
# Upper Willamette (17090003)
# McKenzie (17090004)
# North Santiam (17090005)
# South Santiam (17090006)

HUClist <- c(17090001,17090002,17090003,17090004,17090005,17090006)

input <- list()
input$parms <- 'Temperature'
input$dates <- c("1998-06-01", "2015-10-31")
input$DQL <- "'A+', 'A', 'B'"
input$siteTypeLASAR <- "'Surface water'"
input$siteTypeNWIS <- "ST,ST-TS"

setwd(fun_dir)
source('funDataQuery.R')
source('funClean.R')

for (i in 1:length(HUClist)) {
  
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
  
   lasarData <- lasarQuery(HUClist = HUClist[i],
                           inParms = input$parms,
                           startDate = input$dates[1],
                           endDate = input$dates[2],
                           DQL = input$DQL,
                           siteType = input$siteTypeLASAR)
  odbcCloseAll()
  
  if (!is.data.frame(nwisData)) {nwisData <- NULL}
  df.all <- combine(W=wqpData,L=lasarData,N=nwisData)
  
  setwd(data_query_dir)
  save(df.all, file=paste0(HUClist[i],'.Rdata'))
}
