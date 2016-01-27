library(lubridate)
library(ggplot2)
library(dplyr)
library(wq)
library(reshape2)

fun_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends'
data_query_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends/data_query_NWIS'
plot_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends/plots'
output_dir <- 'F:/WorkSpace/Quantifying_Conservation_2014/SouthernWillamette/TempTrends/outputs'

setwd(fun_dir)
source('funCalculateSDADM_narm.R')
source('funMultiplot.R')

options(stringsAsFactors = FALSE)
setwd(data_query_dir)
load('data_all_lists.Rdata')

# Isolate station info for mapping in order to bring in the applicable standard
# Run the following nine lines once and read back in when standard has been added
#-------------------------------------------------------------------------------------
# stns <- read.csv('/data_query_NWIS/qc_pass_station_summary.csv')
# stns_or <- read.csv('T:/temptrends/station_criteria_state.csv')
# stns <- stns[!duplicated(stns$Station_ID),]
# stns[is.na(stns$DATUM),'DATUM'] <- 'EPSG:4269'
# stns_df <- merge(stns, stns_or[,c('station_id','ZDADM','spawn_dates')],
#                by.x="Station_ID",by.y="station_id",all.x=TRUE)
# write.csv(stns_df, file = 'qc_pass_station_summary_new.csv',row.names = FALSE)

stns <- read.csv('qc_pass_station_summary.csv')

df_list <- lapply(df_list, function(x) {
  x <- merge(x, stns[,c('Station_ID','ZDADM','ben_use','spawn_dates')], by = 'Station_ID', all.x = TRUE)
  return(x)
})

#### Calculate sdadm ####
sdadm_list <- lapply(seq_along(df_list) , function (x) {
  if(nrow(df_list[[x]]) > 0) {
  sdadm <- Calculate.sdadm(df_list[[x]], "Result", "Station_ID", "date", "%Y-%m-%d")
  sdadm$year <- year(sdadm$date)
  sdadm$month <- month(sdadm$date)
  sdadm$day <- day(sdadm$date)
  sdadm <- sdadm[sdadm$month == names(df_list)[[x]] & sdadm$day < 26,]
  sdadm <- merge(sdadm, stns[,c('Station_ID','ZDADM','ben_use', 'spawn_dates')], by.x = 'id', by.y = 'Station_ID', all.x = TRUE)
  return(sdadm)
  }
})

#### Average monthly sdadm ####
#Determine average sdadm by year and calcualte trend
amean_list <- lapply(sdadm_list, function (sdadm) {
  if (!is.null(sdadm)) {
  tapply(sdadm$sdadm, list(sdadm$year, sdadm$id), 
                function(x) {
                  ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
                })
}
  })
tmean_list <- lapply(amean_list, function (amean) {
  if (!is.null(amean)) {
    mannKen(ts(amean))
  }
  })

#### Average monthly daily cumulative degree hours > WQS ####
tdha_list <- lapply(df_list, function (aug) {
  #First take the maximum from each hour of data
  dh <- as.tbl(aug) %>%
    group_by(Station_ID, ZDADM, year, day, hour) %>%
    summarise(Result = max(Result))
  
  #Build id for efficient grouping
  dh$code <- paste(dh$Station_ID, dh$year, dh$day)
  
  #Calculate degree difference for each hourly max
  dh$dd <- as.numeric(dh$Result) - as.numeric(dh$ZDADM)
  
  #Set all negative values to 0
  dh$dd <- ifelse(dh$dd < 0, 0, dh$dd)
  
  #Sum the positive degree differences to derive cumulative degree hours > WQS
  dh_sum <- dh %>% 
    group_by(Station_ID, ZDADM, year, code) %>% 
    summarise(dh = sum(dd))
  
  #Derive the monthly average of daily degree hours > WQS
  dh_avg <- dh_sum %>%
    group_by(Station_ID, ZDADM, year) %>%
    summarise(dh_avg = mean(dh))
  
  #Calculate trend on average daily degree hours > WQS
  dha_wide <- cast(dh_avg, year ~ Station_ID, value = "dh_avg")
  tdha <- mannKen(ts(dha_wide[-1]))
  return(tdha)
})

dhs_list <- lapply(df_list, function (aug) {
  #First take the maximum from each hour of data
  dh <- as.tbl(aug) %>%
    group_by(Station_ID, ZDADM, year, day, hour) %>%
    summarise(Result = max(Result))
  
  #Build id for efficient grouping
  dh$code <- paste(dh$Station_ID, dh$year, dh$day)
  
  #Calculate degree difference for each hourly max
  dh$dd <- as.numeric(dh$Result) - as.numeric(dh$ZDADM)
  
  #Set all negative values to 0
  dh$dd <- ifelse(dh$dd < 0, 0, dh$dd)
  
  #Sum the positive degree differences to derive cumulative degree hours > WQS
  dh_sum <- dh %>% 
    group_by(Station_ID, ZDADM, year, code) %>% 
    summarise(dh = sum(dd))
})

dha_list <- lapply(dhs_list, function (dh_sum) {
  #Derive the monthly average of daily degree hours > WQS
  dh_avg <- dh_sum %>%
    group_by(Station_ID, ZDADM, year) %>%
    summarise(dh_avg = mean(dh))
})
  
msm <- lapply(tmean_list, function(x) {
  row.names(x)[x[,3] < 0.1]
})
hss <- lapply(tdha_list, function(x) {
  y <- row.names(x)[x[,3] < 0.1]
  y <- y[!is.na(y)]
  return(y)
})

# To plot all stns regardless of significant trend first we need a list of the stations
stns_to_graph <- stns$Station_ID

setwd(plot_dir)

a <- NULL
b <- NULL
c <- NULL
d <- NULL
p1 = NULL
p2 = NULL
sig <- ""
tmonths <- c('June','July','August','September','October')
tmonths2 <- c('06', '07', '08', '09', '10', '11')
for (j in 1:5) {
  p1_btm <- floor(range(sdadm_list[[j]]$sdadm, na.rm = TRUE))[1]
  p1_top <- ceiling(range(sdadm_list[[j]]$sdadm, na.rm = TRUE))[2]
  p2_btm <- floor(range(dhs_list[[j]]$dh, na.rm = TRUE))[1]
  p2_top <- ceiling(range(dhs_list[[j]]$dh, na.rm = TRUE))[2]
  for(i in 1:length(stns_to_graph)) {
      #Boxplots of 7DADM
      df <- sdadm_list[[j]]
      df <- df[df$id == stns_to_graph[i],]
      if (nrow(df) == 0) {next}
      df <- df[!is.na(df$sdadm),]
      df$year <- factor(df$year, levels = min(df$year):max(df$year))
      zdadm_stn <- stns[stns$Station_ID == stns_to_graph[i],c("ZDADM")]
      a <- ggplot(data = df, aes(x=year, y=sdadm)) + 
        geom_boxplot() + 
        theme_bw() + 
        theme(axis.title = element_text(size = 8),
              plot.title = element_text(size = 8, face = "bold")) +
        xlab("Year") + 
        scale_y_continuous(breaks = seq(p1_btm, p1_top, by = 2),
                           labels = seq(p1_btm, p1_top, by = 2),
                           lim = c(p1_btm,p1_top)) +
        scale_x_discrete(drop = FALSE) +
        ylab("Temperature (degrees C)") +
        ggtitle("7 Day Average Daily Maximum Temperature") +
        geom_abline(intercept = zdadm_stn, slope = 0, colour = "red", size = 1.01) + 
        annotate("text", label = "Water Quality Standard", 
                 x = ifelse(min(df$sdadm) > 14, 7.5, 3.5), y =zdadm_stn + 0.3,
                 colour = "red", size = 3.5)
      
      
      df <- as.data.frame(amean_list[[j]])
      df$year <- row.names(df)
      df <- melt(df)
      df <- df[df$variable == stns_to_graph[i],]
      df$year <- as.numeric(df$year)
      df <- df[!is.na(df$value),]

      b <- ggplot(data = df, aes(x = year, y = value)) + 
        geom_point(aes(size = 2)) +
        scale_y_continuous(breaks = seq(p1_btm,p1_top, by = 2),
                           labels = seq(p1_btm,p1_top, by = 2),
                           lim = c(p1_btm,p1_top)) +
        scale_x_continuous(breaks = seq(min(df$year),max(df$year),by=1),
                           labels = seq(min(df$year),max(df$year),by=1),
                           lim = c(min(df$year),max(df$year))) +
        xlab("Year") + 
        ggtitle("Average 7 Day Average Daily Maximum Temperature") +
        ylab("Temperature (degrees C)") +
        guides(size = FALSE) +
        theme_bw() +
        theme(axis.title = element_text(size = 8),
              plot.title = element_text(size = 8, face = "bold")) 
      
      if (stns_to_graph[i] %in% msm[[j]]) {
        sig <- "_sig"
        #Trend plot with points as average 7DADM
        slope <- tmean_list[[j]][which(attr(tmean_list[[j]], 
                                            "dimnames")[[1]] == stns_to_graph[i]),1]
        p1 <- tmean_list[[j]][which(attr(tmean_list[[j]], 
                                        "dimnames")[[1]] == stns_to_graph[i]),3]
        x.delta <- as.numeric((max(df$year) - min(df$year)))/2
        SK.min <- median(df$value, na.rm = TRUE) - x.delta*slope
        SK.max <- median(df$value, na.rm = TRUE) + x.delta*slope
        b <- b + geom_segment(x = min(df$year), y = SK.min,
                              xend = max(df$year), yend = SK.max,
                              linetype = 2, size = 1.05)      
       }
    
      b <- b + annotate("text", x = min(df$year) + 3, y = p1_top - 0.5, 
                        label = ifelse(is.null(p1), "No Trend", 
                                       ifelse(p1 < 0.1, 
                                              paste("Significant Trend (p-value",
                                                    ifelse(p1 < 0.05, "< 0.05)", 
                                                           "< 0.1)")), "")), 
                        size = 3.5)
    
      #Boxplots of daily degree hours > WQS
      df <- dhs_list[[j]]
      df <- df[df$Station_ID == stns_to_graph[i],]
      df$year <- factor(df$year, levels = min(df$year):max(df$year))
      c <- ggplot(data = df, aes(x=year, y=dh)) + 
        geom_boxplot() + 
        theme_bw() + 
        theme(axis.title = element_text(size = 8),
              plot.title = element_text(size = 8, face = "bold")) +
        xlab("Year") + 
        scale_y_continuous(breaks = c(seq(p2_btm,p2_top,by=5)),
                           labels = c(seq(p2_btm,p2_top,by=5)),
                           lim = c(p2_btm,p2_top)) +
        scale_x_discrete(drop = FALSE) +
        ggtitle("Daily Degree Hours Above Water Quality Standard") +
        ylab("Daily degree hours (degrees C)")
      
      
      df <- dha_list[[j]]
      df <- df[df$Station_ID == stns_to_graph[i],]
      d <- ggplot(data = df, aes(x = year, y = dh_avg)) + 
        geom_point(aes(size = 1.01)) +
        scale_y_continuous(breaks = seq(p2_btm,p2_top, by = 5),
                           labels = seq(p2_btm,p2_top, by = 5),
                           lim = c(p2_btm,p2_top)) +
        scale_x_continuous(breaks = seq(min(df$year),max(df$year),by=1),
                           labels = seq(min(df$year),max(df$year),by=1),
                           lim = c(min(df$year),max(df$year))) +
        xlab("Year") + 
        ggtitle("Average Daily Degree Hours Above Water Quality Standard") +
        ylab("Daily degree hours (degrees C)") +
        guides(size = FALSE) +
        theme_bw() +
        theme(axis.title = element_text(size = 8),
              plot.title = element_text(size = 8, face = "bold")) 
      
      if (stns_to_graph[i] %in% hss[[j]]) {
        sig = "_sig"
        #Trend plot with points as average daily degree hours > WQS
        slope <- tdha_list[[j]][which(attr(tdha_list[[j]], 
                                           "dimnames")[[1]] == stns_to_graph[i]),1]
        p2 <- tdha_list[[j]][which(attr(tdha_list[[j]], 
                                       "dimnames")[[1]] == stns_to_graph[i]),3]
        x.delta <- as.numeric((max(df$year) - min(df$year)))/2
        SK.min <- median(df$dh_avg) + 3 - x.delta*slope
        SK.max <- median(df$dh_avg) + 3 + x.delta*slope
        d <- d + geom_segment(aes(x = min(df$year), y = SK.min,
                                  xend = max(df$year), yend = SK.max),
                              linetype = 2, size = 1.01)
      }
      
      d <- d + annotate("text", x = min(df$year) + 3, y = ifelse(p2_top == 1, p2_top, p2_top - 1.5), 
                        label = ifelse(is.null(p2), "No trend", 
                                       ifelse(p2 < 0.1, 
                                              paste("Significant Trend (p-value", 
                                                    ifelse(p2 < 0.05, "< 0.05)", 
                                                           "< 0.1)")), "")), 
                                       size = 3.5)
    
      title_stn <- stns[stns$Station_ID == stns_to_graph[i],]
    
      png(file = paste(stns_to_graph[i], "_", tmonths2[j], "_plot", sig,".png", sep = ""),
          width = 11, height = 8.5, units = "in", res = 100)
      multiplot(a, c, b, d, cols = 2, title = paste(title_stn$Station_ID, 
                                                    title_stn$Station_Description,
                                                    tmonths[j],
                                                    sep = " - "))
      dev.off()
    
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    sig <- ""
    p1 = NULL
    p2 = NULL
    }
  
}

#Output tmean_list and tdha_list as tables for Ryan
tmean_dfs <- lapply(seq_along(tmean_list), function (x) {
  y <- tmean_list[[x]]
  df <- as.data.frame(y, row.names = attr(y, "dimnames")[[1]])
  df$Station_ID <- row.names(df)
  df <- merge(df, stns[,c('Station_ID', 'Station_Description', 'HUC')], all.x = TRUE)
  df$Month <- tmonths[x]
  df$Month2 <- tmonths2[x]
  df$Result <-ifelse(df$p.value <=0.10,'Significant','Not Significant')
  return(df)
})

tmean_df <- rbind_all(tmean_dfs)

setwd(output_dir)
write.csv(tmean_df, file = "Trend_Results_Avg_SDADM.csv",row.names = FALSE)

tdha_dfs <- lapply(seq_along(tdha_list), function (x) {
  y <- tdha_list[[x]]
  df <- as.data.frame(y, row.names = attr(y, "dimnames")[[1]])
  df$Station_ID <- row.names(df)
  df <- merge(df, stns[,c('Station_ID', 'Station_Description', 'HUC')], all.x = TRUE)
  df$Month <- tmonths[x]
  df$Month2 <- tmonths2[x]
  df$Result <-ifelse(df$p.value <=0.10,'Significant','Not Significant')
  return(df)
})

tdha_df <- rbind_all(tdha_dfs)
write.csv(tdha_df, file = "Trend_Results_DailyDegreeHoursAboveWQS.csv",row.names = FALSE)
