EvaluateTempWQS <- function(sdadm_df) {
  # Description:
  # Evaluates temperature seven day average daily max values against Oregon's Water Quality Standards for Temperature
  #
  # This function takes 1 argument:
  #  sdadm_df             = A data frame with at minimum 5 columns which must be named
  #                           and formatted as specified in Details below.
  #
  # Details:
  #  Requires plyr and chron
  #
  #  sdadm_df must have columns with name and format as specified:
  #   id          = Class character representing the Station identifier
  #   date        = Class Date representing date of seven day average daily maximum
  #   sdadm       = Class numeric representing the values of the seven day average daily maximum
  #   spwn_dates  = Class character with the start and end dates of the 
  #                           applicable spawning time period. Requires the format 
  #                           "StartMonth Day-EndMonth Day" e.g. ("January 1-May 15") OR
  #                           "No spawning"
  #   ben_use_des = Class character with the beneficial use designation.
  #
  # ben_use_des must be one of:
  #   'Bull Trout Spawning and Juvenile Rearing',
  #   'Core Cold Water Habitat',
  #   'Salmon and Trout Rearing and Migration',
  #   'Salmon and Steelhead Migration Corridors',
  #   'Redband and Lanhontan Cutthroat Trout',
  #   'Cool water species',
  #   'No Salmonid Use/Out of State'
  # 
  # sdadm values are assumed to be in degrees celsius
  # 
  #
  # Value: 
  # An object of class data frame with columns:
  # 
  require(plyr)
  require(chron)
  #test case: 
  #sdadm_df$spwn_dates <- ifelse(sdadm$id %in% c(36837,36838, 36839, 36874),"August 15-May 15",ifelse(sdadm$id %in% c(36849,36850,36854,36857),"January 1-June 15","No spawning"))
  #sdadm_df$ben_use_des <- ifelse(sdadm$id %in% c(36837,36838, 36839, 36874),"Core Cold Water Habitat",ifelse(sdadm$id %in% c(36849,36850,36854,36857),"Salmon and Trout Rearing and Migration","Redband and Lanhontan Cutthroat Trout"))
  
  ## Build the spawning reference data frame based on the spawning dates and benefiicial use specified
  stations <- unique(sdadm_df$id)
  if ('ZDADM' %in% names(sdadm_df)) {
    spd <- unique(sdadm_df[,c('id','spwn_dates','ZDADM')])
  } else {
    spd <- unique(sdadm_df[,c('id','spwn_dates','ben_use_des')]) 
  }
  spd_list <- strsplit(spd$spwn_dates, split = "-")
  spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
  spd_months <- lapply(spd_chron, months)
  spd_days <- lapply(spd_chron, days)
  spd_months_num <- lapply(spd_months, as.numeric)
  spd_days_num <- lapply(spd_days, as.numeric)
  SSTART_MONTH <- unlist(lapply(spd_months_num, function(x) x[1]))
  SEND_MONTH <- unlist(lapply(spd_months_num, function(x) x[2]))
  SSTART_DAY <- unlist(lapply(spd_days_num, function(x) x[1]))
  SEND_DAY <- unlist(lapply(spd_days_num, function(x) x[2]))
  sdata <- cbind(spd, SSTART_MONTH, SSTART_DAY, SEND_MONTH, SEND_DAY)
  if (!'ZDADM' %in% names(sdadm_df)) {
    sdata$ZDADM <- suppressMessages(revalue(sdata$ben_use_des, c(
      'Bull Trout Spawning and Juvenile Rearing' = 12,
      'Core Cold Water Habitat' = 16,
      'Salmon and Trout Rearing and Migration' = 18,
      'Salmon and Steelhead Migration Corridors' = 20,
      'Redband and Lanhontan Cutthroat Trout' = 20,
      'Cool water species' = NA,
      'No Salmonid Use/Out of State' = NA
    ))
    ) 
  } 
  
  rm(spd,spd_list,spd_chron,spd_months,spd_days,
     spd_months_num,spd_days_num,SSTART_MONTH,
     SSTART_DAY,SEND_MONTH,SEND_DAY)
  
  ## Grab numeric spawning values
  sdadm_df$sdata <- match(sdadm_df$id,sdata$id)
  
  ## finds the current date, and spawning start/end date and formats as a numeric in the form mm.dd
  sdadm_df$cdate <- as.numeric(months(as.chron(sdadm_df$date))) + (as.numeric(days(as.chron(sdadm_df$date))) * .01)
  sdadm_df$sstr <- as.numeric(sdata$SSTART_MONTH[sdadm_df$sdata]) + (as.numeric(sdata$SSTART_DAY[sdadm_df$sdata]) *.01)
  sdadm_df$send <- as.numeric(sdata$SEND_MONTH[sdadm_df$sdata]) + (as.numeric(sdata$SEND_DAY[sdadm_df$sdata]) *.01)
  sdadm_df$bioc <- as.numeric(sdata$ZDADM[sdadm_df$sdata])
  
  ## checks to see if there is an over winter spawning period
  sdadm_df$winter <- ifelse(sdadm_df$send < sdadm_df$sstr, TRUE, FALSE)
  
  ## looks up the summer bio criterion and spawning start end/date and returns TRUE/FALSE if current date is in summer or spawning period
  sdadm_df$bioc <- ifelse(is.na(sdadm_df$winter), sdadm_df$bioc, ifelse(
    sdadm_df$winter == TRUE,
    ifelse(sdadm_df$sstr <= sdadm_df$cdate | sdadm_df$send >= sdadm_df$cdate, 13, sdadm_df$bioc),
    ifelse(sdadm_df$sstr <= sdadm_df$cdate & sdadm_df$send >= sdadm_df$cdate, 13, sdadm_df$bioc)))
  
  sdadm_df$summer <- ifelse(sdadm_df$bioc == 13, FALSE, TRUE)
  sdadm_df$spawn <- ifelse(sdadm_df$bioc == 13, TRUE, FALSE)
  
  sdadm_df <- sdadm_df[!is.na(sdadm$sdadm),]
  
  ## Calculate total 7DADM obersvations and # of 7DADM observations that exceed the summer spawning critera in those time periods; and 
  ## number of 7DADM observations that exceed 16 and 18 over the whole time period (not just in the stated periods)
  sdadm_df$exceedsummer <- ifelse(sdadm_df$sdadm >= sdadm_df$bioc & sdadm_df$summer == TRUE, 1, 0)
  sdadm_df$exceedspawn <- ifelse(sdadm_df$sdadm >= sdadm_df$bioc & sdadm_df$spawn == TRUE, 1, 0)
  sdadm_df$daystot <-ifelse(!is.na(sdadm_df$sdadm), 1, 0)
  
  ## Calculate begin/end date by station
  datemax <- tapply(as.character(sdadm_df[!is.na(sdadm_df$sdadm),'date']), sdadm_df[!is.na(sdadm_df$sdadm),'id'],max)
  datemin <- tapply(as.character(sdadm_df[!is.na(sdadm_df$sdadm),'date']), sdadm_df[!is.na(sdadm_df$sdadm),'id'],min)
  
  ## TABULUAR RESULTS
  daystot <- tapply(sdadm_df$daystot,list(sdadm_df$id,sdadm_df$daystot),length)
  exceedsummer <- tapply(sdadm_df$exceedsummer,list(sdadm_df$id,sdadm_df$exceedsummer),length)
  exceedspawn <- tapply(sdadm_df$exceedspawn,list(sdadm_df$id,sdadm_df$exceedspawn),length)
  
  attr(sdadm_df, "result_summary") <- ddply(sdadm_df, .(id), summarise, 
                                            exceedspawn = sum(exceedspawn),
                                            exceedsummer = sum(exceedsummer),
                                            daystot = sum(daystot))
  
  sdadm_df <- within(sdadm_df, rm(cdate, sstr, send, winter, summer, spawn, daystot))
  
  names(sdadm_df)[names(sdadm_df) == 'bioc'] <- 'criteria_value'
  
  return(sdadm_df)
}