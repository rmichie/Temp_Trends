dsc <- function (df, id_col, y_col, d_col, h_col, month_num) {
  #dsc = Data Sufficiency Check function
  #Function to screen data for minmum data requirements
  #df = data frame of continuous temperature data
  #id_col = column name in df holding station id
  #y_col = column name in df holding numeric year
  #d_col = column name in df holding numeric day
  #h_col = column name in df holding numeric hour
  #month_num = numeric representation of month (e.g. August would be 8)
  
  #### QC Screen for data sufficiency ####
  #First determine number of hours collected within each day
  hr <- as.tbl(df) %>%
    group_by_(id_col, y_col, d_col) %>%
    summarise(n = length(unique(h_col)))
  hr <- as.data.frame(hr)
  
  #Isolate to days with 22 or more hours represented
  hrm <- hr[hr$n >= 22,]
  hrm$code <- paste(hrm[,id_col], hrm[,y_col], hrm[,d_col])
  df$code <- paste(df[,id_col], df[,y_col], df[,d_col])
  df <- df[df$code %in% hrm$code,]
  
  #Determine the number of days with at least 22 hours represented
  hrm <- as.data.frame(as.tbl(hrm) %>%
                         group_by_(id_col, y_col) %>%
                         summarise(n = n()))
  
  #Isolate to years with at least 30 days of adequate data
  min_days <- ifelse(month_num %in% c(1,3,5,7,8,10,12), 30, 
                     ifelse(month_num == 2, 27, 29)) #TODO: resolve handling of minumum days in february
  hrmy <- hrm[hrm$n >= min_days,]
  hrmy$code <- paste(hrmy[,id_col], hrmy[,y_col])
  df$code <- paste(df[,id_col], df[,y_col])
  df <- df[df$code %in% hrmy$code,]
  
  #Determine number of years with at least 30 days of data
  keep <- as.data.frame(as.tbl(hrmy) %>%
                          group_by_(id_col) %>%
                          summarise(n = n()))
  #Pull out those stations with at least 8 years where there were 30 days 
  # with at least 22 hours sampled in the month of August
  qc_pass <- keep[keep$n >= month_num, ]
  
  #Pare the data down to the stations that have data that pass the qc requirements
  df <- df[df[,id_col] %in% qc_pass$Station_ID,]
  
  return(df)
}
