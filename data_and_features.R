library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(geosphere)
library(DMwR)
library(bfast)
library(plotly)
library(tidyr)
library(gmapsdistance)

###functions
#obtain Haversine or driving distance between any 2 3-digit zip codes
set.api.key(key = 'MY_API_KEY')
get.api.key()

dMiles<-function(zip1, zip2, dHav = TRUE, dDriving = FALSE) {
  if (is.na(zip1) | is.na(zip2)) {
    return(NA)
  } else {
    #get coordinates for zip1 and zip2
    coords1<-as.data.frame(cbind(long1 = as.numeric(zip3_centroids[zip3_centroids$zip3==zip1,][3]),
                                 lat1 = as.numeric(zip3_centroids[zip3_centroids$zip3==zip1,][2])))
    coords2<-as.data.frame(cbind(long2 = as.numeric(zip3_centroids[zip3_centroids$zip3==zip2,][3]),
                                 lat2 = as.numeric(zip3_centroids[zip3_centroids$zip3==zip2,][2])))
    #get Miles 
    if (dHav == TRUE){
      d<-distHaversine(coords1, coords2)
      geoRes<-d*0.000621371
    } else {
      #note there is a bug in gmapsdistance code: https://github.com/rodazuero/gmapsdistance/issues/17 
      orig<-paste0(as.character(coords1$lat1),'+', as.character(round(coords1$long1,11)))
      dest<-paste0(as.character(coords2$lat2),'+', as.character(round(coords2$long2,11)))
      geoRes<-gmapsdistance(origin = orig, destination = dest, mode = "driving")
    } #else { #if (tGoogle == TRUE)
    #geoRes<-gmapsdistance(origin = orig, destination = dest, mode = "driving",
    #                      departure = "now", key = 'MY_API_KEY')
    #}
    return(geoRes)
  }
}

#obtain the mode of a vector 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#prepare MDC data for analysis
prepare_data2<-function(df){
  #create row ID on ordered data
  df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
  df$row_id <- seq.int(nrow(df))
  #format data - trim strings and make dates in Date format
  df[,c(2,7,8,11,12,13,30)]<-lapply(df[,c(2,7,8,11,12,13,30)],
                                    function(z) as.Date(as.character(z), format = "%m/%d/%Y"))
  for(i in dim(df)[2]){
    if (class(df[,i])=="chr"){
      df[,i]<-str_trim(df[,i])
    }
  }
  #add provider taxonomy description 
  df$PRVDR_TAXNMY_CD<-str_trim(as.character(df$PRVDR_TAXNMY_CD))
  df <- df %>% 
    left_join(select(tax2, "PROVIDER.TAXONOMY.DESCRIPTION", "PROVIDER.TAXONOMY.CODE"),
              by = c("PRVDR_TAXNMY_CD" = "PROVIDER.TAXONOMY.CODE"))
  colnames(df)[ncol(df)]<-"tax_desc"
  
  #remove certain rows from consideration - what abt 99199?
  #NCCI rules plus excludes telemed services, some paperwork-based codes 
  df<-df[df$MED_COST_CTG_CD %in% c(1,2,3,4) &
           !df$PROC_GRP_CD_DER %in% c(3,4,20,50) &
           !df$PRVDR_TAXNMY_GRP_CD_AES_RPT %in% c(31,32,50,99) &
           df$PLC_OF_SERV_CD!='02' &
           substring(df$PROC_CD_DER,1,4)!='9944' & 
           !df$PROC_CD_DER %in% c('99495','99002','99080') &
           !df$PAID_PROC_CD_MDFY %in% c('GT','GQ','95') & !df$PAID_PROC_CD_MDFY_SCND %in% c('GT','GQ','95'),]
  return(df)
}

#get the base zip code for claimant or provider 
get_zip_base<-function(df, clmnt = TRUE){
  if (clmnt == TRUE){
    df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
    clmnt_firstN<-ldply(by(df, df$CLM_ID, head, n=5), data.frame)
    clmnt_mode<-aggregate(PRVDR_ZIP_CD ~ CLM_ID, clmnt_firstN, Mode)
    df<-merge(df, clmnt_mode, by = "CLM_ID")
    colnames(df) <- gsub('.x','',colnames(df))
    colnames(df)[ncol(df)]<-"clmnt_zip_base"
  } else {
    df<-df[order(df$MED_PRVDR, df$DT_OF_SERV),]
    prov_firstN<-ldply(by(df, df$MED_PRVDR, head, n=30), data.frame)
    prov_mode<-aggregate(PRVDR_ZIP_CD ~ MED_PRVDR , prov_firstN, Mode)
    df<-merge(df, prov_mode, by = "MED_PRVDR")
    colnames(df) <- gsub('.x','',colnames(df))
    colnames(df)[ncol(df)]<-"prov_zip_base"
  }
  return(df)
}

#pass the formatted data to this function, cluster = TRUE for continuous variables only 
#I've commented out provider base zip-related features due to concerns abt fidelity of the data
get_features<-function(df, cluster = TRUE){
  df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
  
  #add some features 
  df<-as.data.frame(df) %>% 
    group_by(CLM_ID) %>% 
    mutate(prev_dos = dplyr::lag(DT_OF_SERV),
           prev_zip = dplyr::lag(PRVDR_ZIP_CD),
           prev_prov = dplyr::lag(MED_PRVDR),
           svc2acc = as.integer(DT_OF_SERV-ACDT_DT),
           trans2svc = as.integer(TRANS_DT-DT_OF_SERV),
           perc_paid = ifelse(PAID_AMT>0 & CHRG_AMT>0, round(PAID_AMT/CHRG_AMT*100,2), ifelse(PAID_AMT<=0,0,PAID_AMT)),
           nRows = n())
  df$days_from_prev<-as.integer(df$DT_OF_SERV-df$prev_dos)
  daily_counts <-as.data.frame(df %>% group_by(CLM_ID, DT_OF_SERV) %>% 
                                 summarise(nBill = n_distinct(BILL_ID), nProv = n_distinct(MED_PRVDR), nZip = n_distinct(PRVDR_ZIP_CD)))
  df<-as.data.frame(inner_join(df, daily_counts, by=c('CLM_ID','DT_OF_SERV')))
  
  #establish base zip and get miles from base for clmnt and prov
  df<-get_zip_base(df, clmnt = TRUE)
  #df<-get_zip_base(df, clmnt = FALSE)
  
  clmnt_miles_from_base<-round(mapply(dMiles, df$PRVDR_ZIP_CD, df$clmnt_zip_base, dHav = TRUE),2)
  #prov_miles_from_base<-round(mapply(dMiles, df$PRVDR_ZIP_CD, df$prov_zip_base, dHav = TRUE),2)
  df<-cbind(df, clmnt_miles_from_base) #, prov_miles_from_base)
  
  #get percentage of time at base and perc at this zip code on this row 
  zip_stats<- as.data.frame(df %>% group_by(CLM_ID, PRVDR_ZIP_CD) %>% 
                              summarise(nRow_at_zip = n()))
  df<-merge(df, zip_stats, by = c("CLM_ID", "PRVDR_ZIP_CD"), all.x = TRUE)
  df$perc_this_zip<-round(df$nRow_at_zip/df$nRows*100,2)
  perc_base<-as.data.frame(df %>% filter(PRVDR_ZIP_CD==clmnt_zip_base) %>% 
                             group_by(CLM_ID) %>% distinct(perc_this_zip))
  df<-merge(df, perc_base, by = "CLM_ID")
  colnames(df)[(ncol(df)-1):ncol(df)]<-c("perc_this_row_zip", "perc_clmnt_base_zip")
  
  #geo data
  df<-merge(df, geo_data, by.x = "clmnt_zip_base", by.y = "zip3", all.x = TRUE)
  
  #output features - if cluster = TRUE, only output continuous variables and row_id
  if(cluster==TRUE){
    #remove rows NA (first claim for a patient has NA prev DOS) 
    df<-df[,c("row_id","svc2acc","trans2svc","perc_paid","days_from_prev","nBill","nProv","nZip",
              "clmnt_miles_from_base","perc_this_row_zip","perc_clmnt_base_zip","zip3_sqMi","sqMi_per_hosp")]
    df<-df[complete.cases(df), ]
    return(df)
  } else {
    return(df)
  }
}


#calculate zip centroids
zip5<-read.delim("us_census_2013_zips.txt", header=TRUE, sep=",")
zip5$ZIP<-str_pad(zip5$ZIP,5, pad = "0")
zip5$zip3<-substring(zip5$ZIP,1,3)

zip3_centroids<-as.data.frame(zip5 %>%
                                group_by(zip3) %>%
                                summarize(mean_lat = mean(LAT), mean_long = mean(LNG)))

#load area data
zip_area<-read.delim("2015_gazateer.txt", header = TRUE, sep = "\t")
zip_area$GEOID<-str_pad(zip_area$GEOID,5,pad = "0")
zip_area$zip3<-substring(zip_area$GEOID,1,3)
zip3_area<-as.data.frame(zip_area %>%
                           group_by(zip3) %>%
                           summarise(zip3_sqMi = sum(ALAND_SQMI)))

#load Medicare hospital data
mc_hospitals<-read.csv("medicare_hospitals.csv")
mc_hospitals$zip<-str_pad(mc_hospitals$zip,5,pad = "0")
mc_hospitals$zip3<-substring(mc_hospitals$zip,1,3)
zip3_mc_hosp<-as.data.frame(mc_hospitals %>%
                              group_by(zip3 = as.character(zip3)) %>%
                              summarize(nHosp = n()))


#create geo data -- identify if the base zip in a large or underserved area
geo_data<-left_join(zip3_area, zip3_mc_hosp, by = "zip3")
geo_data$sqMi_per_hosp<-ifelse(!is.na(geo_data$nHosp), round(geo_data$zip3_sqMi/geo_data$nHosp,2), NA)
