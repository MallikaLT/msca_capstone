library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(geosphere)
library(DMwR)
library(bfast)
library(tidyr)
library(gmapsdistance)
library(sqldf)
library(fuzzyjoin)
library(lubridate)

#load data needed
#calculate zip centroids
zip5<-read.delim("us_census_2013_zips.txt", header=TRUE, sep=",")
zip5$ZIP<-str_pad(zip5$ZIP,5, pad = "0")
zip5$zip3<-substring(zip5$ZIP,1,3)

zip3_centroids<-as.data.frame(zip5 %>%
                                group_by(zip3) %>%
                                summarize(mean_lat = mean(LAT), mean_long = mean(LNG)))

#get area data
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

#get taxonomy decsription 
tax<-read.csv("cms_taxonomies.csv")
tax$PROVIDER.TAXONOMY.CODE<-str_trim(tax$PROVIDER.TAXONOMY.CODE)
tax2<-unique(tax[c("PROVIDER.TAXONOMY.CODE","PROVIDER.TAXONOMY.DESCRIPTION")])
tax2<-ddply(tax2, "PROVIDER.TAXONOMY.CODE", head, 1)

#create geo data -- identify if the base zip in a large or underserved area
geo_data<-left_join(zip3_area, zip3_mc_hosp, by = "zip3")
geo_data$sqMi_per_hosp<-ifelse(!is.na(geo_data$nHosp), round(geo_data$zip3_sqMi/geo_data$nHosp,2), NA)


###functions
#obtain Haversine or driving distance between any 2 3-digit zip codes
#f you want driving dist, need to obtain an API key -- as of APril 2018, gmapsdistance code had a bug
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
  df<-df[order(df$CLM_ID, df$DT_OF_SERV, df$PRVDR_ZIP_CD),]
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
  df$service_yr<-year(as.Date(df$DT_OF_SERV, format = "%m/%d/%Y"))
  #df$PRVDR_ZIP_CD<-as.character(df$PRVDR_ZIP_CD)
  
  #remove certain rows from consideration 
  df<-df[df$MED_COST_CTG_CD %in% c(1,2,3,4) & #ASC, OP, IP, physician office
           as.character(df$PRVDR_ZIP_CD) %in% zip3_centroids[,1] & #valid 3 digit zip codes only
           !df$PROC_GRP_CD_DER %in% c(4,20) & #pathology, NDC
           !substring(df$PROC_CD_DER,1,2) %in% c("A4","A5","A6","A7","A8","A9") & #non ambulance A codes
           !substring(df$PROC_CD_DER,1,1) %in% c("B","E","L","K","P","R","Q","V","G") & #most HCPCS cods
           !df$PRVDR_TAXNMY_GRP_CD_AES_RPT %in% c(31,32,50,99) &
           !(df$PROC_GRP_CD_DER==3 & (df$PAID_PROC_CD_MDFY=='26'|df$PAID_PROC_CD_MDFY_SCND=='26')) & #prof component radiology
           !(nchar(as.character(df$PROC_CD_DER))==3 & substring(df$PROC_CD_DER,1,2)=='03') & #hospital-based pathology
           df$ACDT_DT <= df$DT_OF_SERV & #valid date data only 
           df$TRANS_DT >= df$DT_OF_SERV &
           df$PLC_OF_SERV_CD!='02' & #telemed
           substring(df$PROC_CD_DER,1,4)!='9944' & 
           df$PROC_CD_DER!='' & df$ICD10_DX_DER!=''& df$ICD10_DX_SCND_DER!=''& #remove rows w no DX or proc code 
           !df$PROC_CD_DER %in% c('99495','99002','99080') &
           !df$PAID_PROC_CD_MDFY %in% c('GT','GQ','95') & !df$PAID_PROC_CD_MDFY_SCND %in% c('GT','GQ','95'),]
  df$PROC_GRP_CD_DER<-as.factor(df$PROC_GRP_CD_DER)
  df$PRVDR_TAXNMY_GRP_CD_AES_RPT<-as.factor(df$PRVDR_TAXNMY_GRP_CD_AES_RPT)
  return(df)
}

#get the base zip code for claimant or provider 
#using mode of all PT services
get_zip_basePT<-function(df){
  pt_svcs<-df[substring(df$PROC_CD_DER,1,3)==970 | substring(df$PROC_CD_DER,1,3)==971,  ]
  clmnt_mode<-aggregate(PRVDR_ZIP_CD ~ CLM_ID, pt_svcs, Mode)
  df<-merge(df, clmnt_mode, by = "CLM_ID")
  colnames(df) <- gsub('.x','',colnames(df))
  colnames(df)[ncol(df)]<-"clmnt_zip_base"
  return(df)
}

#using mode of first N services (default N = 5) -- also includes code for provider base
get_zip_base<-function(df, clmnt = TRUE){
  if (clmnt == TRUE){
    #claimany base zip code
    df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
    clmnt_firstN<-ldply(by(df, df$CLM_ID, head, n=10), data.frame)
    clmnt_mode<-aggregate(PRVDR_ZIP_CD ~ CLM_ID, clmnt_firstN, Mode)
    df<-merge(df, clmnt_mode, by = "CLM_ID")
    colnames(df) <- gsub('.x','',colnames(df))
    colnames(df)[ncol(df)]<-"clmnt_zip_base"
  } else {
    #provide base zip code
    df<-df[order(df$MED_PRVDR, df$DT_OF_SERV),]
    prov_firstN<-ldply(by(df, df$MED_PRVDR, head, n=30), data.frame)
    prov_mode<-aggregate(PRVDR_ZIP_CD ~ MED_PRVDR , prov_firstN, Mode)
    df<-merge(df, prov_mode, by = "MED_PRVDR")
    colnames(df) <- gsub('.x','',colnames(df))
    colnames(df)[ncol(df)]<-"prov_zip_base"
  }
  return(df)
}

#get zip base but at yearly (via service date) level 
#no longer first N, but mode of all services that year 
get_yearly_zip_base<-function(df){
  df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
  clmnt_mode_yr<-aggregate(PRVDR_ZIP_CD ~ CLM_ID + service_yr, df, Mode)
  df<-merge(df, clmnt_mode_yr, by = c("CLM_ID", "service_yr"))
  names(df) <- gsub(".y", "", names(df))
  names(df) <- gsub(".x", "", names(df))
  colnames(df)[ncol(df)]<-"clmnt_zip_base"
  return(df)
}

#this returns all features
#this function uses the get_zip_base YEARLY function 
complex_svc_codes<-read.csv("complex_svc_codes.csv")
complex_svc_codes<-ifelse(nchar(complex_svc_codes$proc_code==3),str_pad(complex_svc_codes$proc_code,4, pad = "0"),complex_svc_codes$proc_code)

get_features2<-function(df, cluster = TRUE){
  df<-df[order(df$CLM_ID, df$DT_OF_SERV, df$PRVDR_ZIP_CD),]
  df$PROC_CD_DER<-as.character(df$PROC_CD_DER)
  df$ICD10_DX_DER<-as.character(df$ICD10_DX_DER)
  df$ICD10_DX_SCND_DER<-as.character(df$ICD10_DX_SCND_DER)
  
  #add some features 
  q<-df[,c("CLM_ID","DT_OF_SERV")]  %>% distinct()
  q<-as.data.frame(q %>% group_by(CLM_ID) %>% mutate(unique_prev_dos = as.Date(lag(DT_OF_SERV))))
  df<-inner_join(df, q, by = c("CLM_ID","DT_OF_SERV"))
  df$days_from_prev<-as.integer(df$DT_OF_SERV-df$unique_prev_dos)
  
  df<-as.data.frame(df) %>% 
    group_by(CLM_ID) %>% 
    mutate(
           prev_zip = dplyr::lag(PRVDR_ZIP_CD),
           prev_prov = dplyr::lag(MED_PRVDR),
           svc2acc = as.integer(DT_OF_SERV-ACDT_DT),
           trans2svc = as.integer(TRANS_DT-DT_OF_SERV),
           perc_paid = ifelse(PAID_AMT>0 & CHRG_AMT>0, round(PAID_AMT/CHRG_AMT*100,2), ifelse(PAID_AMT<=0,0,PAID_AMT)),
           er_visit = as.factor(ifelse((PROC_CD_DER %in% c('0450','0451','0452','0456','0459','0981','99281','99282','99283','99284','99285','99286','99287','99288')|PLC_OF_SERV_CD =='23'),1,0)),
           compl_svc = as.factor(ifelse(PROC_CD_DER %in% complex_svc_codes,1,0)),
           sev_inj = as.factor(ifelse((substr(ICD10_DX_DER,1,5) %in% c('S06.1','S06.2','S06.3','S06.4','S06.5','S06.6','S06.7','S06.8'))|
                            (substr(ICD10_DX_SCND_DER,1,5) %in% c('S06.1','S06.2','S06.3','S06.4','S06.5','S06.6','S06.7','S06.8'))|
                            (substr(ICD10_DX_DER,1,3) %in% c('T20','T21','T22','T23','T24','T25','T26','T27','T28'))|
                            (substr(ICD10_DX_SCND_DER,1,3) %in% c('T20','T21','T22','T23','T24','T25','T26','T27','T28'))|
                            (substr(ICD10_DX_DER,1,3) %in% c('S14','S24','S34','I63','C71','G82','G83','S88','S48','S58','S68','S78','S98','S38','S08','S36','S37','T20','T30','T31','T32','S67','S17','S07','S28'))| 
                            (substr(ICD10_DX_SCND_DER,1,3) %in% c('S14','S24','S34','I63','C71','G82','G83','S88','S48','S58','S68','S78','S98','S38','S08','S36','S37','T20','T30','T31','T32','S67','S17','S07','S28'))|
                            (substr(ICD10_DX_DER,1,5)=='277.0')|
                            (substr(ICD10_DX_SCND_DER,1,5)=='277.0')|
                            (substr(ICD10_DX_DER,1,7) %in% c('S06.9X5','S06.9X6','S06.9X7','S06.9X8'))|
                            (substr(ICD10_DX_SCND_DER,1,7) %in% c('S06.9X5','S06.9X6','S06.9X7','S06.9X8'))|
                            ICD10_DX_DER %in% c('N28.9','G95.9')| ICD10_DX_SCND_DER %in% c('N28.9','G95.9'),1,0)))

  #get daily count features (zip codes per day, providers per day, services per day)
  daily_counts <-as.data.frame(df %>% group_by(CLM_ID, DT_OF_SERV) %>% 
                                 summarise(nBill = n_distinct(BILL_ID), nProv = n_distinct(MED_PRVDR), nZip = n_distinct(PRVDR_ZIP_CD)))
  df<-as.data.frame(inner_join(df, daily_counts, by=c('CLM_ID','DT_OF_SERV')))
  df$diff_prov<-as.factor(ifelse(df$MED_PRVDR==df$prev_prov,0,1))
  
  #establish base zip and get miles from base for clmnt as well as miles from previous svc
  df<-get_yearly_zip_base(df)
  clmnt_miles_from_base<-round(mapply(dMiles, df$PRVDR_ZIP_CD, df$clmnt_zip_base, dHav = TRUE),2)
  miles_from_prev<-round(mapply(dMiles, df$PRVDR_ZIP_CD, df$prev_zip, dHav = TRUE),2)
  df<-cbind(df, clmnt_miles_from_base, miles_from_prev) 
  
  #normalize miles from base and get miles per day since last service
  df$mfb_norm<-df$clmnt_miles_from_base/sd(df$clmnt_miles_from_base)
  df$avg_daily_miles_from_prev<-ifelse(!is.na(df$ds_from_prev) & df$ds_from_prev > 0 & 
                                       !is.na(df$miles_from_prev) & df$miles_from_prev > 0,
                                       df$miles_from_prev/df$ds_from_prev,0)
  #mfp won't work unless we remove the first service for every claimant (it's NA for mfp)
  #df$mfp_norm<-ifelse(is.na(df$miles_from_prev), NA, df$miles_from_prev/sd(df$miles_from_prev[2:nrow(df)]))
  
  #geo data
  df<-merge(df, geo_data, by.x = "clmnt_zip_base", by.y = "zip3", all.x = TRUE)
  df<-df[order(df$CLM_ID, df$DT_OF_SERV, df$PRVDR_ZIP_CD),]
  return(df)
}


