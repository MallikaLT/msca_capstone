library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(geosphere)
library(DMwR)
library(bfast)
library(plotly)
library(tidyr)

###functions
#obtain centroid coordinates by inputting 3 digit zip code
get_coords<-function(zip){
  if (is.na(zip)) {
    return(NA)
  } else {
    long<-as.numeric(zip3_centroids[zip3_centroids$zip3==zip,][3])
    lat<-as.numeric(zip3_centroids[zip3_centroids$zip3==zip,][2])
    coords<-as.data.frame(cbind(long,lat))
    return(coords)
  }
}

#obtain miles between 2 sets of coordinates 
dMiles<-function(long1,lat1,long2,lat2){
  if (is.na(long1) | is.na(long2) | is.na(lat1) | is.na(lat2)) {
    return(NA)
  } else {
    d<-distHaversine(c(long1,lat1), c(long2,lat2))
    dMiles<-d*0.000621371
    return(dMiles)
  }
}

#obtain the mode of a vector 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#prepare MDC data for analysis
prepare_data<-function(df){
  #create row ID on ordered data
  df$row_id <- seq.int(nrow(df))
  #format data - trim strings and make dates in Date format
  df[,c(2,7,8,11,12,13,30)]<-lapply(df[,c(2,7,8,11,12,13,30)],
                                    function(z) as.Date(as.character(z), format = "%m/%d/%Y"))
  for(i in dim(df)[2]){
    if (class(df[,i])=="chr"){
      df[,i]<-str_trim(df[,i])
    }
  }
  df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
  #remove hard-coding when you have time -- add this to for loop 
  #if (class(df[,i])=="Date"){
  #df[,i]<-as.Date(as.character(df[,i]), format = "%m/%d/%Y")
  #} else if (class(df[,i])=="Factor" & grepl("DT", colnames(df[,i]))) {
  #df[,i]<-as.Date(as.character(df[,i]), format = "%m/%d/%Y")
  #}
  #add provider taxonomy description 
  df$PRVDR_TAXNMY_CD<-str_trim(as.character(df$PRVDR_TAXNMY_CD))
  df <- df %>% 
    left_join(select(tax2, "PROVIDER.TAXONOMY.DESCRIPTION", "PROVIDER.TAXONOMY.CODE"),
              by = c("PRVDR_TAXNMY_CD" = "PROVIDER.TAXONOMY.CODE"))
  colnames(df)[43]<-"tax_desc"
  return(df)
}

#pass the formatted data to this function, cluster = TRUE for continuous variables only 
get_features<-function(df, cluster = TRUE){
  df<-df[order(df$CLM_ID, df$DT_OF_SERV),]
  
  #remove certain rows from consideration - what abt 99199?
  df<-df[df$PROC_GRP_CD_DER %in% c(1,2,3,5,6,30,45,50,60,75) &
           !substring(df$PROC_CD_DER,1,1) %in% c("A","B","E","L","K") &
           !df$PROC_CD_DER %in% c("S9999","99002", "99070","99080") &
           (nchar(as.character(df$PROC_CD_DER))==5 | 
              nchar(as.character(df$PROC_CD_DER))==4), ]
  
  #add some features 
  df<-as.data.frame(df) %>% 
    group_by(CLM_ID) %>% 
    mutate(prev_dos = dplyr::lag(DT_OF_SERV),
           prev_zip = dplyr::lag(PRVDR_ZIP_CD),
           prev_prov = dplyr::lag(MED_PRVDR),
           svc2acc = as.integer(DT_OF_SERV-ACDT_DT),
           trans2svc = as.integer(TRANS_DT-DT_OF_SERV),
           perc_paid = round(PAID_AMT/CHRG_AMT*100,2))
  df$days_from_prev<-as.integer(df$DT_OF_SERV-df$prev_dos)
  daily_counts <-as.data.frame(df %>% group_by(CARRIER, CLM_ID, DT_OF_SERV) %>% 
                                 summarise(nBill = n_distinct(BILL_ID), nProv = n_distinct(MED_PRVDR), nZip = n_distinct(PRVDR_ZIP_CD)))
  df<-as.data.frame(inner_join(df, daily_counts, by=c('CARRIER','CLM_ID','DT_OF_SERV')))
  
  #get miles from base feature
  #first get zip "base"
  first10<-by(df, df$CLM_ID, head, n=5)
  first10<-ldply(first10, data.frame)
  mode1<-aggregate(PRVDR_ZIP_CD ~ CLM_ID, first10, Mode)
  df<-merge(df, mode1, by = "CLM_ID")
  colnames(df)[54]<-"clmnt_zip_mode"
  
  #get coordinates and miles bt base and service
  coords1<-lapply(as.character(df$PRVDR_ZIP_CD.x), FUN = get_coords)
  coords1<-do.call("rbind", coords1)
  coords2<-lapply(as.character(df$clmnt_zip_mode), FUN = get_coords)
  coords2<-do.call("rbind", coords2)
  df<-cbind(df, coords1, coords2)
  names(df)[55:58]<-c("svc_long","svc_lat","base_long","base_lat")
  miles_from_base<-round(mapply(dMiles, df$svc_long, df$svc_lat, df$base_long, df$base_lat),2)
  df<-cbind(df, miles_from_base)
  
  #output features - if cluster = TRUE, only output continuous variables and row_id
  #may want to create addtl categorical and binary features later
  if(cluster==TRUE){
    #remove rows NA (first claim for a patient has NA prev DOS)
    df<-df[,c(42,47:53,59)]
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


q<-get_features(rdat_small1, cluster = TRUE)
qsub<-q[,c(1,5:9)]

##LOF on small cluster
#remove ID
lof_dat<-qsub[-1]
#REMINDER: write a function to loop over multiple values of k later
outlier.scores <- lofactor(lof_dat, k=10)

#make df of row ID - LOF score
lof_score_df<-cbind(qsub[1], outlier.scores)

#in 10K, 72 have NA scored -- investigate!
#in the meantime, remove NA scores 
#only get back 700/10K rows
rm_na_lof_score_df<-lof_score_df[is.finite(lof_score_df$outlier.scores),]
rm_na_lof_score_df<-na.omit(lof_score_df)
plot(density(rm_na_lof_score_df[,2]))

#find score over 4 (arbitrary - just look for higher scores)
v<-rm_na_lof_score_df[rm_na_lof_score_df$outlier.scores >4,]
v<-rm_na_lof_score_df[rm_na_lof_score_df$outlier.scores >3 & rm_na_lof_score_df$outlier.scores < 4,]
head(v[order(-v$outlier.scores),],10)

#now look at the alleged outliers in the data to see if they make sense
qsub[qsub$row_id=='1296429',]
rdat_small1[rdat_small1$row_id=='1296429',] #good example

qsub[qsub$row_id=='807500',]
rdat_small1[rdat_small1$row_id=='807500',] #note this is for surgery and only 20 miles away 

qsub[qsub$row_id=='465260',]
rdat_small1[rdat_small1$row_id=='465260',] #99199 again

qsub[qsub$row_id=='1830380',]
rdat_small1[rdat_small1$row_id=='1830380',] #maybe good example 

#check that patient's claims
q_all<-get_features(rdat_small1, cluster = FALSE)
clmnt_dat<-q_all[q_all$CLM_ID=='1893933',] 

plot_ly(clmnt_dat, x = ~DT_OF_SERV, y = ~miles_from_base, type = "scatter",
        mode = "lines+markers", text = ~paste("Proc: ",PROC_CD_DER, '<br>Prov ID: ', MED_PRVDR, 'Prov Zip: ', PRVDR_ZIP_CD.x,
                                              '<br>Prov Tax: ', tax_desc, '<br>Clm ID: ', CLM_ID, '<br>Row ID: ', row_id))
