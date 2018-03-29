library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(geosphere)
library(plotly)

#clear environment 
#rm(list=ls())
#gc()

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

#obtain mode of a vector 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#build a dataset of random sample of patients to look at distance from "base" zip code 
build_sample_data<-function(sample_no, seed_no) {
  set.seed(as.integer(seed_no))
  ids_set <- sample(unique(rdat$CLM_ID), as.integer(sample_no))
  df_sub<-rdat[rdat$CLM_ID %in% ids_set & rdat$PROC_GRP_CD_DER %in% c(1,2,3,5,6,30,45,50,60,75) &
                 !substring(rdat$PROC_CD_DER,1,1) %in% c("A","B","E","L","K") &
                 !rdat$PROC_CD_DER %in% c("S9999","99002", "99070","99080") &
                 (nchar(as.character(rdat$PROC_CD_DER))==5 | 
                    nchar(as.character(rdat$PROC_CD_DER))==4), ]
  df_sub<-df_sub[order(df_sub$CLM_ID, df_sub$DT_OF_SERV),]
  #get mode or zip "base"
  first10<-by(df_sub, df_sub$CLM_ID, head, n=5)
  first10<-ldply(first10, data.frame)
  mode1<-aggregate(PRVDR_ZIP_CD ~ CLM_ID, first10, Mode)
  #join mode to data
  df_sub<-merge(df_sub, mode1, by = "CLM_ID")
  df_sub<-df_sub[,c(42,1,23,24,26,30,28,2,8,10,36,16,44,43)]
  colnames(df_sub)[13]<-"clmnt_zip_mode"
  #get coordinates and miles bt base and service
  coords1<-lapply(as.character(df_sub$PRVDR_ZIP_CD.x), FUN = get_coords)
  coords1<-do.call("rbind", coords1)
  coords2<-lapply(as.character(df_sub$clmnt_zip_mode), FUN = get_coords)
  coords2<-do.call("rbind", coords2)
  df_sub<-cbind(df_sub, coords1, coords2)
  names(df_sub)[15:18]<-c("svc_long","svc_lat","base_long","base_lat")
  miles_from_base<-mapply(dMiles, df_sub$svc_long, df_sub$svc_lat, df_sub$base_long, df_sub$base_lat)
  df_sub<-cbind(df_sub, miles_from_base)
  return(df_sub)
}

get_spike_clmnts<-function(df, mfb){
  spike_clmnts<-df %>% filter(miles_from_base > as.integer(mfb)) %>% group_by(CLM_ID) %>% 
    summarize(nRow = n()) %>% arrange(desc(nRow))
  return(as.data.frame(spike_clmnts))
}

graph_clmnt<-function(data, clmnts, i){
  clm_id<-clmnts[i,1]
  clmnt_data<-data[data$CLM_ID==as.character(clm_id),]
  plot_ly(clmnt_data, x = ~DT_OF_SERV, y = ~miles_from_base, type = "scatter",
          mode = "lines+markers", text = ~paste("Proc: ",PROC_CD_DER, '<br>Prov ID: ', MED_PRVDR, 'Prov Zip: ', PRVDR_ZIP_CD.x,
                                                '<br>Clm ID: ', clm_id, '<br>Tax: ', tax_desc, '<br>Row ID: ', row_id))
}


#read in MDC data 
rdat<-read.csv("test_ri_data.csv")
rdat[,c(2,7,8,11,12,13,30)]<-lapply(rdat[,c(2,7,8,11,12,13,30)],
                                    function(z) as.Date(as.character(z), format = "%m/%d/%Y"))
rdat$PRVDR_TAXNMY_CD<-str_trim(rdat$PRVDR_TAXNMY_CD)
rdat$row_id <- seq.int(nrow(rdat))

#read in taxonomy data 
tax<-read.csv("cms_taxonomies.csv")
#there are multiple row per ID, so getting unique code/desc and taking the first row of the group
tax2<-unique(tax[c("PROVIDER.TAXONOMY.CODE","PROVIDER.TAXONOMY.DESCRIPTION")])
tax2<-ddply(tax2, "PROVIDER.TAXONOMY.CODE", head, 1)
tax2$PROVIDER.TAXONOMY.CODE<-str_trim(tax2$PROVIDER.TAXONOMY.CODE)
rdat <- rdat %>% 
  left_join(select(tax2, "PROVIDER.TAXONOMY.DESCRIPTION", "PROVIDER.TAXONOMY.CODE"),
            by = c("PRVDR_TAXNMY_CD" = "PROVIDER.TAXONOMY.CODE"))
colnames(rdat)[43]<-"tax_desc"

#read in 5 digit zip codes, pad and obtain 3-digit zips and their centroids by averaging 5 digit zips 
zip5<-read.delim("us_census_2013_zips.txt", header=TRUE, sep=",")
zip5$ZIP<-str_pad(zip5$ZIP,5, pad = "0")
zip5$zip3<-substring(zip5$ZIP,1,3)

zip3_centroids<-as.data.frame(zip5 %>%
                                group_by(zip3) %>%
                                summarize(mean_lat = mean(LAT), mean_long = mean(LNG)))


#graph claimants' distance patterns over time 
set1<-build_sample_data(50,098)
pts1<-get_spike_clmnts(set1,20)
head(pts1)

graph_clmnt(set1, pts1, 1) #example of spikes - possible anomaly















