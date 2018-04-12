#prepare for clustering 
set.seed(1983)
rdat<-read.csv("test_ri_data.csv")
ids <- sample(unique(rdat$CLM_ID), 1000)
rdat_sample<-rdat[rdat$CLM_ID %in% ids,]
p<-prepare_data(rdat_sample)
p2<-get_features(p, cluster = TRUE)

#LOF on rows which are not at the base zip 
p2<-p2[p2$clmnt_miles_from_base > 0,]
p2_scaled<-scale(p2[-1])

##LOF on small cluster
#remove ID
lof_dat<-p2_scaled
#REMINDER: write a function to loop over multiple values of k later
outlier.scores <- lofactor(lof_dat, k=10)

#make df of row ID - LOF score
lof_score_df<-cbind(p2[1], outlier.scores)

#could remove NAs - these have lots of neighbors at same location - denisty is undefined 
#must remove NAs to plot 
rm_na_lof_score_df<-na.omit(lof_score_df)
plot(density(rm_na_lof_score_df[,2]))

#find score over 4 (arbitrary - just look for higher scores)
#LOF score of infinity is most certainly an anomaly 
v<-rm_na_lof_score_df[rm_na_lof_score_df$outlier.scores >2,]
#v<-rm_na_lof_score_df[rm_na_lof_score_df$outlier.scores >3 & rm_na_lof_score_df$outlier.scores < 4,]
head(v[order(-v$outlier.scores),],10)

#now look at the alleged outliers in the data to see if they make sense
p2[p2$row_id=='4936',]
p[p$row_id=='4936',] #good example

p2[p2$row_id=='4905',]
p[p$row_id=='4905',] #note this is for surgery and only 20 miles away 

p2[p2$row_id=='4696',] #anomalous claimant in pattern of visits -- shifted base
p[p$row_id=='4696',]

#check that patient's claims
p_all<-get_features(p, cluster = FALSE)
clmnt_dat<-p_all[p_all$CLM_ID=='1495478',] 

plot_ly(clmnt_dat, x = ~DT_OF_SERV, y = ~clmnt_miles_from_base, type = "scatter",
        mode = "lines+markers", text = ~paste("Proc: ",PROC_CD_DER, '<br>Prov ID: ', MED_PRVDR, 'Prov Zip: ', PRVDR_ZIP_CD,
                                              '<br>ProvTax: ', t_desc, '<br>Clm ID: ', CLM_ID, '<br>Row ID: ', row_id))

#############################
#LOF on rows where there is > 1 Zip on that day 
p3<-p2[p2$nZip>1,]
p3_scaled<-scale(p3[-1])
lof_dat_p3<-p3_scaled

outlier.scores.p3 <- lofactor(lof_dat_p3, k=10)
lof_score_df_p3<-cbind(p3[1], outlier.scores.p3)
rm_na_lof_score_df_p3<-na.omit(lof_score_df_p3)
plot(density(rm_na_lof_score_df_p3[,2]))

head(rm_na_lof_score_df_p3[order(-rm_na_lof_score_df_p3$outlier.scores.p3),],10)

#sanity check of highest scores
#they don't make sense 
p3[p3$row_id=='5890',]
p[p$row_id=='5890',]
p[p$CLM_ID=='22210790' & p$DT_OF_SERV=='2013-09-17',]

p3[p3$row_id=='6263',]
p[p$row_id=='6263',]
p[p$CLM_ID=='23720318' & p$DT_OF_SERV=='2012-08-11',]

#look at chart 
p_all<-get_features(p, cluster = FALSE)
clmnt_dat<-p_all[p_all$CLM_ID=='22210790',] 
plot_ly(clmnt_dat, x = ~DT_OF_SERV, y = ~clmnt_miles_from_base, type = "scatter",
        mode = "lines+markers", text = ~paste("Proc: ",PROC_CD_DER, '<br>Prov ID: ', MED_PRVDR, 'Prov Zip: ', PRVDR_ZIP_CD,
                                              '<br>ProvTax: ', t_desc, '<br>Clm ID: ', CLM_ID, '<br>Row ID: ', row_id))

