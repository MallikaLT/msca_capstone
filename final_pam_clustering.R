library(cluster)
library(data.table)

model_dat<-read.csv("model_features_il.csv")

#need sample for memory reasons
set.seed(18)
claim_ids <- sample(unique(model_dat$CLM_ID), 8000)
model_dat<-model_dat[model_dat$CLM_ID %in% claim_ids,]
print(paste0("Rows in subsample: ", dim(model_dat)[1]))

#clean up data and add label
model_dat$PRVDR_TAXNMY_GRP_CD_AES_RPT<-as.factor(model_dat$PRVDR_TAXNMY_GRP_CD_AES_RPT)
model_dat$PROC_GRP_CD_DER<-as.factor(model_dat$PROC_GRP_CD_DER)
model_dat$er_visit<-as.factor(model_dat$er_visit)
model_dat$compl_svc<-as.factor(model_dat$compl_svc)
model_dat$sev_inj<-as.factor(model_dat$sev_inj)

#continue clean-up and get rid of NAs
#change days from previous NA value to 0 (first day per claimant)
model_dat$ds_from_prev<-ifelse(is.na(model_dat$ds_from_prev),0,model_dat$ds_from_prev)
#change diff provider NA value to 0
model_dat$diff_prov<-ifelse(is.na(model_dat$diff_prov),0,model_dat$diff_prov)
#for claimant zip base which don't have any hospitals, make sqMi_per_hosp = 0 instead of NA
model_dat$sqMi_per_hosp<-ifelse(is.na(model_dat$sqMi_per_hosp),0,model_dat$sqMi_per_hosp)

#get subset of data for model
model_dat2<-model_dat[model_dat$clmnt_miles_from_base > 0, c("row_id","ds_from_prev","PROC_GRP_CD_DER", 
                                                   "sqMi_per_hosp","svc2acc","er_visit", "compl_svc",
                                                   "sev_inj","nZip","diff_prov","avg_daily_miles_from_prev",
                                                   "PRVDR_TAXNMY_GRP_CD_AES_RPT", "clmnt_miles_from_base",
                                                   "mfb_norm")]

print(paste0("Rows in subsample where mfb > 0: ", dim(model_dat2)[1]))

ids<-model_dat2[,1] #IDs for training set 
model_dat2<-model_dat2[,-1] #remove Row ID

daisy.mat <- as.matrix(daisy(model_dat2, metric="gower"))

#fit model with K derived from first file
pam_fit <- pam(daisy.mat, diss = TRUE, k = 11)

print("Medoids:")
pam_fit$medoids

final_dat<-cbind(ids,model_dat2,cluster_id = pam_fit$clustering)

write.csv(final_dat, file = "pam_cluster_res.csv")

