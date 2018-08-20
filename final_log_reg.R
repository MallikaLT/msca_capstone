library(MLmetrics)
library(DMwR)
library(caret)
library(dplyr)

model_prep<-function(df, rm_nas = TRUE, discrete = TRUE){
  df$label <- as.factor(ifelse(df$clmnt_miles_from_base > 0,1,0))
  df<-df[,c("label","row_id","ds_from_prev","PROC_GRP_CD_DER", "sqMi_per_hosp","svc2acc","er_visit", "compl_svc","sev_inj","nZip","diff_prov","avg_daily_miles_from_prev","PRVDR_TAXNMY_GRP_CD_AES_RPT")]
  if(rm_nas == TRUE){
    df$ds_from_prev[is.na(df$ds_from_prev)]<-0
    df$avg_daily_miles_from_prev[is.na(df$avg_daily_miles_from_prev)]<-0
    df$diff_prov[is.na(df$diff_prov)]<-0
    df<-df[!is.na(df$sqMi_per_hosp),]
  }
  #turn binary and categorical back into factors (they lose it upon reading data)
  df$er_visit<-as.factor(df$er_visit)
  df$compl_svc<-as.factor(df$compl_svc)
  df$sev_inj<-as.factor(df$sev_inj)
  df$PROC_GRP_CD_DER<-as.factor(df$PROC_GRP_CD_DER)
  df$PRVDR_TAXNMY_GRP_CD_AES_RPT<-as.factor(df$PRVDR_TAXNMY_GRP_CD_AES_RPT)
  df$diff_prov<-as.factor(df$diff_prov)
  df$nZip<-as.factor(df$nZip)
  if(discrete == TRUE){
     df$bin_ds_from_prev<-as.factor(ntile(df$ds_from_prev, n = 5))
     df$bin_sqMi_per_hosp<-as.factor(ntile(df$sqMi_per_hosp, n = 5))
     df$bin_svc2acc<-as.factor(ntile(df$svc2acc, n = 5))
     df$bin_avg_daily_miles_from_prev<-as.factor(ntile(df$avg_daily_miles_from_prev, n=5))
     df<-df[,c("label","row_id","bin_ds_from_prev","PROC_GRP_CD_DER", "bin_sqMi_per_hosp","bin_svc2acc","er_visit", "compl_svc","sev_inj","nZip","diff_prov","bin_avg_daily_miles_from_prev","PRVDR_TAXNMY_GRP_CD_AES_RPT")]
  }  
  return(df)
}

#function to get eval metrics for models
eval_model<-function(ypred, ytest){
  acc<-Accuracy(ytest,ypred)
  F1<-F1_Score(ytest,ypred)
  auc<-AUC(ypred,ytest)
  return(cbind(acc,F1,auc))
}

il_dat<-read.csv("model_features_il.csv")

#small sample for debugging
#il_dat<-il_dat[1:10000,]

data<-model_prep(il_dat)
str(data)
set.seed(1000)
train_ind<-sample(seq_len(nrow(data)), size = floor(0.7*nrow(data)))

train<-data[train_ind,]
test<-data[-train_ind,]
print(paste0("Dim of orig Train incl. label and ID: ", dim(train)))
print(paste0("Dim of orig Test incl. label and ID: ", dim(test)))

train_ids<-train[,"row_id"] #IDs for training set 
train<-train[,-2] #remove Row ID - function puts label first

#train model
ctrl<-trainControl(method = "cv", number = 5) #not enough memory for 10
model_lr<- caret::train(label ~ ., data = train, method="glm", family="binomial", trControl = ctrl)
#model_lr<- caret::train(label ~ ., data = train, preProcess = c("center","scale"), method="glm", family="binomial", trControl = ctrl)
#model_lr<- caret::train(label ~.+ avg_daily_miles_from_prev*nZip + avg_daily_miles_from_prev*diff_prov + nZip*diff_prov + svc2acc*er_visit, data = train, preProcess = c("center","scale"), method="glm", family="binomial", trControl = ctrl)

summary(model_lr)

#predict 
#lr_pred<-ifelse(predict(model_lr, newdata=test, type="prob") < 0.5, 0,1)
lr_pred<-predict(model_lr, newdata=test, type="prob")

head(lr_pred)
dim(lr_pred)

#LR_PRED CONTAINS 2 cols of probs 0/1
lr_pred_class<-ifelse(lr_pred[,1]>0.5,0,1)
length(lr_pred_class)

#evaluate
ytest<-test[,1]
dim(ytest)
head(ytest)

lr_eval<-eval_model(lr_pred_class, ytest)

lr_eval

ConfusionMatrix(lr_pred_class, ytest)

############################################
#now predict on everything 

all_label<-data[,1]
all_ids<-data[,2]

lr_pred_all<-predict(model_lr, newdata=data[,-c(1,2)], type="prob")
head(lr_pred_all)

#output preds
out<-data.frame(all_ids, all_label, lr_pred_all[,2])
write.csv(out, "logReg_pred_output.csv")