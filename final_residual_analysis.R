dat<-read.csv("logReg_pred_output.csv")

head(dat)

dat<-dat[,-1]

#dat<-as.data.frame(out)
colnames(dat)<-c("ids","label","pred_oob")
dat$label<-ifelse(dat$label=='0',as.numeric(0),as.numeric(1))
dat$abs_resid<-abs(dat$label-dat$pred_oob)
dat$oob_resid<-dat$label-dat$pred_oob

dim(dat)
head(dat)



#now define bins
for(i in 1:9){
  compare_val<-as.numeric(paste0("0.",i))
  new_col<-ifelse(dat$abs_resid > compare_val,1,0)
  dat<-cbind(dat,new_col)
  colnames(dat)[ncol(dat)]<-as.character(paste0("absErr_lt_0.",i))
}


#for chart only 
oob_dat<-dat[dat$label==1,]
in_base_dat<-dat[dat$label==0,]

col_sums_oob<-colSums(oob_dat[,6:ncol(oob_dat)])
dim(oob_dat)

col_sums_in_base<-colSums(in_base_dat[,6:ncol(in_base_dat)])
dim(in_base_dat)

out<-cbind(oob = col_sums_oob, in_base = col_sums_in_base)
out

write.csv(out, "resid_by_level.csv")

