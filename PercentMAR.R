library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(geosphere)
library(DMwR)
library(bfast)
library(plotly)
library(tidyr)
library(pastecs)

rdatAR <- read.csv("MDC_AR_SY11_17_UChi_20171211.CSV")

rdatAK <- read.csv("MDC_AK_SY11_17_UChi_20171211.CSV")

rdatAL <- read.csv("MDC_AL_SY11_17_UChi_20171211.CSV")

rdat<-rbind(rdatAR,rdatAK,rdatAL)



rdat1<-rdat[sample(nrow(rdat), 6000000), ]

head(rdat1)

#Calculates MAR per NCCI, Identifies where unit_count and derived unit_cnt are different, and paid amount per unit

rdat1$MAR <- with(rdat1, WC_FEE_SCHD_AMT/UNIT_CNT_DER)
rdat1$UNITDIF <- with(rdat1,UNIT_CNT-UNIT_CNT_DER)
rdat1$unitcost <- round(with(rdat1,PAID_AMT/UNIT_CNT),2)


#Filters out NA'S and 0's from WC_Fee_Schd_Amt, where there are differences in unit_cnt and derived_unit_cnt (provider typed in 0 for units), and when paid amount is 0
#Once NA' and 0's are filtered out, the only difference between units and derived units is -1, which is when provider typed in 0 and NCCI derived 1. I filtered those records out.
CleanData<-filter(rdat1, WC_FEE_SCHD_AMT >0)
CleanData<-filter(CleanData, UNITDIF == 0)
CleanData<-filter(CleanData, PAID_AMT > 0)

#Calculates % of MAR

CleanData$PercentMAR<-round(with(CleanData,unitcost/MAR),2)

head(CleanData)

#function to identify outliers and plot data with and without outliers
#coef is the % above or below the interquartiles PercentMar has to be for it to be considered an outlier. Default is 1.5.

outlierKD <- function(dt, var, coef, response) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = coef)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

summary(CleanData$PercentMAR)

#Run function with DF, the variable to analyze, the threshold for considering data point an outlier, and Yes or no to remove outliers from data frame.

Outlier<-outlierKD(CleanData,PercentMAR,1.5,"No")

#Outliers are coded as "NA" in output. This converts NA to 0.

Outlier[is.na(Outlier)] <- 0

#Gives a binary value to each record that identifies it as an outlier or not.

CleanData$Outlier<-ifelse(Outlier>0,0,1)

head(CleanData)

write.csv(CleanData, file = "PercentMarData.csv")


