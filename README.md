# msca_capstone
Code for MSCA capstone

For main data load, use the following code to ensure proper date formatting (client data is in DD-MM-YYYY format and needs to be YYYY-MM-DD for MySQL):

mysql -h mysql.rcc.uchicago.edu -u msca_ncci -p msca_ncci --local-infile=1 -e "load data local infile 'MDC_RI_SY11_17_UChi_20171211.csv' INTO TABLE mdc_ri FIELDS TERMINATED BY ',' IGNORE 1 LINES (carrier,@pol_eff_dt,trans_cd,jur_st,gndr_cd,birth_yr,@acc_dt,@trans_dt,bill_id,line_no,@svc_dt,@svc_dt_from,@svc_dt_to,proc_cd1,mod1,mod2,chrg_amt,paid_amt,dx1,dx2,prov_tax,prov_id,prov_zip3,ntwk_cd,units,pos,proc_cd2,clm_id,carrier_grp,@svc_dt_der,dx1_der,dx2_der,ip_ep_no,jur_st_der,med_cst_cat_cd,proc_cd_der,proc_cd_grp_der,prov_st_der,prov_tax_grp,units_der,wc_fee_sched_amt) SET pol_eff_dt = STR_TO_DATE(@pol_eff_dt, '%m/%d/%Y'), acc_dt = STR_TO_DATE(@acc_dt, '%m/%d/%Y'), trans_dt = STR_TO_DATE(@trans_dt, '%m/%d/%Y'), svc_dt = STR_TO_DATE(@svc_dt, '%m/%d/%Y'),svc_dt_from = STR_TO_DATE(@svc_dt_from, '%m/%d/%Y'),svc_dt_to = STR_TO_DATE(@svc_dt_to, '%m/%d/%Y'),svc_dt_der = STR_TO_DATE(@svc_dt_der, '%m/%d/%Y')"
