Create table prov_st_features_base as

select mdc.row_id as mdc_row_id
	, mdc.prov_zip3
	, mdc.prov_st_der
	, mdc.jur_st_der
	, concat(mdc.carrier, mdc.prov_id) as carrier_prov
	, concat(mdc.carrier, mdc.clm_id) as carrier_clmnt
	, concat(mdc.carrier, mdc.prov_id, mdc.clm_id) as carrier_prov_clmnt
	, concat(mdc.carrier, mdc.clm_id, mdc.svc_dt_der) as carrier_clmnt_dos
	, concat(mdc.carrier, mdc.prov_id, mdc.svc_dt_der) as carrier_prov_dos
	, case when ((substring(dx1_der,1,5) between 'S06.1' and 'S06.8') or
		(substring(dx2_der,1,5) between 'S06.1' and 'S06.8') or
		(substring(dx1_der,1,3) between 'T20' and 'T28') or
		(substring(dx2_der,1,3) between 'T20' and 'T28') or
		(substring(dx1_der,1,3) in ('S14','S24','S34','I63','C71','G82','G83','S88','S48','S58','S68','S78','S98','S38','S08','S36','S37','T20','T30','T31','T32','S67','S17','S07','S28')) or
		(substring(dx2_der,1,3) in ('S14','S24','S34','I63','C71','G82','G83','S88','S48','S58','S68','S78','S98','S38','S08','S36','S37','T20','T30','T31','T32','S67','S17','S07','S28')) or
		(substring(dx1_der,1,5) = 'Z77.0') or
		(substring(dx2_der,1,5) = 'Z77.0') or
		(substring(dx1_der,1,7) in ('S06.9X5','S06.9X6','S06.9X7','S06.9X8')) or
		(substring(dx2_der,1,7) in ('S06.9X5','S06.9X6','S06.9X7','S06.9X8')) or
		dx1_der in ('N28.9','G95.9') or 
		dx2_der in ('N28.9','G95.9')) 
			then 1 else 0 end as severe_injury
	, case when  ((proc_cd_der between '23900' and '23921') or
		(proc_cd_der between '24900' and '24940') or
		(proc_cd_der between '25900' and '25931') or
		(proc_cd_der between '26910' and '26952') or
		(proc_cd_der between '27290' and '27295') or 
		(proc_cd_der between '27590' and '27598') or 
		(proc_cd_der between '27880' and '27889') or
		(proc_cd_der between '28800' and '28825') or
		(proc_cd_der between '63055' and '63066') or
		(proc_cd_der between '63075' and '63091') or
		(proc_cd_der between '22551' and '22558') or 
		(proc_cd_der between '22532' and '22534') or 
		(proc_cd_der between '22590' and '22634') or 
		(proc_cd_der between '63101' and '63103') or 
		(proc_cd_der between '96401' and '96549') or
		(proc_cd_der in ('22633','22630','63047','22548','22585','22586','0207','0208'))) 
			then 1 else 0 end as complex_svc
	, case when proc_cd_grp_der = '04' then 1 else 0 end as pathology_svc
	, case when med_cst_cat_cd='05' then 1 else 0 end as drug_svc
	, case when substring(proc_cd_der,1,1) in ('A','E','K') then 1 else 0 end as dme_svc 
	, case when med_cst_cat_cd='03' then 1 else 0 end as ip_hospital_svc
	, case when proc_cd_grp_der='85' then 1 else 0 end as carrier_specific_proc
	, case when ntwk_cd in ('Y','P','H') then 1 else 0 end as in_carrier_network 
    	, case when (proc_cd_der between '0450' and '0459') or (proc_cd_der between '99281' and '99288') or pos='23' then 1 else 0 end as er_svc
	, datediff(svc_dt_der, acc_dt) as days_svc_to_acc
	, datediff(trans_dt, svc_dt_der) as days_trans_to_svc
	, case when paid_amt > 0 then 1 else 0 end as paid_svc
	, case when acc_dt < pol_eff_dt or 
		svc_dt_der < pol_eff_dt or 
		svc_dt_der < acc_dt or 
		trans_dt < acc_dt or 
		trans_dt < pol_eff_dt or 
		trans_dt < svc_dt_der or
		year(svc_dt_der)>'2017' or
		year(acc_dt)>'2017' or
		year(trans_dt)>'2017'
			then 1 else 0 end as bad_date_data
	, case when bs.border_st_id is not null then 1 else 0 end as border_st_billed
	, case when bs.border_st_id is null and mdc.jur_st_der!=mdc.prov_st_der then 1 else 0 end as out_of_st_billed
	, case when (proc_cd_der between '98966' and '98969') or 
		(proc_cd_der between '99441' and '99449') or 
		proc_cd_der='99495' or
		pos='02'
		    then 1 else 0 end as non_f2f_svc
	, case when trans_cd='03' then 1 else 0 end as replacement_trans 
from mdc_all_data mdc
left join mdc_state_borders bs
		on mdc.jur_st_der = bs.st_id
		and mdc.prov_st_der = bs.border_st_id
where jur_st_der is not null and jur_st_der REGEXP '^[A-Z]+$'
	and prov_st_der is not null and prov_st_der REGEXP '^[A-Z]+$' and prov_st_der not in ('AE','AA','AP','FD','')
	and prov_id is not null
	and clm_id is not null
	and svc_dt_der is not null
	and proc_cd_der is not null
	and proc_cd_grp_der is not null
	and acc_dt is not null
	and ntwk_cd is not null
	and pos is not null
	
