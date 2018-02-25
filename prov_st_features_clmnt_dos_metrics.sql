create table prov_st_features_clmnt_dos_metrics as

select carrier
	, clm_id
	, svc_dt_der
	, count(distinct prov_st_der) as pt_state_count
	, concat(carrier, clm_id, svc_dt_der) as row_id
	from mdc_all_data
	group by carrier
		, clm_id
		, svc_dt_der