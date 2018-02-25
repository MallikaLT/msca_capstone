create table prov_st_features_prov_dos_metrics as

select carrier
	, prov_id
	, svc_dt_der
	, count(distinct prov_st_der) as prov_state_count
	, concat(carrier, prov_id, svc_dt_der) as row_id
	from mdc_all_data
	group by carrier
		, prov_id
		, svc_dt_der