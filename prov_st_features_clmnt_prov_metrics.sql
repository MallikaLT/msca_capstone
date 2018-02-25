Create table prov_st_features_clmnt_prov_metrics as

	select carrier
		, clm_id
		, prov_id
		, count(row_id) as pt_prov_svc_count
		, count(distinct prov_st_der) as pt_prov_st_count
		, concat(carrier, prov_id, clm_id) as row_id
	from mdc_all_data 
	group by carrier
		, clm_id
		, prov_id


