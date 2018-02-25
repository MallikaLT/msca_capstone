
Create table prov_st_features_st_metrics as

SELECT st_id
	, case when border_st_count = 0 then 1 else 0 end as border0
	, case when border_st_count between 1 and 3 then 1 else 0 end as border1
	, case when border_st_count between 4 and 6 then 1 else 0 end as border2
	, case when border_st_count >6 then 1 else 0 end as border3
from (
	select st_id
		, count(distinct border_st_id) as border_st_count
	from mdc_state_borders
	group by st_id
) BorderStCounts;