


-- NPDES Permit IDs for regions 2,3,4,5 from Ryan's script (ideally should be able to just use geocodes above, but this does capture ~30 that are missed by the geo join)
-- HOWEVER, this also misses a ton, for instance half of the POTWs permits in these regions aren't included in this extract (some may be historical permits, but several are definitely current)
DROP TABLE IF EXISTS tmp_region_permits CASCADE;
CREATE LOCAL TEMPORARY TABLE tmp_region_permits
    ON COMMIT PRESERVE ROWS
    AS
    SELECT unnest(
    ARRAY['CA0053287','CA0054861','CA0082511','CA0001139','CA0001201','CA0003786','CA0059013','CA0059731','CA0062405','CA0062286',
      'CA0063291','CA0082821','CA0058297','CA0083861','CA0004111','CA0063363','CA0080900','CA0061051','CA0038474','CA0064351','CA0038059','CA0006751',
      'CA0038491','CA0030171','CA0062375','CA0038768','CA0063886','CA0059153','CA0000451','CA0003778','CA0002020','CA0000680','HH0049646','CA0085201',
      'CA0083984','CA0063045','CA0060259','CA0049018','CA0061603','CA0028282','CA0079197','CA0047830','CA0064009','CA0000868','CA0082830','CA0059285',
      'CA0000442','CA0058343','CA0059561','CA0060232','CA0083895','CA0057690','CA0081213','CA0085146','CA0038091','CA0038466','CA0027791','CA0078867',
      'CA0060755','CA0062189','CA0030058','CA0057720','CA0082783','CA0029947','CA0049352','CA0049361','CA0038873','CA0083542','CA0005240','CA0083640',
      'CA0029980','CA0059188','CA0058696','CA0063983','CA0054267','CA0078093','CA0082546','CA0047856','CA0004791','CA0030201','CA0078581','CA0078875',
      'CA0085332','CA0048267','CA0085278','CA0084620','CA0083411','CA0083968','CA0057371','CA0082082','CA0054496','CA0058874','CA0002992','CA0059064',
      'CA0079758','CA0037966','CA0064521','CA0081566','CA0049921','CA0059501','CA0053848','CA0028703','CA0028690','CA0061704','CA0049972','CA0037869',
      'CA0038849','CA0029149','CA0030104','CA3000001','CA0030180','CA0084531','CA0059315','CA0048682','CA0048739','CA0057177','CA0060615','CA0037648',
      'CA0054127','CA0001899','CA0049212','CA0058998','CA0064131','CA0063975','CA0001465','CA0083429','CA0000337','CA0005134','CA0049832','CA0078352',
      'CA0082295','CA0080853','CA0078280','CA0059650','CA0059293','CA0060976','CA0000469','CA0061182','CA0055361','CA0029971','CA0058009','CA3000002',
      'CA0038776','CA0037494','CA0060763','CA0079529','CA0081531','CA0064319','CA0078999','CA0083283','CA0062367','CA0061514','CA0059846','CA0063185',
      'CA0059242','CAS000001','CA0037885','CA0028207','CA0081841','CA0059676','CA0038784','CA0060577','CA0029904','CA0059935','CA0064505','CA0002941',
      'CA0004928','CA0083291','CA0004090','CA0038547','CA0062022','CA0061336','CA0053091','CA0001414','CA0061646','CA0004910','CA0037613','CA0038679',
      'CA0081558','CA0006254','CA0004936','CA0038440','CA0037702','CA0038342','CA0038636','CA0006165','CA0057363','CA0057932','CA0083305','CA0084794',
      'CA0001147','CA0062537','CA0061808','CA0064050','CA0038792','CA0083933','CA0084883','CA0049221','CA0058971','CA0003689','CA0064301','CA0063304',
      'CA0055387','CA0062928','CA0062910','CA0062723','CA0084344','CA0060631','CA0028185','CA0038024','CA0057886','CA0059838','CA0061948','CA0084816',
      'CA0004057','CA0058114','CA0057827','CA0050628','CA0084093','CA0082571','CA0058823','CA0082309','CA0083950','CA0029122','CA0029106','CA0056413',
      'CA0004847','CA0004863','CA0083739','CA0059617','CA0063428','CA0059951','CA0061174','CA0062669','CA0005274','CA0008069','CA0030147','CA0030139',
      'CA0038351','CA0060003','CA0060267','CA0001333','CA0030155','CA0059552','CA0038580','CA0048941','CA0004146','CA0047872','CA0049441','CA0063142',
      'CA0064211','CA0064467','CA0062162','CA0058688','CA0079227','CA0085197','CA0028100','CA0027961','CA0083941','CA0063321','CA0058211','CA0079391',
      'CA0084085','CA0049859','CA0059455','CA0002798','CA0053813','CA0064661','CA0053911','CA0053619','CA0054011','CA0054119','CA0053716','CA0083666',
      'CA0000892','CA0003344','CA0006289','CA0082201','CA0057126','CA0063509','CA0084701','CA0084719','CA0001911','CA0055816','CA0056863','CA0060178',
      'CA0084387','CA0030112','CA0081256','CA0062171','CA0056707','CA0059803','CA0064491','CA0061867','CA0081396','CA0082651','CA0037851','CA0085359',
      'CA0030210','CA0064068','CA0084255','CA0079651','CA0038008','CA0001996','CA0048127','CA0059544','CA0064530','CA0001171','CA0056120','CA0053856',
      'CA0058432','CA0064149','CA0056995','CA0057673','CA0057665','CA0057649','CA0000370','CA0057568','CA0056448','CA0056383','CA0000361','CA0057037',
      'CA0056855','CA0055824','CA0056545','CA0062341','CA0059099','CA0064670','CA0062766','CA0062561','CA0064254','CA0059714','CA0064092','CA0064106',
      'CA0064513','CA0059919','CA0061913','CA0064203','CA0002739','CA0003352','CA0062065','CA0003069','CA0064084','CA0061085','CA0037427','CA0038814',
      'CA0038148','CA0005789','CA0085243','CA0057746','CA0057070','CA0049654','CA0061581','CA0083054','CA0085316','CA0083801','CA0063908','CA0047899',
      'CA0063380','CA0003743','CA0005185','CA0060992','CA0061476','CA0007005','CA0038563','CA0062847','CA0037770','CA0081922','CA0060704','CA0064602',
      'CA0038644','CA0037575','CA0028631','CA0028835','CA0064122','CA0029581','CA0079901','CA0064556','CA0037737','CA0063924','CA0059226','CA0064114',
      'CA0063916','CA0001104','CA0056928','CA0037958','CA0082384','CA0049590','CA0061140','CA0038512','CA0050016','CA0085111','CA0047791','CA0077836',
      'CA0083607','CA0053163','CA0081809','CA0001180','CA0001198','CA0082287','CA0056464','CA0083852','CA0057274','CA0082961','CA0003751','CA0005649',
      'CA0028240','CA0082627','CA0064190','CA0083496','CA0053571','CA0056065','CA0063355','CA0036285','CA0059871','CA0037810','CA0059358','CA0064611',
      'CA0000051','CA0005053','CA0038504','CA0004839','CA0064688','CA0081248','CA0055638','CA0079332','CA0078786','CA0079367','CA0079316','CA0060798',
      'CA0055786','CA0061123','CA0080071','CA0063894','CA0030007','CA0001848','CA0004316','CA0059480','CA0054615','CA0049417','CA0002658','CA0059595',
      'CA0058726','CA0064297','CA0058629','CA0038539','CA0079588','CA0083771','CA0081574','CA0055964','CA0060607','CA0082708','CA0060623','CA0082414',
      'CA0053392','CA0038598','CA0063215','CA0081787','CA0079111','CA0079961','CA0084549','CA0083658','CA0083569','CA0004758','CA0083593','CA0083585',
      'CA0078794','CA0077682','CA0084948','CA0000884','CA0048364','CA0055409','CA0028070','CA0038318','CA0005321','CA0038610','CA0038857','CA0037681',
      'CA0037672','CA0082848','CA0037842','CA0047902','CA0038881','CA0061042','CA0047961','CA0029238','CA0061638','CA0054313','CA0054216','CAS049883',
      'CA0054101','CA0054224','CA0078859','CA0030228','CA0048909','CA0082376','CA0049051','CA0061328','CA0064289','CA0064637','CA0000809','CA0062685',
      'CA0055263','CA0038750','CA0028541','CA0078841','CA0004219','CA0083011','CA0082431','CA0060887','CA0037800','CA0084018','CA0085286','CA0038130',
      'CA0061191','CA0063002','CA0079545','CA0058556','CA0000981','CA0048976','CA0038016','CA0064025','CA0083224','CA0053112','CA0038482','CA0063070',
      'CA0030236','CA0082252','CA0082643','CA0048054','CA0037621','CA0001821','CA0081094','CA0064599','CA0084191','CA0080845','CA0004961','CA0063011',
      'CA0062839','CA0084891','CA0001309','CA0085049','CA0063100','CA0030198','CA0053490','CA0059382','CA0004880','CA0063851','CA0064157','CA0063541',
      'CA0056359','CA0001813','CA0063495','CA0053503','CA0060062','CA0064343','CA0064360','CA0079154','CA0110116','CA0084263','CA0060330','CA0083551',
      'CA0083691','CA0084000','CA0084727','CA0078948','CA0082937','CA0077895','CA0083712','CA0084182','CA0083267','CA0082775','CA0083992','CA0084743',
      'CA0030031','CA0083330','CA0000787','CA0083364','CA0081931','CA0003905','CA0083917','CA0082368','CA0084905','CA0005002','CA0055719','CA0005720',
      'CA0082841','CA0038733','CA0063134','CA0063461','CA0056651','CA0054453','CA0061956','CA0061441','CA0049450','CA0061425','CA0049123','CA0002305',
      'CA0029297','CA0064696','CA0005550','CA0084808','CA0081311','CA0059005','CA0000761','CA0064165','CA0055247','CA0063177','CA0061000','CA0059510',
      'CA0061387','CA0062031','CA0064238','CA0081833','CA0064581','CA0063401','CA0064246','CA0079171','CA0084484','CA0001902','CA0053074','CA0002186',
      'CA0079979','CA0059927','CA0049743','CA0078034','CA0059391','CA0056529','CA0083119','CA0038121','CA0082422','CA0006157','CA0052949'])::VARCHAR
      AS npdes_permit_id
;
ALTER TABLE tmp_region_permits ADD PRIMARY KEY(npdes_permit_id);


-- Hand-checking the 12 POTWs that fail to join via CIWQs (and then based on county/city for the couple where that fails)


-- Combined set of NPDES permits from both methods
-- Note the spatial join here on the lat/lon in facilities
DROP TABLE IF EXISTS tmp_treated_ca_permits CASCADE;
CREATE LOCAL TEMPORARY TABLE tmp_treated_ca_permits
    ON COMMIT PRESERVE ROWS
    AS
    -- All individual permittees in CA
WITH perms AS (
    SELECT distinct npdes_permit_id
    FROM icis.permits
    WHERE permit_state = 'CA' AND individual_permit_flag = 1
)
    -- Join in lat/longs for each permittee
, perm_geos AS (
    SELECT npdes_permit_id, ST_SetSRID(ST_MakePoint(geocode_longitude, geocode_latitude), 4326) AS point
    FROM icis.facilities f
    JOIN perms p USING(npdes_permit_id)
)
    -- Join in waterboard region boundaries based on permittee location within region geometry
, all_perm_regions AS (
    SELECT p.npdes_permit_id, 
        COALESCE(r.region, r2.region) AS region, 
        COALESCE(r.region_num, h.region_num) AS region_num, 
        f.city, f.zip, f.county_code, f.geocode_latitude, f.geocode_longitude, f.facility_name
    FROM perm_geos p
    LEFT JOIN kit_sandbox.ca_waterboard_regions r
    ON ST_Contains(r.geometry, p.point)
    LEFT JOIN icis.facilities f using(npdes_permit_id)
    LEFT JOIN kit_sandbox.ca_potw_hand_geocode h using(npdes_permit_id)
    LEFT JOIN kit_sandbox.ca_waterboard_regions r2 ON h.region_num = r2.region_num
)
    -- Select only permittees in regions 2, 3, 4, and 5 (the treatment regions)
, geocoded_perms AS (
    SELECT DISTINCT npdes_permit_id FROM all_perm_regions WHERE region_num IN (2,3,4,5)
)
    -- Define permittees listed in the treatment regions on CIWQS
, ciwqs_perms AS (
    SELECT DISTINCT npdes_permit_id FROM tmp_region_permits JOIN icis.permits USING(npdes_permit_id) WHERE individual_permit_flag = 1
)
    -- Select the union of permittees listed in the treatment regions on CIWQS 
    -- and permittees we have geocoded into these regions, and save it into 
    -- the temporary table `tmp_treated_ca_permits`
SELECT COALESCE(n.npdes_permit_id, o.npdes_permit_id) AS npdes_permit_id
FROM geocoded_perms n
FULL OUTER JOIN ciwqs_perms o USING(npdes_permit_id)
;
ALTER TABLE tmp_treated_ca_permits ADD PRIMARY KEY(npdes_permit_id);


DROP TABLE IF EXISTS tmp_synth_permits CASCADE;
CREATE LOCAL TEMPORARY TABLE tmp_synth_permits
    ON COMMIT PRESERVE ROWS
    AS
WITH permits_pre AS (
    SELECT npdes_permit_id, facility_type_indicator, 
            row_number() OVER (partition BY npdes_permit_id ORDER BY CASE WHEN facility_type_indicator = 'POTW' THEN 1 ELSE 0 END DESC, RANDOM()) AS rn
    FROM icis.permits p
    LEFT JOIN tmp_treated_ca_permits r using(npdes_permit_id)
    WHERE       
      -- Conditions for a permit to be counted
      p.individual_permit_flag = 1
        -- Remove territories & DC
      AND NOT p.permit_state IN (
        'AS',
        'DC',
        'GE',
        'GU',
        'GM',
        'MP',
        'MW',
        'NN',
        'PR',
        'SR',
        'VI'
      )
      -- Remove CA permittees in non-treatment regions
      AND (r.npdes_permit_id IS NOT NULL OR p.permit_state != 'CA')
)
SELECT * FROM permits_pre WHERE rn = 1;
ALTER TABLE tmp_synth_permits ADD PRIMARY KEY(npdes_permit_id);


-- Create final output data table 
DROP TABLE IF EXISTS scratch.test_synth_data_quarter_geo CASCADE;
CREATE TABLE scratch.test_synth_data_quarter_geo
    AS
    -- Collect DMRs meeting our basic inclusion criteria, assigning them a parameter category
    -- and time period, and calculating exceedance %.
WITH dmr_data AS (
    SELECT 
      d.permit_state,
      p.facility_type_indicator,
      d.npdes_permit_id,
      -- Assign each DMR to a time period
      DATE_TRUNC('year', d.monitoring_period_end_date) AS monitoring_period,
      (CASE WHEN (
        (
          CAST(dmr_value_standard_units AS double precision) - CAST(limit_value_standard_units AS double precision)
        ) * 100 * limit_value_qualifier_factor  > 0) THEN 1 ELSE 0 END) AS any_exceedance
    FROM
      icis.dmrs d
      -- Join in permit data in order to omit general permittees
      -- and non-POTWs
      JOIN tmp_synth_permits p USING(npdes_permit_id)
    WHERE
      -- Conditions for a DMR to be counted:
      d.monitoring_period_end_date <= '2016-12-31'
)
  -- Aggregate facility data over time, winsorizing remaining extreme exceedance %s
, data_by_facility_period AS (
    SELECT
        permit_state,
        npdes_permit_id,
        facility_type_indicator,
        monitoring_period,
        SUM(any_exceedance) AS n_exceedances,
        COUNT(*) AS n_dmrs
    FROM dmr_data
    GROUP BY permit_state, facility_type_indicator, npdes_permit_id, monitoring_period
)

-- Into the final data table, insert data now aggregated over facilities.
SELECT 
  permit_state,
  facility_type_indicator,
  monitoring_period,
  COUNT(DISTINCT npdes_permit_id) AS n_permittees,
  SUM(n_exceedances) AS n_exceedances,
  SUM(n_dmrs) AS n_dmrs
FROM data_by_facility_period
GROUP BY 
  GROUPING SETS(
  (permit_state, facility_type_indicator,monitoring_period),
  (permit_state, monitoring_period)
  )
;
COMMIT
;
SELECT * FROM scratch.test_synth_data_quarter_geo
;