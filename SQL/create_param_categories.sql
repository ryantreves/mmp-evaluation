/*
This script creates a table of parameter <-> parameter category links based on hand-coded parameters, as well as 
other parameters with the same pollutant codes as our hand-coded parameters.
Author: Ryan Treves
Prerequisites: 
    - Connection to our Postgres DB configured
    - Write privileges in the `scratch` schema
*/

DROP TABLE IF exists scratch.param_categories cascade;
CREATE TABLE  scratch.param_categories
    AS
WITH hand_coded AS (
    SELECT *,
          (CASE WHEN parameter_desc IN (
            'Ammonia [as N] + unionized ammonia',
            'Ammonia & ammonium- total',
            'Ammonia nitrogen, total, [as N] 30 day',
            'Ammonia, unionized',
            'Nitrogen, ammonia [NH3-N]',
            'Nitrogen, ammonia [NH3-N], [sludge]',
            'Nitrogen, ammonia dissolved',
            'Nitrogen, ammonia in bottom deposits',
            'Nitrogen, ammonia per cfs of streamflow',
            'Nitrogen, ammonia total [as N]',
            'Nitrogen, ammonia total [as N] [per discharge]',
            'Nitrogen, ammonia total [as N] [per season]',
            'Nitrogen, ammonia total [as NH4]',
            'Nitrogen, ammonia, percent removal',
            'Nitrogen, ammonia, sludge solid',
            'Nitrogen, ammonia, sludge, tot dry wgt',
            'Nitrogen, ammonia, tot unionized [as N]',
            'Nitrogen, ammonia, total [as NH3]',
            'Nitrogen, AmmoniaTotal',
            'Nitrogen, Ammonia  Total'
          ) THEN 'Ammonia'
          WHEN parameter_desc IN (
            'Cyanide, total [as CN]',
            'Nitrite + Nitrate total [as N]',
            'Nitrogen, nitrate total [as N]',
            'Nitrogen, nitrite total [as N]',
            'Nitrogen, total [as N]',
            'Nitrogen, Kjeldahl, total [as N]',
            'Nitrogen, organic total [as N]',
            'Nitrogen, inorganic total'
          ) THEN 'Other Nitrogen'
          WHEN parameter_desc IN (
            'Pathogens',
            'Coliform, fecal - % samples exceeding limit',
            'Coliform, fecal 10/mL',
            'Coliform, fecal general',
            'Coliform, fecal MF, MFC agar, 44.5 C, 24hr',
            'Coliform, fecal MF, MFC broth, 44.5 C',
            'Coliform, fecal MF, MFC, 0.7um',
            'Coliform, fecal MPN + membrane ftl 44.5 C',
            'Coliform, fecal, colony forming units',
            'Coliform, tot, MPN, completed, [100 mL]',
            'Coliform, total - % samples exceeding limit',
            'Coliform, total 10/mL',
            'Coliform, total colony forming units',
            'Coliform, total general',
            'Coliform, total MF, delayed, m-endo med',
            'Coliform, total MF, immed, les endo agar',
            'Coliform, total MF, immed, m-endo med 35 C',
            'Fecal coliform',
            'Fecal coliform in sludge, CFU/gram',
            'Fecal coliform, MPN, EC med, 44.5 C',
            'Sanitary waste, fecal coliform',
            'Enterococci',
            'Enterococci - % of upper confidence limit',
            'Enterococci - % samples exceeding limit',
            'Enterococci: group D, MF trans, M-E, EIA',
            'E. coli',
            'E. coli exceedances',
            'E. coli, colony forming units [CFU]',
            'E. coli, MTEC-MF',
            'E. coli, thermotol, MF, MTEC'
          ) THEN 'Pathogens'
          WHEN parameter_desc IN (
            'Solids, total suspended',
            'Solids, settleable',
            'Solids, suspended percent removal',
            'Solids, total dissolved'
          )
          THEN 'Solids'
          WHEN parameter_desc = 'pH' THEN 'pH'
          WHEN parameter_desc IN (
            'BOD, carbonaceous [5 day, 20 C]', 
            'BOD, 5-day, 20 deg. C',
            'BOD, 5-day, percent removal'
          ) THEN 'BOD'
          WHEN parameter_desc = 'Chlorine, total residual' THEN 'TRC'
          WHEN parameter_desc IN ('Oxygen, dissolved [DO]', 'DO') THEN 'DO'
          WHEN parameter_desc IN ('Flow, in conduit or thru treatment plant', 
          'Flow rate', 'Flow, maximum during 24 hr period') THEN 'Flow'
          WHEN parameter_desc = 'Oil & Grease' THEN 'Oil & Grease'
          WHEN parameter_desc IN (
            'Copper, total recoverable',
            'Mercury, total recoverable',
            'Lead, total recoverable',
            'Zinc, total recoverable',
            'Nickel, total recoverable',
            'Cadmium, total recoverable',
            'Silver total recoverable',
            'Arsenic, total recoverable',
            'Chromium, hexavalent [as Cr]',
            'Iron, total recoverable',
            'Chromium, trivalent [as Cr]'
          ) THEN 'Metals'
          WHEN parameter_desc = 'Phosphorus, total [as P]' THEN 'Phosphorus'
          ELSE 'Other'
          END) AS parameter_category
    FROM ontologies.icis__ref_parameter
), pol_codes AS (
    -- find other parameters with the same pollutant codes...
    -- checked that this is 1-to-many so max() over identical values
    SELECT pollutant_code, max(parameter_category) AS parameter_category
    FROM hand_coded
    WHERE parameter_category != 'Other'
    GROUP BY 1
), new_params AS (
    SELECT t.parameter_code, coalesce(p.parameter_category, t.parameter_category) AS parameter_category
    FROM hand_coded t
    LEFT JOIN pol_codes p using(pollutant_code)
)
SELECT p.parameter_code, p.parameter_desc, p.pollutant_code,
coalesce(n.parameter_category, 'Other') AS parameter_category 
FROM ontologies.icis__ref_parameter p LEFT JOIN new_params n using(parameter_code)
;
ALTER TABLE scratch.param_categories ADD PRIMARY KEY(parameter_code)
;
COMMIT ;

SELECT * FROM scratch.param_categories LIMIT 100;
