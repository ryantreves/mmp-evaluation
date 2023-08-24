###############################################################################
# Utility functions for policy evaluation of the MMP program.
#
# Inputs:
#   -
#
# Outputs:
#   - 
#
# Author(s): Maggie Hilderbran, Ryan Treves
#
# TO DO:
#   - How do we define MMP violations? Currently, we're just filtering out
#     non-exempt violations, but how might we identify MMP violations?
#
###############################################################################

import numpy as np
import pandas as pd
import scipy.stats as ss
import statsmodels.stats.proportion as ssp
import yaml
from pathlib import Path

# set constants / environment variables
NPDES_PROGRAMS = ['DODNPDESSW', 'DODNPDESWW', 'NPDESWW', 'NPDINDLRG',
                  'NPDINDSML', 'NPDMINING', 'NPDMUNILRG', 'NPDMUNIOTH',
                  'NPDNONMUNIPRCS']
PRETREATMENT_START_DATE = '2007-01-01'  # we define this to be the beginning of our pretreatment period
TREATMENT_START_DATE = '2008-07-01'  # beginning of our treatment period
TREATMENT_END_DATE = '2009-01-22'  # end of our treatment period
LOW_ENFORCEMENT_RATE_THRESHOLD = 0  # threshold for low enforcement for identifying treatment facilities
HIGH_ENFORCEMENT_RATE_THRESHOLD = 0.75  # threshold for 'consistent enforcement'
# (i.e., min. proportion of violations for which there's enforcement)


# -----------------------------------------------------------------------------

def get_epls(enf, issuance_dates_only=True, valid_only=True):
    if issuance_dates_only:
        # must have ACL and/or EPL issuance date
        epl_enf = enf[
            # Either be categorized as an EPL
            ((enf['ENFORCEMENT ACTION TYPE'] == 'Expedited Payment Letter') |
            # Or be categorized as an ACL but contain references to being an EPL in
            # the title or description
              ((enf['ENFORCEMENT ACTION TYPE'] == 'Admin Civil Liability') &
               ((enf['TITLE'].str.contains('xpedited')) |
                (enf['TITLE'].str.contains('\bEPL')) |
                (enf['DESCRIPTION'].str.contains('xpedited')) |
                (enf['DESCRIPTION'].str.contains('\bEPL'))))) &

            # And have a valid issuance date
            ((~pd.isna(enf['ACL ISSUANCE DATE'])) |
             (~pd.isna(enf['EPL ISSUANCE DATE'])))]
    else:
        # not conditioning on having ACL and/or EPL issuance dates
        epl_enf = enf[
            # Either be categorized as an EPL
            (enf['ENFORCEMENT ACTION TYPE'] == 'Expedited Payment Letter') |
             # Or be categorized as an ACL but contain references to being an EPL in
             # the title or description
            ((enf['ENFORCEMENT ACTION TYPE'] == 'Admin Civil Liability') &
             ((enf['TITLE'].str.contains('xpedited')) |
              (enf['TITLE'].str.contains('\bEPL')) |
              (enf['DESCRIPTION'].str.contains('xpedited')) |
              (enf['DESCRIPTION'].str.contains('\bEPL'))))]
    
    # some actions with 'REPLACE' (or similar) are getting through - we need to exclude those
    #epl_enf = epl_enf[~epl_enf['DESCRIPTION'].str.contains('replac', na=False)]

    # use heuristic to estimate EPL date
    epl_enf['ACL ISSUANCE DATE'] = pd.to_datetime(epl_enf['ACL ISSUANCE DATE'], errors='coerce')
    epl_enf['EPL ISSUANCE DATE'] = pd.to_datetime(epl_enf['EPL ISSUANCE DATE'], errors='coerce')
    epl_enf['estimated_epl_date'] = epl_enf[['EPL ISSUANCE DATE', 'ACL ISSUANCE DATE']].min(axis=1, skipna=True)
    if not issuance_dates_only:
        # take effective date in cases where ACL/EPL issuance dates is (are) missing
        epl_enf['estimated_epl_date'].fillna(epl_enf['EFFECTIVE DATE.1'], inplace=True)
    if valid_only:
        # drop EPLs with a status of 'Withdrawn' or 'Draft'
        epl_enf = epl_enf[epl_enf['STATUS.1'].apply(lambda x: x not in list(['Withdrawn', 'Draft']))]
    return epl_enf


def clean_enforcements(enf, mmp_only=True):
    enf_clean = enf
    # keep only enforcements for NPDES permittees
    #enf_clean = enf_clean[enf_clean['PROGRAM'].apply(lambda x: x in NPDES_PROGRAMS)]
    enf_clean = enf_clean[
        # option 1: both fields contain NPDES programs
                          ( (enf_clean['PROGRAM'].isin(NPDES_PROGRAMS)) & (enf_clean['PROGRAM.1'].isin(NPDES_PROGRAMS)) ) |
        # option 2: one is empty and the other contains a NPDES program
                          ( (pd.isna(enf_clean['PROGRAM'])) & (enf_clean['PROGRAM.1'].isin(NPDES_PROGRAMS)) ) |
                          ( (enf_clean['PROGRAM'].isin(NPDES_PROGRAMS)) & (pd.isna(enf_clean['PROGRAM.1'])) )
                          ]
    # drop enforcements with 'Draft' status
    enf_clean = enf_clean[enf_clean['STATUS.1'].apply(lambda x: x not in list(['Draft']))]
    if mmp_only:
        # keep only enforcement actions tied to MMP violations
        enf_clean = enf_clean[enf_clean['TOTAL MMP VIOLATIONS #'] > 0]
    return enf_clean


def clean_violations(viol, mmp_only=True, effluent_only=True):
    viol_clean = viol
    # remove dismissed violations
    viol_clean = viol_clean[viol_clean['STATUS.1'] != 'Dismissed']
    # keep only violations for NPDES permittees
    #viol_clean = viol_clean[viol_clean['PROGRAM'].apply(lambda x: x in NPDES_PROGRAMS)]
    viol_clean = viol_clean[
        # option 1: both fields contain NPDES programs
                          ( (viol_clean['PROGRAM'].isin(NPDES_PROGRAMS)) & (viol_clean['PROGRAM.1'].isin(NPDES_PROGRAMS)) ) |
        # option 2: one is empty and the other contains a NPDES program
                          ( (pd.isna(viol_clean['PROGRAM'])) & (viol_clean['PROGRAM.1'].isin(NPDES_PROGRAMS)) ) |
                          ( (viol_clean['PROGRAM'].isin(NPDES_PROGRAMS)) & (pd.isna(viol_clean['PROGRAM.1'])) )
                          ]
    # keep only MMP non-exempt violations
    viol_clean = viol_clean[viol_clean['EXEMPT (Y/N)'] == 'N']
    if mmp_only:
        viol_clean = viol_clean[(viol_clean['EXEMPT (Y/N)'] == 'N') |
                                (~pd.isna(viol_clean['MMP TYPE']))]
        viol_clean = viol_clean  # currently just taking all violations until we figure out how to define an MMP viol.
    if effluent_only:
        # drop non-effluent violations
        viol_clean = viol_clean[viol_clean['VIOLATION TYPE'] == 'Effluent']
    return viol_clean


def get_facility_entrance_exit(fac_reports, viol):
    """Estimates entrance/exit dates for each facility in population.

    Estimates the dates on which facilities entered and exited the population
    of NPDES permittees.

    NB: Here, we define 'entering the population' as a facility's receiving a
    NPDES permit. Defining when facilities exit the population is less
    straightforward: a facility's NPDES permit may expire, but until the permit
    is renewed or terminated, the permittee may still be discharging and is
    still regulated under NPDES. Furthermore, the TERMINATION DATE listed on a
    violation record may not actually reflect a population exit, as the
    permittee may have switched NPDES subprograms (e.g., from NPDESWW to
    NPDESNONMUNIPRCS, as in the case of FACILITY_ID 261701). Thus, as a
    heuristic until we can come up with a better way or do everything by hand,
    we define the date when a facility exits the population as the expiration
    date of the facility's last NPDES permit, or the latest permit termination
    date (TERMINATION DATE) in the violations record, whichever is latest. In
    the case that we don't have a permit termination date, the facility is
    assumed to still be active and is assigned an estimated exit date of NA.
    In some cases, violations will still appear after this date; however, they
    seem to be due to late reporting in all cases.

    Args:
        fac_reports: Facility reports table.
        viol: Violations table (pd.DataFrame).
    
    Returns:
        Violations table with estimated entrance and exit dates for each
        corresponding facility (pd.DataFrame).
    """
    # Extract NPDES enrollment and expiration dates. (NB: though all facilities
    # in our groups are NPDES permittees, not all have a Reg Measure on file of
    # type 'NPDES Permit' - I'm not quite sure why this is, as reasons seem to
    # vary by permittee.)
    fac_reports = fac_reports[fac_reports['Reg Measure Type'] == 'NPDES Permit']
    # Extract the oldest permit issuance date and most recent permit expiration date for each facility
    fac_reports = fac_reports.groupby('FACILITY_ID', as_index=False).agg(
        enroll_date=pd.NamedAgg(column='Effective Date', aggfunc=lambda x: x.min()),
        final_expiration_date=pd.NamedAgg(column='Expiration Date', aggfunc=lambda x: x.max()))
    # Merge with violations record by facility
    viol = viol.merge(fac_reports, on='FACILITY_ID', how='left')
    # Determine last permit termination date
    last_termination_dates = viol.groupby('FACILITY_ID', as_index=False).agg(
        last_termination_date=pd.NamedAgg(column='TERMINATION DATE', aggfunc=lambda x: x.max()))
    viol = viol.merge(last_termination_dates, on='FACILITY_ID')

    # Calculate estimated population entrance date
    viol['estimated_entrance_date'] = viol['enroll_date']

    # For facilities for which we don't have a NPDES enrollment date, take the
    # effective date of the active permit from the violations record. (Though
    # there may be multiple violations per facility, examination of the data
    # reveals all violations share an 'EFFECTIVE DATE' for facilities w/o a
    # NPDES enroll date.)
    for i in range(0, len(viol)):
        if pd.isna(viol['estimated_entrance_date'].iloc[i]):
            viol['estimated_entrance_date'].iloc[i] = viol['EFFECTIVE DATE'].iloc[i]

    # Convert estimated entrance date to a quarter. There are still some cases
    # where we don't have an 'EFFECTIVE DATE' or an 'enroll_date', in which case
    # dt.to_period() returns a negative value, so we need to manually set the
    # 'estimated_entrance_quarter' value to NaN.
    viol['estimated_entrance_quarter'] = viol['estimated_entrance_date'].dt.to_period(
        'Q').astype(int)
    viol['estimated_entrance_quarter'] = viol['estimated_entrance_quarter'].apply(
        lambda x: np.nan if (x < 0) else x)
    # Calculate estimated population exit date
    viol['estimated_exit_date'] = viol[
        ['last_termination_date', 'final_expiration_date']].max(axis=1)

    # Similar handling as above for 'estimated_exit_quarter'
    viol['estimated_exit_quarter'] = viol['estimated_exit_date'].dt.to_period(
        'Q').astype(int)
    viol['estimated_exit_quarter'] = viol['estimated_exit_quarter'].apply(
        lambda x: np.nan if (x < 0) else x)

    return viol


def get_enf_rate(fac, enf, viol, treat_start, pretreat_start=None):
    """Calculates enforcement rates for facilities before treatment begins.

    Calculates facility-level enforcement rates during the period prior to a specified treatment period. Optional: also
    calculates enforcement rates during a specified 'pretreatment' period.

    Args:
        fac: Facilities of interest, as 'FACILITY_ID' column (pd.Dataframe).
        enf: Table of enforcements of interest - e.g., MMP enforcements (pd.Dataframe).
        viol: Table of violations of interest (pd.Dataframe).
        treat_start: Treatment start date in YYYY-MM-DD format (str).
        pretreat_start: Pretreatment start date in YYYY-MM-DD format (str). Default is None.

    Returns:
        Table of facilities with associated enforcement rates.
    """
    print('Calculates enforcement rates for facilities.')

    print('     Calculating enforcement rates prior to treatment.')
    # get count of violations linked to prior enforcements for each facility
    enf_prior_violcounts = \
        enf[enf['EFFECTIVE DATE.1'] < pd.to_datetime(treat_start)
            ].groupby('FACILITY_ID', as_index=False).sum()[['FACILITY_ID',
                                                            'TOTAL MMP VIOLATIONS #']]
    enf_prior_violcounts.rename(columns={'TOTAL MMP VIOLATIONS #': 'enforcement_prior_violcount'},
                                inplace=True)
    # get count of ALL violations prior to treatment
    fac = fac.merge(enf_prior_violcounts, how='left', on='FACILITY_ID')

    viol_prior = viol[viol['OCCURRED ON'] < pd.to_datetime(treat_start)
                      ].groupby('FACILITY_ID', as_index=False).nunique()[['FACILITY_ID',
                                                                          'VIOLATION ID (VID)']]
    viol_prior.rename(columns={'VIOLATION ID (VID)': 'violations_prior_count'},
                      inplace=True)
    fac = fac.merge(viol_prior, how='left', on='FACILITY_ID')
    # calculate enforcement rate during period prior to treatment
    fac['enforcement_prior_rate'] = fac['enforcement_prior_violcount'] / fac['violations_prior_count']

    if pretreat_start is not None:
        print('     Calculating enforcement rates during pretreatment period.')
        # get count of violations linked to pretreatment enforcements for each facility
        enf_pretreat_violcounts = \
            enf[(enf['EFFECTIVE DATE.1'] < pd.to_datetime(treat_start)) &
                (enf['EFFECTIVE DATE.1'] >= pd.to_datetime(pretreat_start))
                ].groupby('FACILITY_ID', as_index=False).sum()[['FACILITY_ID',
                                                                'TOTAL MMP VIOLATIONS #']]
        enf_pretreat_violcounts.rename(
            columns={'TOTAL MMP VIOLATIONS #': 'enforcement_pretreat_violcount'},
            inplace=True)
        # get count of ALL violations during pretreatment period
        fac = fac.merge(enf_pretreat_violcounts, how='left', on='FACILITY_ID')

        viol_pretreat = \
            viol[(viol['OCCURRED ON'] < pd.to_datetime(treat_start)) &
                 (viol['OCCURRED ON'] >= pd.to_datetime(pretreat_start))
                 ].groupby('FACILITY_ID', as_index=False).nunique()[['FACILITY_ID',
                                                                     'VIOLATION ID (VID)']]
        viol_pretreat.rename(columns={'VIOLATION ID (VID)': 'violations_pretreat_count'}, inplace=True)
        fac = fac.merge(viol_pretreat, how='left', on='FACILITY_ID')
        # calculate enforcement rate during pretreatment period
        fac['enforcement_pretreat_rate'] = fac['enforcement_pretreat_violcount'] / fac['violations_pretreat_count']

    # replace NaN with 0
    fac.fillna(0, inplace=True)

    return fac


def get_treatment_comparison_groups(fac_with_enf_rates, enf_mmp_epl, treat_start, treat_end, high_enf_thresh,
                                    low_enf_thresh, pretreat=False):
    """Creates lists of treatment and comparison facilities.

    Given a table of facilities with prior enforcement rates (and, optionally, pretreatment enforcement rates),
    identifies a treatment group and a comparison group based on a) EPL (non-) receipt during treatment period and b)
    enforcement rates prior to the treatment period.

    NB: Pretreatment functionality isn't yet built out here.

    Args:
        fac_with_enf_rates: Facilities of interest with associated enforcement rates (pd.Dataframe).
        enf_mmp_epl: Table of MMP EPL enforcements (pd.Dataframe).
        treat_start: Treatment start date in YYYY-MM-DD format (str).
        treat_end: Treatment end date in YYYY-MM-DD format (str).
        high_enf_thresh: Threshold for 'consistent' enforcement prior to treatment, as proportion (float).
        low_enf_thresh: Threshold for 'no' enforcement prior to treatment, as proportion (float).
        pretreat: Flag for whether we're considering a pretreatment period or not (default = False).

    Returns:
        fac_treat: treatment facilities by 'FACILITY_ID' (list of str).
        fac_comp: comparison facilities by 'FACILITY_ID' (list of str).
    """
    print('Creating lists of treatment and comparison groups.')

    # identify recipients of treatment EPLs
    print('     Identifying facilities that did and did NOT receive EPLs during treatment period.')
    fac_epl_treat = enf_mmp_epl[
        (enf_mmp_epl['estimated_epl_date'] >= pd.to_datetime(treat_start)) &
        (enf_mmp_epl['estimated_epl_date'] <= pd.to_datetime(treat_end))
        ]
    fac_epl_treat = fac_epl_treat['FACILITY_ID'].unique()
    fac_with_enf_rates['epl_treat_flag'] = fac_with_enf_rates['FACILITY_ID'].isin(fac_epl_treat)

    print('     Identifying facilities with \'consistent\' enforcement.')
    # identify facilities with 'consistent' enforcement
    fac_with_enf_rates['enforcement_prior_flag'] = fac_with_enf_rates['enforcement_prior_rate'] >= high_enf_thresh
    if pretreat is not False:
        fac_with_enf_rates['enforcement_pretreat_flag'] = fac_with_enf_rates['enforcement_pretreat_rate'] >= \
                                                          high_enf_thresh

    print('     Identifying treatment and comparison facilities.')
    # treatment facilities need to have a) received an EPL during our treatment period
    # and b) had a low MMP enforcement rate prior to the treatment period
    fac_treat = list(fac_with_enf_rates[(fac_with_enf_rates['epl_treat_flag']) &
                                        (fac_with_enf_rates['enforcement_prior_rate'] <= low_enf_thresh)
                                        ]['FACILITY_ID'])
    # comparison facilities need to have a) NOT received an EPL during the treatment period
    # and b) had a high MMP enforcement rate prior to the treatment period
    fac_comp = list(fac_with_enf_rates[~(fac_with_enf_rates['epl_treat_flag']) &
                                       (fac_with_enf_rates['enforcement_prior_flag'])]['FACILITY_ID'])

    return fac_treat, fac_comp


def regression_preprocess_facility(treat_viol, comp_viol, mmp_viol, timescale='quarterly'):
    """Preprocesses treatment and comparison violations data for regression.

    Args:
        treat_viol: Table of violations for treatment group (pd.DataFrame).
        comp_viol: Table of violations for comparison group (pd.DataFrame).
        mmp_viol: Table of all valid MMP violations being considered (pd.DataFrame).
        timescale: Temporal resolution - i.e., are we considering violations per quarter or per year (str). Options:
                   'quarterly' and 'yearly' (default: 'quarterly').
    
    Returns:
        Table of violations for regression analysis (pd.DataFrame).
    """

    time_var = 'violation_quarter'
    if timescale == 'yearly':
        time_var = 'violation_year'

    # Record year of violation
    treat_viol['violation_year'] = treat_viol['OCCURRED ON'].dt.year
    comp_viol['violation_year'] = comp_viol['OCCURRED ON'].dt.year
    treat_viol['violation_quarter'] = treat_viol['OCCURRED ON'].dt.to_period('Q').astype(int)
    comp_viol['violation_quarter'] = comp_viol['OCCURRED ON'].dt.to_period('Q').astype(int)

    # Aggregate number of violations each year for each group
    treat_viol_grouped = treat_viol.groupby(['FACILITY_ID', time_var], as_index=False).nunique()[
        ['FACILITY_ID', time_var, 'VIOLATION ID (VID)']]
    comp_viol_grouped = comp_viol.groupby(['FACILITY_ID', time_var], as_index=False).nunique()[
        ['FACILITY_ID', time_var, 'VIOLATION ID (VID)']]
    
    treat_viol_grouped.rename(columns={'VIOLATION ID (VID)': 'n_violations'}, inplace=True)
    comp_viol_grouped.rename(columns={'VIOLATION ID (VID)': 'n_violations'}, inplace=True)

    # fill in with 'n_violations' = 0 during periods w/o violations
    viol_periods = pd.DataFrame({time_var: [x for x in range(min(treat_viol[time_var].min(), comp_viol[time_var].min()),
                                                             max(treat_viol[time_var].max(), comp_viol[time_var].max())
                                                             + 1)]})
    treat_viol_timeseries = pd.DataFrame()
    for facility in treat_viol_grouped['FACILITY_ID'].unique():
        facility_timeseries = treat_viol_grouped[treat_viol_grouped['FACILITY_ID'] == facility]
        facility_timeseries = pd.merge(facility_timeseries, viol_periods,
                                       on=time_var, how='outer').fillna(0).sort_values(by=time_var)
        facility_timeseries = facility_timeseries.assign(FACILITY_ID=facility)
        treat_viol_timeseries = pd.concat([treat_viol_timeseries, facility_timeseries])
    comp_viol_timeseries = pd.DataFrame()
    for facility in comp_viol_grouped['FACILITY_ID'].unique():
        facility_timeseries = comp_viol_grouped[comp_viol_grouped['FACILITY_ID'] == facility]
        facility_timeseries = pd.merge(facility_timeseries, viol_periods,
                                       on=time_var, how='outer').fillna(0).sort_values(by=time_var)
        facility_timeseries = facility_timeseries.assign(FACILITY_ID=facility)
        comp_viol_timeseries = pd.concat([comp_viol_timeseries, facility_timeseries])

    # Create dummy treatment variable and concatenate the two tables
    treat_viol_timeseries['treatment'] = 1
    comp_viol_timeseries['treatment'] = 0
    violations_to_analyze = pd.concat([treat_viol_timeseries, comp_viol_timeseries])

    # determine violation status (1 if any violations during period, else 0)
    violations_to_analyze['violation_status'] = violations_to_analyze['n_violations'].apply(lambda x: x > 0).astype(int)

    # Merge in data linking each facility to a region, for use with region fixed effects
    violations_to_analyze = violations_to_analyze.merge(
        mmp_viol[['FACILITY_ID', 'VIOLATED FACILITY REGION']].drop_duplicates('FACILITY_ID'), on='FACILITY_ID',
        how='left').rename(columns={'VIOLATED FACILITY REGION': 'Region'})

    # Create dummy variable indicating post-treatment
    if timescale == 'yearly':
        violations_to_analyze['post'] = (violations_to_analyze[time_var] > 2008).astype(int)
    if timescale == 'quarterly':
        violations_to_analyze['post'] = (violations_to_analyze[time_var] > 155).astype(int)

    # Drop facility column
    #violations_to_analyze.drop(['FACILITY_ID'], axis=1, inplace=True)

    # generalize violation year/quarter column
    if timescale == 'quarterly':
        violations_to_analyze.rename(columns={'violation_quarter': 'violation_period'}, inplace=True)
    elif timescale == 'yearly':
        violations_to_analyze.rename(columns={'violation_year': 'violation_period'}, inplace=True)

    return violations_to_analyze


def compare_composition(data_a, data_b, column, top_n=3):
    # This function takes in a comparison group, treatment group, and a column name,
    # and returns the top n values of the column in order of the absolute difference
    # in composition of the two groups. Streamlines compositional analysis.
    A = pd.DataFrame(data_a[column].value_counts(normalize=True)).rename(columns={column: 'Comparison'})
    B = pd.DataFrame(data_b[column].value_counts(normalize=True)).rename(columns={column: 'Treatment'})
    results = pd.concat([A, B], axis=1).fillna(0)
    results['diff'] = np.abs(results['Comparison'] - results['Treatment'])
    results = results.apply(lambda x: x.round(2))
    results.sort_values(by='diff', inplace=True, ascending=False)
    results = results.rename_axis('Value').reset_index().set_index(pd.Index([column] * len(results)))

    # Add p-value
    results['p_value (fishers_exact_test)'] = np.nan
    for i in range(0, len(results)):
        variable = results.index[i]
        value = results['Value'].iloc[i]
        count_a = len(data_a[data_a[variable] == value])
        count_b = len(data_b[data_b[variable] == value])
        results['p_value (fishers_exact_test)'].iloc[i] = ss.fisher_exact([[count_a, len(data_a)-count_a], [count_b, len(data_b)-count_b]])[1]
    return results.head(top_n)


def get_reported_values(dmr_vals, timescale='quarterly'):
    """Gets number of reported DMR values per permittee per specified time period.

    Args:
        dmr_values: Table of DMR values, with one value per row (pd.DataFrame).
        timescale: Temporal resolution - i.e., are we considering violations per quarter or per year (str). Options:
                   'quarterly' and 'yearly' (default: 'quarterly').
        impute_missing: Flag for whether to impute missing DMR parameter counts (for facilities only partially
                        missing data) or not (default = False).
    
    Returns:
        Table of count of reported DMR values by permittee (as NPDES permit ID) and time period (pd.DataFrame).
    """

    if timescale == 'quarterly':
        dmr_vals['monitoring_period_end'] = dmr_vals['monitoring_period_end_date'].dt.to_period('Q').astype(int)
    elif timescale == 'yearly':
        dmr_vals['monitoring_period_end'] = dmr_vals['monitoring_period_end_date'].dt.year
    
    # filter out empty values, then drop duplicate rows
    reported_dmr_vals = dmr_vals.dropna(subset=['dmr_value_nmbr']).drop_duplicates()

    # get count of values reported by each permittee during specified time period
    reported_dmr_vals_grouped = reported_dmr_vals.groupby(['npdes_permit_id', 'monitoring_period_end']
                                                          ).size().reset_index(name='n_values_permit_period')
    
    reported_dmr_vals_grouped['mean_n_values_permit'] = reported_dmr_vals_grouped['n_values_permit_period'].groupby(reported_dmr_vals_grouped['npdes_permit_id']).transform('mean')
    
    return reported_dmr_vals_grouped


# -----------------------------------------------------------------------------

if __name__ == '__main__':
    # read in configuration file
    with open(Path().resolve().parent / 'config.yml', 'r') as file:
        configs = yaml.safe_load(file)
    data_path = Path(configs['data_path'])

    # read in data
    print('Reading violations file.')
    violations = pd.read_excel(data_path / 'violations_export_2021-10-26.xlsx', dtype={'FACILITY_ID': str})
    print('Reading enforcements file.')
    enforcements = pd.read_csv(data_path / 'enf_actions_export.csv',
                               dtype={'FACILITY ID': object},
                               parse_dates=['DATE OF OLDEST VIOLATION LINKED TO ENFORCEMENT ACTION',
                                            'EFFECTIVE DATE.1'],
                               date_parser=lambda x: pd.to_datetime(x, errors='coerce'))
    enforcements.rename(columns={'FACILITY ID': 'FACILITY_ID'}, inplace=True)
    print('Reading enforcements-violations crosswalk file.')
    enf_viol_links = pd.read_excel(data_path / 'EnfActionToViolationRships.xlsx', dtype={'Violation ID': object})
