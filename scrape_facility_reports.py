
import os
import pandas as pd
import requests
from bs4 import BeautifulSoup
from IPython.display import clear_output

# We want to generate a complete list of permits with historical versions.
# Specifically, we want WDID, NPDES ID, permit version, start date, and end date.
# The information is available in CIWQS's public Facility at-a-glance report:
# https://ciwqs.waterboards.ca.gov/ciwqs/readOnly/CiwqsReportServlet?reportName=facilityAtAGlance&inCommand=reset
# This script automates the process of scraping reports.


def scrape_facility_reports(facility_id_list, verbose=False):
	"""
	@param facility_id_list: array-like, list of facility IDs to scrape data for
	@param verbose: bool, if True, print checkpoint messages. Default False.
	@return: pd DataFrame of facilities with covariates from the Facility-
	at-a-Glance reports.
	"""

	# Get facility at a glance report for each place id
	place_dfs = []
	errors = []
	for i in range(0, len(facility_id_list)):
		facility_id = facility_id_list[i]
		url = f'https://ciwqs.waterboards.ca.gov/ciwqs/' \
			  f'readOnly/CiwqsReportServlet?inCommand=drilldown&reportName=' \
			  f'facilityAtAGlance&placeID={facility_id}'
		try:
			r = requests.get(url)
			soup = BeautifulSoup(r.text, 'html.parser')
			table = soup.find('td', id = 'sc2')
			elements = table.findAll('td')
			cells = [e.text for e in elements]
			n_rows = int(len(cells )/10)
			df = []
			for n in range(1, n_rows):
				row = pd.DataFrame([cells[n*10:(n+1)*10]])
				df.append(row)
			if len(df) != 0:
				place_df = pd.concat(df)
				place_df['FACILITY_ID'] = facility_id
				place_dfs.append(place_df)
		except:
			errors = errors + [facility_id]
		if verbose:
			print(str(round(100*i/len(facility_id_list))) + '%')
			clear_output(wait=True)
	clear_output()

	# Combine data from all facilities
	facilities = pd.concat(place_dfs)
	facilities.columns = cells[:10] + ['FACILITY_ID']

	# Coerce date columns to datetime
	facilities['Effective Date'] = pd.to_datetime(facilities['Effective Date'], errors='coerce')
	facilities['Expiration Date'] = pd.to_datetime(facilities['Expiration Date'], errors='coerce')

	if len(errors) > 0:
		print('Errors for the following facilities: ')
		print(errors)

	return facilities


if __name__ == "__main__":
	data_dir = os.path.join(os.getenv('OAK'), 'EPA', 'Data', 'manual', 'california')
	fac_list = pd.read_csv(os.path.join(data_dir, 'Facility_At-A-Glance_List_2021-12-06.csv'))
	place_ids = fac_list['Place ID']
	scrape_facility_reports(place_ids)
