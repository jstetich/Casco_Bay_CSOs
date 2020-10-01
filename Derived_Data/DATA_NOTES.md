# Basic Data Prep -- 2018 version.
Step 1:  Combine those data sets into Portland CSO Activity 2015-2017.xlsx

Step 2:  Add a Tab to extract data on total annual EEWTF overflows from the
         original data tables.

Step 3:  Add row with simplified data column names, just below existing headers,
         on all three spreadsheets.


Notice that the data from 2015 uses a slightly different data format from 2016
and 2017.  It is structured with a row for each DAY within events.  All other
data sheets have a row for each EVENT, with a start date and an end date.


Step 4:  Create Pivot Table to reframe 2015 data by events ("2015 Pivot Table"),
following format of other years.  Rename original 2015 data tab "2015 original".
Add tab for data derived from pivot table (2015) so that data can be edited
without altering original data or pivot table.

Step 5:  Clean up data in "2015" data tab, by deleting data looked up from the
pivot table for dates/events when data was missing in the "2015 Original".


Notice that is not consistent with how later years were reoported, where data
appears where discharges were monitored. Only a handful of sites show missing
data. I don't think there's an easy way to make these data fully consistent.  We
probably need to figure out whether there are similar gaps in monitoring data in
later years.

Step 6:  Combine data for 2015, 2016, and 2017 into a single tab, "Combined".
Check data alignment. Convert CSO column headers from CSO<space>### to CSO### to
simplify later analysis.  Save file

Step 7:  Correct uncovered errors (ANY SPECIAL EDITING NOT IN FORMULAS MARKED IN
YELLOW):
	Revise formulas for 2015 to carry forward missing data, not replace all with zeros.
	(Assume any missing data is proporly flagged n 2016 and 2017.)
	Correct date for start date of 2015 event 18 from 2014 to 2015
	Second row of data in 2016 spreasheet is eroneously maked with a 2017 date. Correct to 2016
	Remove word "BLOCK" from CSO002 2016 event 1.
	Remove multiple missing value symbols from CSO 024, year 2017
	Remove mising value symbol from CSO 017  event 2017.07
	Remove mising value symbol from CSO 012  event 2017.08

Step 8:  Export that data sheet to CSV file for analysis



# Altering GIS files to match the PWD data

Working from "Casco_Bay_CSOs_2", recently prepared from DEP on-line GIS data
files. Data downlaoded May 29, imported as event layer, and converted to
shapefile.

Note that these GIS files also provide ID numbers, unique within towns.
Partial matching shows that the numbers in the GIS data (Deriived from DEP data)
appear to line up with the numbers presented in the Portland CSO discharge data.
However, there are a couple of CSOs that don't line up.

CSO 35 and CSO 36 and are included in the GIS data, but not in the discharge
data. They are labeled in the GIS data as not active, so it makes sense that
they would not be monitored by PWD.

CSO 42 and 43 -- the Warren Ave, Capisic Brook discharges -- are not included in
the GIS data, but are included in discharge data. CSO 042 is included in all
three years.  CSO 043 is included in the 2015 and 2016 data, but with no
reported discharges.

I copied data on CSO 42 from the CSO data layer in our 2015 SotB data archive,
and added it to "Casco_Bay_CSOs_2", creating "Casco_Bay_CSOs_3".

Finally, I simplified the atributes, by removing several data columns that I
don't need for now.

 

# Preparation of Statewide CSO data for regional totals

(1) Downloaded 2017 CSO report from DEP. "2017_status_report.pdf"
(2) Copied tables from the PDF file using simple copy.  Format of data not
directly importable into Excel as tables.
(3) imported into Excel file , "CSO data Discharge from 2017 State CSO
Report.xlsx"  Import by pasteing data and rearranging, or via a python
script that reordered data to simplify the task.  In either case, inport
required hand corrections.
(4) Checked that all calculated annual totals matched totals provided in the PDF.
(5) COnstructed pivot tables to extract annual total discharges from Casco Bay
CSO communities.


