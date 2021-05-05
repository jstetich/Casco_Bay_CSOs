
<img src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Preparation of Portland CSO Data
We use an R Notebook to prepare the Portland data for  analysis, working with
data from 2015 through 2019.

## Reformatting 2015 Data
The data from 2015 uses a slightly different  format from later years.  It is
structured with a row for each DAY within events.  All other data sheets have
a row for each EVENT, with a start date and an end date.

We use R code to transform the data into the same format as presented from later
years.

## Unmeasured Discharges From 2015
In addition, missing data has different import in 2015.  In 2015, "--",
which is interpreted as missing by our code, is flagged in the source file
as meaning "metering data unavailable."  It is quite abundant at some CSO sites.  

In later years, missing values ("NA"s) generally only means no discharges were
detected, so they function as an implicit zero.

There may be no easy way to make these data fully consistent.  However, to
prevent severe misinterpretation of the 2015 data, we looked at each
CSO to determine how often it was marked with either the "--" code or the
"BLOCK" code.  A total of eleven CSOs were not monitored during at least one
CSO event.

Three CSOs were always marked as not monitored:

*  CSO 002  
*  CSO 009  
*  CSO 011  

Two other CSO locations were frequently not monitored:

*  CSO_007  
*  CSO_023  

Results from these five CSOs should be used with caution for 2015.

# Data From DEP Annual CSO Reports
We accessed data from the 2008, 2016, and 2019 Annual CSO Reports, as described
in the DATA_SOURCES.md page under Original_Data. We relied most closely on the
data from the 2019 Report.

The relevant data tables are on Pages 10 (volumes) and 11 (events) of the 2019
report. Because these are PDF documents, extracting the data without errors is
slightly tricky.

## Extracting Data to Excel Files
We extracted data to Excel Files for further processing as follows:

### Data From the 2019 Report 
1. Copy the entire table from the PDF, and paste it into Word. The formatting
   is somewhat awkward, but it is all there.  
2. Copy the entire table from *Word*, and paste it into a new word document,
   using "Paste text only." This generates a tab delimited text file, however,
   it is hard to check or correct the file.  
3. Copy the tab-delimited text, and paste it into a new Excel Spreadsheet.  
4. This Excel spreadsheet requires minor corrections, for example, DEP
   reports City of Portland and Portland Water District permits on a single
   line, which does not get carried over to Excel correctly.
5. Check over the data for misalignment of rows, and correct any discrepancies
   found.  Any town where CSOs were not reported for several years, like
   Rockland, will be misaligned. Any totals rows will be misaligned.
6. The 2008 report does not provide data on the number of outfalls.  Neither do
   the 2013 or 2014 reports we  also archived.  We can add three more years of
   record on the number of outfalls by extracting data from the 2016
   report and adding it to the 2019 data.  The data from the 2016 report could
   not be copied successfully from the PDF, so data was copied by hand and typed
   manually into the spreadsheet.

We accessed data from the 2008 report, which CBEP has in our archives, to
provide older data. Users should be cautious, as some of the older data (prior
to about 1997) is estimated, and considered unreliable. Wedo not report on
that older data in the 2020 Report because of uncertainty about its reliability.

### Data From the 2008 Report
Unfortunately, the older report was prepared slightly differently, and copying
and pasting from the PDF is less successful, since the resulting data is not
fully delimited.

1. Copy the entire table from the PDF, and paste it into Word.  It does not
   transfer to Word as a table, but as bare text.  
2. Use global search and replace to replace all whitespace (designated by "^w"
   in the Find and Replace Dialog box) with tabs ("^t^").  This will *almost*
   work, but it will divide extra table boundaries for any table cell that
   contained a space.  
3. Use "find" to locate tabs, and eliminate any that are in error, most
   of which are in town names or extra spaces  in the permit ID numbers.  
4. Copy and Paste the tab-delimited text to Excel, where it can be further
   checked and cleaned up.  
5. This Excel spreadsheet may require minor corrections. Especially, Check over
   the data for misalignment of rows, and correct any discrepancies found.  Any
   totals rows will be misaligned.


# GIS Data
## Source
The original geospatial data was downloaded on September 12, 2019 by
Curtis C. Bohlen, from:
https://geolibrary-maine.opendata.arcgis.com/datasets/mainedep-cso.
The equivalent data has since moved to:
https://hub.arcgis.com/datasets/maine::mainedep-cso

This file contains a statewide CSO data layer, derived from permit data.
The file includes both "Active" and "Inactive" CSOs.

## We noted the following: 
*  Unlike data downloaded in previous years, the "STATUS" of all outfalls
   in the 2019 download was "ACTIVE".  (The data now available again contains 
   both "Active" and "Inactive" CSOs.)
*  "OUTFALL_ID" in this data set is NOT unique -- the ID numbers (here as
   strings, not numeric values) are apparently unique only within towns.  
*  "OUTFALL_NA" in Portland does not always exactly match the names in the
   Portland data we received from PWD.  The CSO numbers are consistent.
*  South Portland now only has four active CSO (per the 2019 CSO report), 
   but this data still includes six nominally "Active" CSOs. We correct that 
   in data preparation.

## Processing
1.  We used ArcGIS to select CSO locations in the Casco Bay region, and saved them
    in a shapefile "Regional_CSOs".   
2,  To facilitate working with Portland's CSOs, for which we have detailed
    storm by storm discharge data, we exported a "Portland only" subset of the
    data as the "Portland_CSOs" shapefile.  
3.  We simplified the attribute table in "Portland_CSOs"" by removing
    uninformative and minimally informative attributes.  
4.  We added a new attribute, "Town", which removes extraneous details from each
    "FACILITY_N", combines PWD and Portland DPW outfalls (to "Portland"), and
    renames towns in standard capitalization.  
5.  We realized the on-line data is out of date, and includes two South Portland
    CSO locations that are no longer Active.  The Annual CSO reports from DEP
    correctly states that South Portland now has only four CSOs.  That information 
    simply is not represented in the on-line geospatial data.  We contacted South
    Portland City staff, who confirmed that their CSO outfalls #4 and #19 are no
    longer Active.  We removed them from the geospatial data by hand.

For the "Portland_CSOs" shapefile, we took several additional steps:
5.  We added a new (calculated) attribute with the form "CSO_###", which matches
    the nomenclature used in the Portland-specific CSO discharge data from 
    2015 through 2019.  
6.  We imported the file "portland_cso_summary.csv", which was produced in R, to
    ArcGIS, and "Joined" it to the "Portland_CSOs" layer.
7.  We then created new attributes and transferred the data to the
    "Portland_CSOs" shapefile, thus adding data on CSO frequency and magnitude
    to the shapefile.  The presence of location names offers a chance to double
    check the text-based merge to make sure data was assigned to the correct
    CSO location.

