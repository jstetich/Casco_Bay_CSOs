
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

## Unmeasured Discharges From From2015
In addition, missing data has different import in 2015.  In 2015, "--",
which is interpreted as missing by our code, is flagged in the source file
as meaning "metering data unavailable."  It is quite abundant at some CSO sites.  

In later years, missing values ("NA"s) generally only means no discharges were
detected, so they function as an implicit zero.

There may be no easy way to make these data fully consistent.  However, to
prevent severe misinterpretation of the 2015 data, we looked at each
CSO to determine how often it was marked with either the "--" code or the
"BLOCK" code.  A total of eleven CSOs were not monitoried during at least on
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

WE accessed data from the 2008 report, which CBEP has in our archives, to
provide older data. Users should be cautious, as some of the older data (prior
to about 1997) is estimated, and considered unreliable. 

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


## Processing Files for Analysis in R.





# Altering GIS files to match the PWD data

Working from "Casco_Bay_CSOs_2", recently prepared from DEP on-line GIS data
files. Data downloaded May 29, imported as event layer, and converted to
shapefile.

Note that these GIS files also provide ID numbers, unique within towns.
Partial matching shows that the numbers in the GIS data (Derived from DEP data)
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

Finally, I simplified the attributes, by removing several data columns that I
don't need for now.

 

# Preparation of Statewide CSO data for regional totals

(1) Downloaded 2017 CSO report from DEP. "2017_status_report.pdf"
(2) Copied tables from the PDF file using simple copy.  Format of data not
directly importable into Excel as tables.
(3) imported into Excel file , "CSO data Discharge from 2017 State CSO
Report.xlsx"  Import by pasteing data and rearranging, or via a python
script that reordered data to simplify the task.  In either case, mport
required hand corrections.
(4) Checked that all calculated annual totals matched totals provided in the PDF.
(5) COnstructed pivot tables to extract annual total discharges from Casco Bay
CSO communities.


