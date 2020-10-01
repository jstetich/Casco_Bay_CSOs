# Preparation of Portland CSO Data
We use an R Notebook to prepare the Portland data for  analysis, working with
data from 2015 through 2019.

The data from 2015 uses a slightly different  format from later years.  It is
structured with a row for each DAY within events.  All other data sheets have
a row for each EVENT, with a start date and an end date.

We use R code to transform the data into the same format as presented from later
years.

In addition, it missing data has different import in 2015.  In 2015, "--",
which is interpreted as missing by our code, is flagged in the source file
as meaning "metering data unavailable."  It is quite abundant at some CSO sites.  

In later years, missing values ("NA"s) generally simply means no discharges were
detected, so they function as an implicit zero.

There may be no easy way to make these data fully consistent.  However, to
prevent severe misinterpretation of the 2015 data, we looked at each
CSO to determine how often it was marked with either the "--" code or the
"BLOCK" code.  A total of eleven CSOs were not monitoried during at least on
CSO event.

Three CSOs were always marked as not monitored:
CSO 002,
CSO 009
CSO 011

Two other CSO locations were frequently not monitored:
CSO_007
CSO_023

Results from these five CSOs should be used with caution for 2015, but the data
was NOT converted to 

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


