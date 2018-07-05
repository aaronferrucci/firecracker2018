# firecracker2018
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/master/time_vs_age.svg "SVG image")

To reproduce these results: 
1. The race data is [here](https://runsignup.com/Race/Results/47158/?rsus=100-200-fe20fdef-2ac6-4a9b-ba22-de732186e749#resultSetId-121795)
  * I resorted to displaying all results, selecting, copying, and pasting into
    a file (data/raw_10k)
  * data/Makefile cleans up the raw data, to a form suitable for read.table.
2. Load the project in RStudio
3. source("firecracker2018.R")

From there you can export the plot, or experiment further with the data set.

This data set is rather messy (like all race data sets I've seen). Here are
some issues I noticed:
1. Of the 319 entries, 207 lack their bib number.
2. Gender seems wrong in some cases: in particular, the 2nd place winner, "Luke
   Colosi", is marked as female. (Luke's Division is "M 19-24", though, so the
   data isn't even internally consistent.)
3. 3 entries have Division="No Age Given"; 2 of those do have an age listed
4. Of the two entries with no age listed, one lists a Division.

