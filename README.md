# firecracker2019
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/2019/time_vs_age5k.svg "5k time vs. age image")
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/2019/time_vs_age10k.svg "10k time vs. age image")

To reproduce these results: 
1. The race data is [here](https://runsignup.com/Race/Results/47158/?rsus=100-200-fe20fdef-2ac6-4a9b-ba22-de732186e749#resultSetId-161091;perpage:10)
  * I resorted to displaying all results, selecting, copying, and pasting into
    a file (data/raw_10k, data/raw5k)
  * data/Makefile cleans up the raw data, to a form suitable for read.table.
2. Load the project in RStudio
3. source("firecracker2018.R")

From there you can export the plots, or experiment further with the data set.

Data cleanup: the 5k data has a few glaring errors. Here are notes and
resolutions:
1. Incomplete records (missing runtime): deleted.
2. Bib #247 has a run time of more than 100 hours! Deleted.

