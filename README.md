# firecracker2019
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/2019/time_vs_age5k.svg "5k time vs. age image")
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/2019/time_vs_age10k.svg "10k time vs. age image")

To reproduce these results: 
1. The race data is [here](https://runsignup.com/Race/Results/47158/?rsus=100-200-fe20fdef-2ac6-4a9b-ba22-de732186e749#resultSetId-121795)
  * I resorted to displaying all results, selecting, copying, and pasting into
    a file (data/raw_10k, data/raw5k)
  * data/Makefile cleans up the raw data, to a form suitable for read.table.
2. Load the project in RStudio
3. source("firecracker2018.R")

From there you can export the plots, or experiment further with the data set.

Data cleanup: there were a few glaring errors in the data. Here are notes and
resolutions:
1. The last 10k finisher took 10 hours. Deleted that record, sorry Debbie.
2. The first 5k finisher had a world-record-breaking pace (just over 3-minute 
   miles!). Deleted that record.

