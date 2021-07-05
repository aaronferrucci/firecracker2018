# firecracker2021
![alt text](https://github.com/aaronferrucci/firecracker2018/blob/2019/time_vs_age5k.svg "5k time vs. age image")

To reproduce these results: 
1. The race data is [here](https://runsignup.com/Race/Results/47158#resultSetId-260924;perpage:5000)
  * I resorted to displaying all results, selecting, copying, and pasting into
    a file (data/raw_5k)
  * data/Makefile cleans up the raw data, to a form suitable for read.table.
2. Load the project in RStudio
3. source("firecracker2018.R")

From there you can export the plots, or experiment further with the data set.


