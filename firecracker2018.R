library(ggplot2)

timestr <- function(elapsed) {
  seconds <- elapsed
  hours <- as.integer(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60, digits=2)
  
  minute_prefix <- ifelse(minutes < 10, "0", "")
  minutes <- paste0(minute_prefix, minutes)
  second_prefix <- ifelse(seconds < 10, "0", "")
  seconds <- paste0(second_prefix, seconds)
  
  time <- paste(hours, minutes, seconds, sep=":")
  return(time)
}

data10k <- read.table("data/processed_10k.txt", sep="\t", stringsAsFactors=F)
names(data10k) <- c("Place", "Bib", "FirstName", "LastName", "Gender", "City", "State", "Country", "ClockTime", "ChipTime", "Pace", "Age", "AgePercent", "DivisionPlace", "Division")
hoursMinutesSeconds <- strsplit(data10k$ClockTime, ":")
data10k$Time <- sapply(hoursMinutesSeconds, function(hms) Reduce(function(acc, x) as.numeric(acc) * 60 + as.numeric(x), hms ))
time_ticks <- seq(0, max(data10k$Time), 10 * 60)

plot <-
  ggplot(data10k, aes(x = Age, y = Time, color=Gender)) +
  expand_limits(x = 0, y = 30*60) +
  scale_y_continuous(breaks = time_ticks, labels = timestr(time_ticks), name = "elapsed time (h:mm:ss)") + 
  geom_point()
print(plot)