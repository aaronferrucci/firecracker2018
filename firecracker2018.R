library(ggplot2)
library(testit)

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

data5k <- read.table("data/processed_5k.txt", sep="\t", quote="", stringsAsFactors=F)
# column 3 is all NA. Remove it
assert("column 3 is all NA", all(is.na(data5k[,3])))
data5k <- data5k[-3]

names(data5k) <- c("Place", "Bib", "FirstName", "LastName", "Gender", "City", "State", "Country", "ChipTime", "Age", "AgePercent", "DivisionPlace", "Division")

hoursMinutesSeconds <- strsplit(data5k$ChipTime, ":")
data5k$Time <- sapply(hoursMinutesSeconds, function(hms) Reduce(function(acc, x) as.numeric(acc) * 60 + as.numeric(x), hms ))
time_ticks <- seq(5 * 60, max(data5k$Time), 10 * 60)
age_ticks <- seq(0, max(data5k$Age, na.rm=T), 10)

divisions = data.frame(xmin=c(13, 25, 35, 45, 55, 65, 75) - 0.5, xmax=c(18, 29, 39, 49, 59, 69, 79) + 0.5, ymin=c(-Inf), ymax=c(Inf))
plot5k <-
  ggplot(data5k) +
  geom_rect(data=divisions, aes(xmin=xmin, xmax=xmax,ymin=ymin,ymax=ymax), fill="moccasin",linetype=0, alpha=0.3) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
  geom_point(aes(x=Age, y=Time, color=Gender)) +
  ggtitle("2021 Firecracker 5k") +
  theme(plot.title = element_text(hjust = 0.5)) +
  expand_limits(x = 0, y = 15*60) +
  scale_y_continuous(breaks = time_ticks, labels = timestr(time_ticks), name = "elapsed time (h:mm:ss)") +
  scale_x_continuous(breaks = age_ticks) +
  geom_smooth(data=data5k, method="loess", aes(x=Age, y=Time, color=Gender), formula = y~x)

## To highlight any particular data (points plotted as black)
# extra <- data5k[data5k$City == "Santa Cruz",]
# plot5k <- plot5k + geom_point(data=extra, aes(x = Age, y = Time))
## to do: how to leave the "fill" per gender, but put a black border on each point?

print(plot5k)
svg(filename="time_vs_age5k.svg")
print(plot5k)
dev.off()
