
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

# manually clean the data. 2026 data needed less cleaning than previous years.
cleanit <- function(data, tag) {

  return(data)
}

divisions = data.frame(
  xmin=c(8, 13, 19, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 99) - 0.5,
  xmax=c(12, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 98, 99) + 0.5,
  ymin=c(-Inf),
  ymax=c(Inf)
)

# the last line of the raw data is malformed; I've deleted that line in the 'manually-modified' data.
data10k <- read.table("data/firecracker_10k_2026_manually_modified.txt", header=T, sep="\t", quote="", stringsAsFactors=F)
data10k <- cleanit(data10k, "data10k")

hoursMinutesSeconds <- strsplit(data10k$Gun.Elapsed.Time, ":")
data10k$Time <- sapply(hoursMinutesSeconds, function(hms) Reduce(function(acc, x) as.numeric(acc) * 60 + as.numeric(x), hms ))
time_ticks <- seq(5 * 60, max(data10k$Time), 10 * 60)
age_ticks <- seq(10, max(data10k$Age, na.rm=T), 10)

plot10k <-
  ggplot(data10k) +
  geom_rect(data=divisions, aes(xmin=xmin, xmax=xmax,ymin=ymin,ymax=ymax), fill="moccasin",linetype=0, alpha=0.3) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) +
  geom_point(aes(x=Age, y=Time, color=Gender)) +
  ggtitle("Firecracker 10k") +
  theme(plot.title = element_text(hjust = 0.5)) +
  expand_limits(x = 10, y = 35*60) +
  scale_y_continuous(breaks = time_ticks, labels = timestr(time_ticks), name = "elapsed time (h:mm:ss)") +
  scale_x_continuous(breaks = age_ticks) +
  geom_smooth(data=data10k, level=0.95, se=T, method="loess", aes(x=Age, y=Time, color=Gender), formula = y~x)

extra <- data10k[c(grep("Ferrucci", data10k$Name), grep("Erick Castillo", data10k$Name), grep("Lieby", data10k$Name)),]
plot10k <- plot10k + geom_point(data=extra, aes(x = Age, y = Time))

print(plot10k)
svg(filename="time_vs_age10k.svg")
print(plot10k)
dev.off()
