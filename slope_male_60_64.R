library(ggplot2)
library(ggrepel)
library(dplyr)

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

parse_time <- function(hms_str) {
  sapply(strsplit(hms_str, ":"), function(hms) Reduce(function(acc, x) as.numeric(acc) * 60 + as.numeric(x), hms))
}

# 2025: no header row in the raw file
data2025 <- read.table("data/firecracker_10k_2025.txt", header=F, sep="\t", quote="", stringsAsFactors=F,
  col.names=c("Place","Bib","Full.Name","Gender","Age","City","State","Time",
              "GenderPlace","AgeGroup","AgeGroupPlace","Pace","Blank1","Blank2"))
data2025$Time <- parse_time(data2025$Time)
data2025$Year <- "2025"

# 2026: the last line of the raw data is malformed; using the manually-modified file (see main.R)
data2026 <- read.table("data/firecracker_10k_2026_manually_modified.txt", header=T, sep="\t", quote="", stringsAsFactors=F)
data2026$Time <- parse_time(data2026$Gun.Elapsed.Time)
data2026$Year <- "2026"

cols <- c("Full.Name", "Age", "Time", "Year")
combined <- rbind(data2025[data2025$Gender == "Male" & data2025$Age >= 60 & data2025$Age <= 64, cols],
                   data2026[data2026$Gender == "Male" & data2026$Age >= 60 & data2026$Age <= 64, cols])

combined <- combined %>%
  group_by(Year) %>%
  mutate(AgeGroupRank = rank(Time, ties.method="min")) %>%
  ungroup()

# names that raced male 60-64 in both years
repeat_names <- intersect(combined$Full.Name[combined$Year == "2025"],
                           combined$Full.Name[combined$Year == "2026"])

combined$Runner <- ifelse(combined$Full.Name %in% repeat_names, combined$Full.Name, "Other finishers")
combined$Runner <- factor(combined$Runner, levels=c(repeat_names, "Other finishers"))

field <- combined[combined$Runner == "Other finishers",]
repeats <- combined[combined$Runner != "Other finishers",]

# validated palette slots: magenta, green, violet, yellow. Andy Olson gets
# magenta (not the usual slot-1 blue) because it separates further from Aaron's
# green than blue does (normal-vision ΔE 35.9 vs 29.0, CVD ΔE 17.6 vs 26.5) --
# Andy's 2025 dot sits almost on top of Aaron's, so the extra separation matters.
runner_colors <- setNames(c("#e87ba4", "#008300", "#4a3aa7", "#eda100")[seq_along(repeat_names)], repeat_names)
runner_colors["Other finishers"] <- "#c3c2b7"

# highlight Aaron Ferrucci with a heavier line/point throughout, plus Andy
# Olson's 2025 dot, since it sits only 5 seconds from Aaron's that year
repeats$is_ferrucci <- repeats$Full.Name == "Aaron Ferrucci"
repeats$highlight_point <- repeats$is_ferrucci |
  (repeats$Full.Name == "Andy Olson" & repeats$Year == "2025")

time_ticks <- seq(45*60, 75*60, by=5*60)

label_data <- repeats[repeats$Year == "2026",]

set.seed(42)
slope_plot <-
  ggplot() +
  geom_point(data=field, aes(x=Year, y=Time), color="#c3c2b7", size=2, alpha=0.8,
             position=position_jitter(width=0.06, height=0)) +
  geom_line(data=repeats, aes(x=Year, y=Time, group=Full.Name, color=Runner, linewidth=is_ferrucci)) +
  geom_point(data=repeats, aes(x=Year, y=Time, color=Runner, size=highlight_point)) +
  geom_text_repel(data=label_data, aes(x=Year, y=Time, label=Full.Name, color=Runner),
                   nudge_x=0.15, hjust=0, direction="y", segment.size=0.3, show.legend=F) +
  scale_color_manual(values=runner_colors, breaks=repeat_names, name="Repeat finishers (M60-64)") +
  scale_linewidth_manual(values=c(`FALSE`=0.6, `TRUE`=1.4), guide="none") +
  scale_size_manual(values=c(`FALSE`=2.5, `TRUE`=4), guide="none") +
  scale_y_reverse(breaks=time_ticks, labels=timestr(time_ticks), name="elapsed time (h:mm:ss) - faster is higher") +
  scale_x_discrete(expand=expansion(add=c(0.3, 0.6))) +
  labs(x="Race year",
       title="Firecracker 10k - Male, age 60-64",
       subtitle="Aaron Ferrucci moved from 6th to 3rd in the age group (58:31 -> 57:08)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(slope_plot)
svg(filename="slope_male_60_64.svg", width=8, height=7)
print(slope_plot)
dev.off()
