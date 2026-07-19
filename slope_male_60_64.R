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

band_of_age <- function(age) {
  ifelse(age >= 55 & age <= 59, "55-59",
  ifelse(age >= 60 & age <= 64, "60-64",
  ifelse(age >= 65 & age <= 69, "65-69", NA)))
}

# 2024: the raw file wraps each finisher's name across several physical lines
# (a PDF-table-extraction artifact); using the manually-modified file, which
# has already collapsed each record to one tab-separated row (see main.R).
data2024 <- read.table("data/firecracker_10k_2024_manually_modified.txt", header=F, sep="\t", quote="", stringsAsFactors=F,
  col.names=c("Place","Bib","Initial","Full.Name","Gender","City","State","Country","Time",
              "Time2","Blank1","Age","Blank2","AgeGroupPlace","AgeGroup"))
data2024$Time <- parse_time(data2024$Time)
data2024$Year <- "2024"

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

# every year shows the same 3 age bands: 60-64 plus the one below (55-59) and
# above (65-69), so a runner's line can track them aging across bands
cols <- c("Full.Name", "Age", "Time", "Year")
combined <- rbind(data2024[data2024$Gender == "M" & data2024$Age >= 55 & data2024$Age <= 69, cols],
                   data2025[data2025$Gender == "Male" & data2025$Age >= 55 & data2025$Age <= 69, cols],
                   data2026[data2026$Gender == "Male" & data2026$Age >= 55 & data2026$Age <= 69, cols])
combined$AgeBand <- band_of_age(combined$Age)

combined <- combined %>%
  group_by(Year, AgeBand) %>%
  mutate(AgeGroupRank = rank(Time, ties.method="min")) %>%
  ungroup()

# names that raced any of these 3 bands in more than one year
year_counts <- table(unique(combined[, c("Full.Name", "Year")])$Full.Name)
repeat_names_by_years <- names(year_counts)[year_counts >= 2]

# order: the 4 runners already carried over from the earlier 2-year chart
# keep their established colors (green/magenta/violet/yellow) -- a filter
# widening the series set should never repaint the survivors. The 8 newly
# surfaced repeats (visible now that neighboring bands are included) fill the
# remaining slots, alphabetically for a reproducible order.
established <- c("Aaron Ferrucci", "Andy Olson", "David Scholar", "John Collins")
new_repeats <- sort(setdiff(repeat_names_by_years, established))
repeat_names <- c(established, new_repeats)

combined$Runner <- ifelse(combined$Full.Name %in% repeat_names, combined$Full.Name, "Other finishers")
combined$Runner <- factor(combined$Runner, levels=c(repeat_names, "Other finishers"))

field <- combined[combined$Runner == "Other finishers",]
repeats <- combined[combined$Runner != "Other finishers",]

# numeric x position: year clusters spaced widely (1, 2, 3), each split into
# 3 closely-spaced sub-columns for the age bands, so the chart reads as
# "grouped by year, then by age band" rather than 9 evenly-spaced columns.
# Field points get a further small jitter so overlapping times separate
# visually; the jitter is computed once, here, as a plain numeric column so
# the age-label layer (geom_text_repel) can anchor to the exact same spot
# (position_jitter inside geom_point has no data a separate layer can see).
year_pos <- c(`2024`=1, `2025`=2, `2026`=3)
band_offset <- c(`55-59`=-0.22, `60-64`=0, `65-69`=0.22)
set.seed(42)
field$PlotX <- year_pos[field$Year] + band_offset[field$AgeBand] + runif(nrow(field), -0.05, 0.05)
repeats$PlotX <- year_pos[repeats$Year] + band_offset[repeats$AgeBand]

# validated palette slots (dataviz skill, adjacent-pair check): established
# 4 keep their colors; the 8 new repeats take the remaining slots in an order
# chosen so no two adjacent slots are both warm earth tones (red/orange/
# brown/olive clustered badly together in testing). Note: this only clears
# the *adjacent* CVD/contrast checks, not all-pairs -- with 12 series on a
# scatter-like chart, no ordering of this many hues clears all-pairs (see
# dataviz skill palette.md); direct name labels on every line are the
# secondary encoding that keeps identity readable when two hues do collide.
runner_colors <- setNames(
  c("#008300", "#e87ba4", "#4a3aa7", "#eda100",  # established: green, magenta, violet, yellow
    "#e34948", "#2a78d6", "#eb6834", "#1baf7a",  # red, blue, orange, aqua
    "#a5651f", "#009a9d", "#8a8a1f", "#c2185b"), # brown, teal, olive, rose
  c(established, new_repeats))
runner_colors["Other finishers"] <- "#c3c2b7"

# highlight Aaron Ferrucci with a heavier line/point throughout, plus Andy
# Olson's 2025 dot, since it sits only 5 seconds from Aaron's that year
repeats$is_ferrucci <- repeats$Full.Name == "Aaron Ferrucci"
repeats$highlight_point <- repeats$is_ferrucci |
  (repeats$Full.Name == "Andy Olson" & repeats$Year == "2025")

time_ticks <- seq(40*60, 95*60, by=5*60)

# label each repeat runner's line at their own last-raced year (not every
# runner has a 2026 row in these 3 bands)
label_data <- repeats %>%
  group_by(Full.Name) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# every dot, all years and bands, gets a small muted age label anchored to
# its exact (possibly jittered) plotted position
age_labels <- rbind(field[, c("PlotX","Time","Age")], repeats[, c("PlotX","Time","Age")])

# thin dividers between year clusters reinforce the year-first grouping
year_dividers <- c(1.5, 2.5)

set.seed(42)
slope_plot <-
  ggplot() +
  geom_vline(xintercept=year_dividers, color="#e3e2d9", linewidth=0.4) +
  geom_point(data=field, aes(x=PlotX, y=Time), color="#c3c2b7", size=2, alpha=0.8) +
  geom_line(data=repeats, aes(x=PlotX, y=Time, group=Full.Name, color=Runner, linewidth=is_ferrucci)) +
  geom_point(data=repeats, aes(x=PlotX, y=Time, color=Runner, size=highlight_point)) +
  geom_text_repel(data=age_labels, aes(x=PlotX, y=Time, label=Age),
                   size=2.5, color="#898781", segment.size=0.2, segment.color="#c3c2b7",
                   box.padding=0.15, point.padding=0.05, force=0.3, max.overlaps=Inf, seed=42) +
  geom_text_repel(data=label_data, aes(x=PlotX, y=Time, label=Full.Name, color=Runner),
                   nudge_x=0.2, hjust=0, direction="y", segment.size=0.3, show.legend=F) +
  # scale_y_reverse plots smaller elapsed times higher, so "above the data"
  # means a y value *faster* than the fastest finisher, not slower
  annotate("text", x=year_pos, y=min(combined$Time) - 2*60, label=names(year_pos), fontface="bold", size=4) +
  expand_limits(y=min(combined$Time) - 2*60) +
  scale_color_manual(values=runner_colors, breaks=repeat_names, name="Repeat finishers") +
  scale_linewidth_manual(values=c(`FALSE`=0.6, `TRUE`=1.4), guide="none") +
  scale_size_manual(values=c(`FALSE`=2.5, `TRUE`=4), guide="none") +
  scale_y_reverse(breaks=time_ticks, labels=timestr(time_ticks), name="elapsed time (h:mm:ss) - faster is higher") +
  scale_x_continuous(breaks=unlist(lapply(year_pos, function(yp) yp + band_offset)),
                      labels=rep(names(band_offset), times=length(year_pos)),
                      expand=expansion(add=c(0.3, 1.0))) +
  labs(x="Age band (grouped by race year)",
       title="Firecracker 10k - Male, age 55-69",
       subtitle="Aaron Ferrucci moved from 6th to 3rd in the 60-64 age group (58:31 -> 57:08)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  guides(color=guide_legend(nrow=3))

print(slope_plot)
svg(filename="slope_male_60_64.svg", width=11, height=8)
print(slope_plot)
dev.off()
