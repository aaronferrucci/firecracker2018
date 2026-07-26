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

# --- Firecracker 10k ---------------------------------------------------

fc2024 <- read.table("data/firecracker_10k_2024_manually_modified.txt", header=F, sep="\t", quote="", stringsAsFactors=F,
  col.names=c("Place","Bib","Initial","Full.Name","Gender","City","State","Country","Time",
              "Time2","Blank1","Age","Blank2","AgeGroupPlace","AgeGroup"))
fc2024$Time <- parse_time(fc2024$Time)
fc2024$Year <- "2024"

fc2025 <- read.table("data/firecracker_10k_2025.txt", header=F, sep="\t", quote="", stringsAsFactors=F,
  col.names=c("Place","Bib","Full.Name","Gender","Age","City","State","Time",
              "GenderPlace","AgeGroup","AgeGroupPlace","Pace","Blank1","Blank2"))
fc2025$Time <- parse_time(fc2025$Time)
fc2025$Year <- "2025"

fc_cols <- c("Full.Name", "Time", "Year")
fc <- rbind(fc2024[, fc_cols], fc2025[, fc_cols])
names(fc)[names(fc) == "Time"] <- "FC.Time"

# --- Wharf to Wharf ------------------------------------------------------

# 2024: only two age bands were scraped (M50-59, since that's Aaron's band
# that year, and M20-29, Ian's) -- not the full field. This caps how many
# 2024 Firecracker finishers can possibly match.
w2w2024a <- read.table("data/wharf2wharf_2024_m50_59.txt", header=T, sep="\t", quote="", stringsAsFactors=F)
w2w2024b <- read.table("data/wharf2wharf_2024_m20_29.txt", header=T, sep="\t", quote="", stringsAsFactors=F)
w2w2024 <- rbind(w2w2024a, w2w2024b)
# a handful of records are DNS/DNF (no elapsed time) or have a malformed
# Chip.Elapsed.Time from missing upstream fields shifting columns -- drop
# anything that isn't a clean h:mm:ss or mm:ss value
valid_time <- function(x) grepl("^[0-9]+:[0-9]{2}(:[0-9]{2})?$", x)

w2w2024 <- w2w2024[valid_time(w2w2024$Chip.Elapsed.Time),]
w2w2024$Time <- parse_time(w2w2024$Chip.Elapsed.Time)
w2w2024$Year <- "2024"

# 2025: full field
w2w2025 <- read.table("data/wharf2wharf_2025.txt", header=T, sep="\t", quote="", stringsAsFactors=F, fill=T)
w2w2025 <- w2w2025[valid_time(w2w2025$Chip.Elapsed.Time),]
w2w2025$Time <- parse_time(w2w2025$Chip.Elapsed.Time)
w2w2025$Year <- "2025"

w2w_cols <- c("Full.Name", "Time", "Year")
w2w <- rbind(w2w2024[, w2w_cols], w2w2025[, w2w_cols])
names(w2w)[names(w2w) == "Time"] <- "W2W.Time"

# --- Match by name + year -------------------------------------------------

# Firecracker 10k (~10km) and Wharf to Wharf (6mi/~9.66km) are close enough
# in distance that raw elapsed time, not pace, is a reasonable proxy for
# fitness -- no unit conversion needed for a first-cut correlation.
matched <- inner_join(fc, w2w, by=c("Full.Name", "Year"))

n <- nrow(matched)
corr <- cor.test(matched$FC.Time, matched$W2W.Time)
cat(sprintf("Matched runners: %d (%d in 2024, %d in 2025)\n", n,
            sum(matched$Year == "2024"), sum(matched$Year == "2025")))
cat(sprintf("Pearson r = %.3f (95%% CI %.3f-%.3f), p = %.2e\n",
            corr$estimate, corr$conf.int[1], corr$conf.int[2], corr$p.value))

matched$is_aaron <- matched$Full.Name == "Aaron Ferrucci"

# --- Aaron's 2026 point (Firecracker field is scraped in full; 2026 Wharf to
# Wharf isn't -- only Aaron's own result is recorded, in
# wharf2wharf_2026_aaron.txt) -- so 2026 can't join the population
# correlation above, but his point is still worth showing on the chart.
fc2026 <- read.table("data/firecracker_10k_2026_manually_modified.txt", header=T, sep="\t", quote="", stringsAsFactors=F,
  col.names=c("Race.Place","Bib","Full.Name","Gender","Age","Gun.Start","Time","Finish.Clock","GenderPlace","AgeGroup"))
fc2026$Time <- parse_time(fc2026$Time)

w2w2026_aaron <- read.table("data/wharf2wharf_2026_aaron.txt", header=T, sep="\t", quote="", stringsAsFactors=F, na.strings="NA")
w2w2026_aaron$Time <- parse_time(w2w2026_aaron$Chip.Elapsed.Time)

aaron_2026 <- data.frame(
  Full.Name="Aaron Ferrucci",
  Year="2026",
  FC.Time=fc2026$Time[fc2026$Full.Name == "Aaron Ferrucci"],
  W2W.Time=w2w2026_aaron$Time[w2w2026_aaron$Full.Name == "Aaron Ferrucci"],
  is_aaron=TRUE)

time_ticks_fc  <- seq(30*60, 90*60, by=10*60)
time_ticks_w2w <- seq(25*60, 75*60, by=10*60)

# validated palette (dataviz skill): 3 categorical series (year) -- within
# the first-3-slots all-pairs-safe range for a scatter chart
year_colors <- c(`2024`="#2a78d6", `2025`="#eb6834", `2026`="#1baf7a")

# Aaron's 2024/2025/2026 points, combined for the highlight ring + label --
# all three sit close together, so one label anchored at their centroid
# reads more cleanly than three (or even two) colliding ones
aaron_all <- rbind(matched[matched$is_aaron, c("Full.Name","Year","FC.Time","W2W.Time")],
                    aaron_2026[, c("Full.Name","Year","FC.Time","W2W.Time")])

set.seed(42)
corr_plot <-
  ggplot(matched, aes(x=FC.Time, y=W2W.Time)) +
  geom_smooth(method="lm", formula=y~x, color="#898781", fill="#e1e0d9", linewidth=0.6) +
  geom_point(aes(color=Year), size=2.5, alpha=0.85) +
  geom_point(data=aaron_2026, aes(color=Year), size=2.5, alpha=0.85) +
  geom_point(data=aaron_all, shape=21, size=4.5, stroke=1.2,
             color="#0b0b0b", fill=NA) +
  geom_text_repel(data=aaron_all[1,] %>%
                     mutate(FC.Time=mean(aaron_all$FC.Time),
                            W2W.Time=mean(aaron_all$W2W.Time)),
                   aes(label="Aaron Ferrucci\n(2024-2026)"),
                   color="#0b0b0b", size=3.2, nudge_x=1100, nudge_y=550,
                   segment.size=0.3, min.segment.length=0, lineheight=0.9) +
  scale_x_continuous(breaks=time_ticks_fc, labels=timestr(time_ticks_fc), name="Firecracker 10k elapsed time (h:mm:ss)") +
  scale_y_continuous(breaks=time_ticks_w2w, labels=timestr(time_ticks_w2w), name="Wharf to Wharf (6mi) elapsed time (h:mm:ss)") +
  scale_color_manual(values=year_colors, name="Year") +
  labs(title="Firecracker 10k vs. Wharf to Wharf finish times",
       subtitle=sprintf("%d runners who ran both races the same year, 2024-2025 -- Pearson r = %.2f (p %s); 2026 shows Aaron's result only",
                         n, corr$estimate, ifelse(corr$p.value < 0.001, "< 0.001", sprintf("= %.3f", corr$p.value)))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(corr_plot)
svg(filename="correlation_fc_w2w.svg", width=9, height=7)
print(corr_plot)
dev.off()

# --- Aaron's own trajectory, 2024-2026 ------------------------------------

# 2026 Wharf to Wharf can't join the population correlation above (only
# Aaron's own result was scraped), but his point (already loaded as
# aaron_2026, above) still lets us track him against the population model.
aaron <- aaron_all[order(aaron_all$Year), c("Year", "FC.Time", "W2W.Time")]

model <- lm(W2W.Time ~ FC.Time, data=matched)
aaron$pop_pred <- predict(model, newdata=aaron)
aaron$resid_sec <- aaron$W2W.Time - aaron$pop_pred

# personal offset, estimated from years with actual W2W results only (2024-2025)
personal_offset <- mean(aaron$resid_sec[aaron$Year %in% c("2024", "2025")])
aaron$personal_pred <- aaron$pop_pred + personal_offset

cat("\nAaron Ferrucci, Firecracker 10k vs Wharf to Wharf, 2024-2026:\n")
for (i in seq_len(nrow(aaron))) {
  cat(sprintf("  %s  FC %s  W2W %s  (population-model pred %s, personal-offset pred %s)\n",
              aaron$Year[i], timestr(aaron$FC.Time[i]), timestr(aaron$W2W.Time[i]),
              timestr(aaron$pop_pred[i]), timestr(aaron$personal_pred[i])))
}
# 2026 actual (48:39) landed close to the personal-offset estimate made ahead
# of the race (49:18), and well inside the population model's error band --
# validates using Aaron's own historical residual as a correction term.
