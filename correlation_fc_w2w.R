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

time_ticks_fc  <- seq(30*60, 90*60, by=10*60)
time_ticks_w2w <- seq(25*60, 75*60, by=10*60)

# validated palette (dataviz skill): 2 categorical series (year) -- well
# within the first-3-slots all-pairs-safe range for a scatter chart
year_colors <- c(`2024`="#2a78d6", `2025`="#eb6834")

set.seed(42)
corr_plot <-
  ggplot(matched, aes(x=FC.Time, y=W2W.Time)) +
  geom_smooth(method="lm", formula=y~x, color="#898781", fill="#e1e0d9", linewidth=0.6) +
  geom_point(aes(color=Year), size=2.5, alpha=0.85) +
  geom_point(data=matched[matched$is_aaron,], shape=21, size=4.5, stroke=1.2,
             color="#0b0b0b", fill=NA) +
  # Aaron's 2024 and 2025 points sit almost on top of each other -- one
  # label pointing at the pair reads more cleanly than two colliding ones
  geom_text_repel(data=matched[matched$is_aaron,][1,] %>%
                     mutate(FC.Time=mean(matched$FC.Time[matched$is_aaron]),
                            W2W.Time=mean(matched$W2W.Time[matched$is_aaron])),
                   aes(label="Aaron Ferrucci\n(2024 & 2025)"),
                   color="#0b0b0b", size=3.2, nudge_x=1100, nudge_y=550,
                   segment.size=0.3, min.segment.length=0, lineheight=0.9) +
  scale_x_continuous(breaks=time_ticks_fc, labels=timestr(time_ticks_fc), name="Firecracker 10k elapsed time (h:mm:ss)") +
  scale_y_continuous(breaks=time_ticks_w2w, labels=timestr(time_ticks_w2w), name="Wharf to Wharf (6mi) elapsed time (h:mm:ss)") +
  scale_color_manual(values=year_colors, name="Year") +
  labs(title="Firecracker 10k vs. Wharf to Wharf finish times",
       subtitle=sprintf("%d runners who ran both races the same year -- Pearson r = %.2f (p %s)",
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
