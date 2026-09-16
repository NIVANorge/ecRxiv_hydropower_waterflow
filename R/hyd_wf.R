library(dplyr)

read_seriedata <- function(file_path = "data/seriedata.csv") {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data
}

read_seriestasjoner <- function(file_path = "data/seriestasjoner.csv") {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data <- data %>% filter(lowflow_m3s > 0)

  normalavrenning <- data$normalQ_m3s / data$areal_km2 * 1000
  outside_nifs_range <- data$areal_km2 > 53 |
    normalavrenning > 163 |
    data$sjo_prosent > 21
  data$highflow_m3s <- ifelse(outside_nifs_range,
                              data$rffa_high_m3s,
                              data$nifs_high_m3s)
  data
}

yearly_highflow <- function(data) {
  data$year <- as.integer(format(as.Date(data$date), "%Y"))
  result <- aggregate(dailyQ_m3s ~ stasjonsnr + year,
                      data = data, FUN = max, na.rm = TRUE)
  names(result)[names(result) == "dailyQ_m3s"] <- "highflow"
  result
}

yearly_lowflow <- function(data) {
  month <- as.integer(format(as.Date(data$date), "%m"))
  year <- as.integer(format(as.Date(data$date), "%Y"))
  keep <- month %in% c(10, 11, 12, 1, 2, 3, 4)
  data <- data[keep, ]
  data$year <- ifelse(month[keep] >= 10, year[keep] + 1, year[keep])
  result <- aggregate(dailyQ_m3s ~ stasjonsnr + year,
                      data = data, FUN = quantile, probs = 0.05, na.rm = TRUE)
  names(result)[names(result) == "dailyQ_m3s"] <- "lowflow"
  result
}

flow_stats <- function(lowflow, highflow) {
  combined <- merge(lowflow, highflow, by = c("stasjonsnr", "year"))

  combined %>%
    group_by(stasjonsnr) %>%
    summarise(
      lowflow_mean = mean(lowflow, na.rm = TRUE),
      lowflow_sd = sd(lowflow, na.rm = TRUE),
      highflow_mean = mean(highflow, na.rm = TRUE),
      highflow_sd = sd(highflow, na.rm = TRUE),
      .groups = "drop"
    )
}

plot_lowflow <- function(seriestasjoner, flowstats, logscale = FALSE) {
  combined <- merge(seriestasjoner, flowstats, by = "stasjonsnr")
  plot(combined$lowflow_m3s, combined$lowflow_mean,
       xlab = "lowflow_m3s (Nevina)", ylab = "lowflow_mean (Målt)",
       main = "Nevina lowflow vs. målt middel lavvann", log = ifelse(logscale,"xy",""))
  abline(0, 1, col = "red", lty = 2)  # 1:1 reference line
}

plot_highflow <- function(seriestasjoner, flowstats, logscale = FALSE) {
  combined <- merge(seriestasjoner, flowstats, by = "stasjonsnr")
  plot(combined$highflow_m3s, combined$highflow_mean,
       xlab = "highflow_m3s (Nevina)", ylab = "highflow_mean (Målt)",
       main = "Nevina highflow vs. målt middel flom", log = ifelse(logscale,"xy",""))
  abline(0, 1, col = "red", lty = 2)  # 1:1 reference line
}

flom_indikator <- function(seriestasjoner, flowstats, logscale = FALSE) {
  combined <- merge(seriestasjoner, flowstats, by = "stasjonsnr")
  combined$flom_indikator <- 1 - (combined$highflow_m3s - combined$highflow_mean) / combined$highflow_m3s
  combined
}

plot_flom_indikator <- function(flom_indikator_data) {
  hist(log10(flom_indikator_data$flom_indikator),
       breaks = 30,
       xlab = "log10(flom_indikator)", main = "Fordeling av flom_indikator (log-skala)")
  abline(v = 0, col = "red", lty = 2)  # flom_indikator = 1: målt middel flom lik Nevina-estimat
}
