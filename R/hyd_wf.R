library(dplyr)
library(lubridate)

read_seriedata <- function(file_path = "../data/seriedata.csv") {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data
}

read_seriestasjoner <- function(file_path = "../data/seriestasjoner.csv") {
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

yearly_highflow_base <- function(data) {
  result <- aggregate(dailyQ_m3s ~ stasjonsnr + year,
                      data = data, FUN = max, na.rm = TRUE)
  names(result)[names(result) == "dailyQ_m3s"] <- "highflow"
  result
}

yearly_highflow <- function(data) {
  data |> 
    group_by(stasjonsnr, year) |> 
    summarise(highflow = max(dailyQ_m3s), .groups = "drop")
}

if (FALSE){
  # test
  t0 = Sys.time(); res1 <- yearly_highflow_base(dat); t1 = Sys.time(); t1-t0
  t0 = Sys.time(); res2 <- yearly_highflow(dat)     ; t1 = Sys.time(); t1-t0
}


yearly_lowflow_base <- function(data) {
  # month <- as.integer(format(as.Date(data$date), "%m"))
  # year <- as.integer(format(as.Date(data$date), "%Y"))
  data <- as.data.frame(data)
  month <- data[["month"]]
  year <- data[["year"]]
  keep <- month %in% c(10, 11, 12, 1, 2, 3, 4)
  data <- data[keep, ]
  data$year <- ifelse(month[keep] >= 10, year[keep] + 1, year[keep])
  result <- aggregate(dailyQ_m3s ~ stasjonsnr + year,
                      data = data, FUN = quantile, probs = 0.05, na.rm = TRUE)
  names(result)[names(result) == "dailyQ_m3s"] <- "lowflow"
  result
}

yearly_lowflow <- function(data) {
  data |> 
    filter(month %in% c(10, 11, 12, 1, 2, 3, 4)) |> 
    mutate(year = ifelse(month >= 10, year+1, year)) |> 
    group_by(stasjonsnr, year) |> 
    summarise(lowflow = quantile(dailyQ_m3s, probs = 0.05), .groups = "drop")
}

if (FALSE){
  # test
  t0 = Sys.time(); res1 <- yearly_lowflow_base(dat); t1 = Sys.time(); t1-t0
  t0 = Sys.time(); res2 <- yearly_lowflow(dat)     ; t1 = Sys.time(); t1-t0
  res1 |> arrange(stasjonsnr, year) |>  head(5)
  res2 |> arrange(stasjonsnr, year) |>  head(5)
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
  combined$flood_index <- 1 - (combined$highflow_m3s - combined$highflow_mean) / combined$highflow_m3s
  combined
}

kalkuler_indikatorer <- function(seriestasjoner, flowstats, logscale = FALSE) {
  combined <- merge(seriestasjoner, flowstats, by = "stasjonsnr")
  combined$flood_index <- 1 - (combined$highflow_m3s - combined$highflow_mean) / combined$highflow_m3s
  combined$lowwater_index <- with(combined, 1-(lowflow_mean-lowflow_m3s)/(highflow_mean-lowflow_m3s))
  combined
}

plot_flood_index <- function(flood_index_data) {
  hist(log10(flood_index_data$flood_index),
       breaks = 30,
       xlab = "log10(flood_index)", main = "Distribution of 'flood_index' (log-scale)")
  abline(v = 0, col = "red", lty = 2)  # flood_u = 1: målt middel flom lik Nevina-estimat
}


plot_indikator <- function(indikator_data, indikator = 1) {
  if (indikator == 1){
  hist(indikator_data$flood_index,
       breaks = 30, log = "x",
       xlab = "flood_index", main = "Distribution of flood_index (log-scale)")
  } else {
    hist(log10(indikator_data$lowwater_index),
         breaks = 30,
         xlab = "log10(lowwater_index)", main = "Distribution of lowwater_index (log-scale)")
  }
  abline(v = 1, col = "red", lty = 2)  # indikator = 1
}


#
# data column formats ----
#

columntypes_meta <- cols(
  stasjonsnr = col_character(),
  Stasjonsnavn = col_character(),
  Måleparameter = col_character(),
  Versjon = col_double(),
  Målested = col_character(),
  Stasjontype = col_character(),
  `Status (i drift/nedlagt)` = col_character(),
  `Målestart (dato)` = col_character(),
  `Data kontrollert fra (dato)` = col_datetime(format = ""),
  `Data kontrollert til (dato)` = col_datetime(format = ""),
  `Evt. nedlagt (dato)` = col_character(),
  `Normal årsavrenning (l/s km2)` = col_double(),
  `Totalt feltareal (km2)` = col_double(),
  `Bratthet (1085-gradient, m/km)` = col_double(),
  Myrprosent = col_double(),
  `Effektiv sjøprosent` = col_double(),
  Jordbruksprosent = col_double(),
  Skogprosent = col_double(),
  Innsjøprosent = col_double(),
  Snaufjellprosent = col_double(),
  `Urbantareal prosent` = col_double(),
  Breprosent = col_double(),
  `Elvegradient (m/km)` = col_double(),
  `Elvelengde (km)` = col_double(),
  Elvenavnhierarki = col_character(),
  `Elvetetthet (m/km)` = col_double(),
  `Vassdragsomr. nr.` = col_character(),
  `Vassdragsomr. navn` = col_character(),
  `Reguleringsgrad areal` = col_double(),
  `Reguleringsgrad mag,` = col_double(),
  `Måleparameter kode` = col_double(),
  `Høyde 10 persentil (m)` = col_double(),
  `Høyde 90 persentil (m)` = col_double(),
  `Medianhøyde (m)` = col_double(),
  `Høyeste punkt (m)` = col_double(),
  `Laveste punkt (m)` = col_double(),
  `Høyde over havet (m)` = col_double(),
  ObjektID = col_double()
)

