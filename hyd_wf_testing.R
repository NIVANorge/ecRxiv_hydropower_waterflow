
source("R/hyd_wf.R")

# read data and station data (the latter includes NEVINA calculations)  
dat <- read_seriedata("data/seriedata.csv")
stations_orig <- read_seriestasjoner("data/seriestasjoner.csv")

# add degree of hydropower dams to metadata
stations_meta <- readr::read_csv("data/seriestasjoner_meta.csv")
stations_meta$regulgrad_mag <- as.numeric(stations_meta$`Reguleringsgrad mag,`)
stations_meta$`Reguleringsgrad mag,` <- NULL
stations <- merge(stations_orig, subset(stations_meta, select = c(stasjonsnr, regulgrad_mag)), by = "stasjonsnr") 
# str(stations)

# get observed means 
flow_high <- yearly_highflow(dat)
flow_winter <- yearly_lowflow(dat)
flow_statistics <- flow_stats(flow_winter, flow_high)

# floods: plot observed vs. NEVINA values
plot_highflow(flowstats = flow_statistics, seriestasjoner = stations)
plot_highflow(flowstats = flow_statistics, seriestasjoner = stations, logscale = TRUE)

# low water: plot observed vs. NEVINA values
plot_lowflow(flowstats = flow_statistics, seriestasjoner = stations)
plot_lowflow(flowstats = flow_statistics, seriestasjoner = stations, logscale = TRUE)

# indicator 001: effect of hydropower on floods  
index <- flom_indikator(flowstats = flow_statistics, seriestasjoner = stations)
plot_flom_indikator(index)

# indicator 001: effect of hydropower on floods and low water  
# indicator 002, preliminary (unadjusted) version: effect of hydropower on low water  
dat_indekser <- kalkuler_indikatorer(flowstats = flow_statistics, seriestasjoner = stations)
plot_indikator(dat_indekser, 1)
plot_indikator(dat_indekser, 2)

# plot flood index  
hist(dat_indekser$flom_indikator, breaks = 100, xlim = c(0,2.5))
abline(v = 1, col = "red")
abline(v = mean(index$flom_indikator), col = "blue")
abline(v = exp(mean(log(index$flom_indikator))), col = "purple")
abline(v = median(index$flom_indikator), col = "green")

# plot temporary low water index  
hist(dat_indekser$lavvann_indikator_prelim, breaks = 100, xlim = c(0,2.5))
abline(v = 1, col = "red")
abline(v = mean(dat_indekser$lavvann_indikator_prelim, na.rm = TRUE), col = "blue")
abline(v = exp(mean(log(dat_indekser$lavvann_indikator_prelim))), col = "purple")
abline(v = median(dat_indekser$lavvann_indikator_prelim), col = "green")

# adjust low water index
plot(dat_indekser$regulgrad_mag, dat_indekser$lavvann_indikator_prelim)
plot(dat_indekser$regulgrad_mag, dat_indekser$lavvann_indikator_prelim, log = "y")

# option 1: regression
dat_indekser$lavvann_indikator_log <- log(dat_indekser$lavvann_indikator_prelim)
mod <- lm(lavvann_indikator_log ~ regulgrad_mag, data = dat_indekser)
plot(dat_indekser$regulgrad_mag, dat_indekser$lavvann_indikator_log)
abline(mod$coefficients, col = "red")

# option 2: select regulgrad_mag == 0 and take the mean og log-transformed values
sel <- dat_indekser$regulgrad_mag == 0
adjustment_factor <- exp(mean(dat_indekser$lavvann_indikator_log[sel]))
adjustment_factor  # 0.5489

# adjustment
dat_indekser$lavvann_indikator <- adjustment_factor*dat_indekser$lavvann_indikator_prelim

# plot finale low water index  
hist(dat_indekser$lavvann_indikator, breaks = 100, xlim = c(0,2.5))
abline(v = 1, col = "red")
abline(v = mean(dat_indekser$lavvann_indikator_prelim, na.rm = TRUE), col = "blue")
abline(v = exp(mean(log(dat_indekser$lavvann_indikator_prelim))), col = "purple")
abline(v = median(dat_indekser$lavvann_indikator_prelim), col = "green")









