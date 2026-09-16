
dat <- read_seriedata()
stations <- read_seriestasjoner()
station_sample <- sample(stations$stasjonsnr, 12)

flow_high <- yearly_highflow(dat)
flow_winter <- yearly_lowflow(dat)
flow_statistics <- flow_stats(flow_winter, flow_high)

plot_highflow(flowstats = flow_statistics, seriestasjoner = stations)
plot_highflow(flowstats = flow_statistics, seriestasjoner = stations, logscale = TRUE)

plot_lowflow(flowstats = flow_statistics, seriestasjoner = stations)
plot_lowflow(flowstats = flow_statistics, seriestasjoner = stations, logscale = TRUE)

index <- flom_indikator(flowstats = flow_statistics, seriestasjoner = stations)
plot_flom_indikator(index)




