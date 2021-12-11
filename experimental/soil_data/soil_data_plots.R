# make plots

# source
source("./experiments/soil_data/soil_data_preprocessing.R")

# libs
require(gridExtra)
require(ggplot2)
require(ggmap)


# data
d <- loadSoilData()

# plots of responses
m <- match(d$geo_data$id, d$soil_data$id)

d$soil_data[,ph0_30_com:= mean(ph0_30),by="id"]
d$soil_data[,sand0_30_com:= mean(sand0_30),by="id"]
d$soil_data[,silt0_30_com:= mean(silt0_30),by="id"]

d$geo_data$ph0_30_com <- d$soil_data$ph0_30_com[m]
d$geo_data$sand0_30_com <- d$soil_data$sand0_30_com[m]
d$geo_data$silt0_30_com <- d$soil_data$silt0_30_com[m]

l <- ggplot() +
  geom_polygon(data = d$geo_data, aes( x = long, y = lat, group = group),fill="white", color="grey") +
  geom_point(data = d$soil_data, aes( x = long, y = lat)) +
  theme_void() +
  coord_map()

r1 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = ph0_30_com, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

r2 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = sand0_30_com, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

r3 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = silt0_30_com, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

png("./experiments/soil_data/Plots/PLOT_SOIL_INTRO.png", width = 700, height = 700)
grid.arrange(l, r1,
             r2, r3,
             # layout_matrix = rbind(c(1, 3, 4),
             #                       c(4, 5, 6)),
             nrow = 2, ncol=2)
dev.off()
