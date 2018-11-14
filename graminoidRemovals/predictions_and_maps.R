library(readxl)
library(cowplot)
library(wesanderson)
source('~/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/plotting_dim.R')

preds <- read_excel("~/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/predictions.xlsx", sheet = 1)
preds2 <- read_excel("~/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/predictions.xlsx", sheet = 2)


plot1 <- preds %>%
  filter(trait %in% c("richness", "height", "SLA")) %>% 
  mutate(trait = factor(trait, levels = c("richness", "height", "SLA"))) %>%
ggplot(aes(x = group, y = val, fill = trait)) +
  geom_col(position = position_dodge(width = 1), alpha = 0.7, colour = "grey30") +
  scale_fill_manual(values = wes_palette("BottleRocket2"), labels = c("Richness", "Height (mean)", "SLA (variance)")) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 1.5, alpha = 0.5, linetype = "dashed") +
  axis.dimLarge +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.6,0.8)) +
  labs(y = "-   response   +", x = "competition      facilitation")

varTriangle <- data.frame(climate = c(0.5,0.5,1.5), climVal = c(2,0,-1), trait = c(1,2,3))

predsPlot2 <- preds %>%
  mutate(group = factor(group, levels = c("facilitation", "competition"))) %>%
  filter(trait == "SLA")

plot2 <- ggplot(predsPlot2, aes(x = climate, y = climVal)) +
  geom_polygon(data = varTriangle, aes(climate, climVal), fill = "#273046", alpha = 0.55) +
  geom_line(size = 0.9, position = position_dodge(width = 0.07), colour = "#273046") +
  geom_hline(yintercept = 0, alpha = 0.3) +
  axis.dimLarge +
  theme(axis.text = element_blank(),
        legend.position = "none") +
  labs(x = "low abiotic stress    high abiotic stress", y = "-   response   +")


plot3 <- ggplot(preds2, aes(x = time, y = val, colour = group, linetype = group)) +
  geom_smooth(se = FALSE, span = 0.7) +
  #geom_line() +
  scale_colour_manual(values = wes_palette("Cavalcanti1")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0.2, linetype = "dashed") +
  axis.dimLarge +
  theme(axis.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.55,0.85)) +
  labs(y = "-   response   +")



### SITE COORDINATES
siteCoord <- tbl(con, "sites") %>% 
  select(siteID, latitude, longitude, Temperature_level, Precipitation_level) %>% 
  collect() %>% 
  rename(siteID = siteID, lat = latitude, lon = longitude, temp = Temperature_level, precip = Precipitation_level) %>% 
  mutate(temp = recode(temp, "1" = "boreal", "2" = "sub-alpine", "3" = "alpine"))

library("raster")
library("ggthemes")
library('osmar')
library("grid")
library("ggsn")

### COORDINATES FIELD SITES
srtmTEMP <- getData('worldclim', var = "bio", res = 0.5, lon=3, lat=61)
srtmALT <- getData('alt', country = "NOR", res = 0.5, lon=3, lat=61)
norgeOutline <- getData("GADM", country = "NOR", level = 0)
e <- extent(4.9, 9.5, 60.25, 61.5)
clipped <- crop(srtmTEMP, e)
clipped2 <- crop(clipped, norgeOutline)
dem.p  <-  rasterToPoints(clipped2)
dem.f <- data.frame(dem.p)
dem.f <- dem.f %>% 
  mutate(MAT = bio1_06/10, AnnPrecip = bio13_06)


sognMapPRECIP <- fortify(siteCoord) %>% 
  ggplot() +
  geom_raster(data = dem.f, aes(x, y, fill = bio13_06)) +
  geom_point(aes(x = lon, y = lat, shape = temp), fill = "grey90", colour = "darkslategrey", stroke = 2, size = 2.2, position = position_jitter(width = 0, height = 0.02)) +
  scale_shape_manual("", values = c(25, 24, 21)) +
  scale_fill_gradientn("", colours = c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e")) +
  theme_map(base_family = "Helvetica") +
  theme(axis.text=element_text(size=10),
        legend.text = element_text(size=12),
        legend.title = element_text(size=15),
        legend.background = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0, 1, 0, 0), "cm")) +
  coord_quickmap()

sognMapTEMP <- fortify(siteCoord) %>% 
  ggplot() +
  geom_raster(data = dem.f, aes(x, y, fill = MAT)) +
  geom_point(aes(x = lon, y = lat, shape = temp), fill = "grey90", colour = "darkslategrey", stroke = 2, size = 2.2, position = position_jitter(width = 0, height = 0.02)) +
  scale_shape_manual("", values = c(25, 24, 21), guide = "none") +
  scale_fill_distiller("", palette = "RdBu") +
  theme_map(base_family = "Helvetica") +
  theme(axis.text=element_text(size=10),
        legend.text = element_text(size=12),
        legend.title = element_text(size=15),
        legend.background = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0, 1, 0, 0), "cm")) +
  coord_quickmap()


climMap <- plot_grid(sognMapTEMP, sognMapPRECIP, ncol = 2, labels = c("A", "B"), rel_widths = c(1, 0.9))

### NORWAY MAP
#### Get Wolrdclim elevation data ####
elev <- getData('worldclim', var = "alt", res = 2.5)
e <- extent(3,20,56,70)
elev.china <- crop(elev, e)

# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
elev.norway.spdf <- as(elev.china, "SpatialPixelsDataFrame")
elev.norway.df <- as.data.frame(elev.norway.spdf)

square = data.frame(x = c(4.5, 9, 9, 4.5),
                    y = c(60.5, 60.5, 61.6, 61.6))

# plot China map
border <- map_data("world") %>% 
  filter(region == "Norway")

norgeKart <- ggplot() +
  #geom_raster(data = elev.china.df, aes(x=x, y=y, fill = alt)) +
  geom_map(aes(map_id = region, fill = region), data = border, map = border, color = "grey20") +
  geom_polygon(data = square, aes(x = x, y = y), color = "darkslategrey", size = 2, fill = NA) +
  scale_x_continuous(expand = c(0,0), limits = c(3, 31)) +
  scale_y_continuous(expand = c(0,0), limits = c(58, 71.5)) +
  coord_quickmap() +
  scale_fill_manual(limits=c("Norway", "Sweden"), values = c("grey90", "grey96")) + 
  labs(x = "", y = "") +
  theme_map(base_family = "Helvetica") +
  theme(axis.text=element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
#theme_map(base_size = 10) +
#theme(legend.position = c(0,1), legend.justification = c(0,1))

ggsave(norgeKart, filename = "norgeKart.png", dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures", height = 5, width = 4, bg = "transparent") # bg = transparent so that the plot background is see-through

maps <- plot_grid(climMap, norgeKart, nrow = 1, rel_widths = c(2,0.5))

predsGath <- plot_grid(plot1, plot2, plot3, labels = c('C', 'D', 'E'), nrow = 1, align = 'v')
fig1MapPred <- plot_grid(maps, predsGath, nrow = 2, rel_widths = c(1.5, 0.7))

ggsave(fig1MapPred, filename = "fig1MapPreds_v3.jpg", height = 7, width = 13, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")




#### map of Europe ####
mapworld <- map_data("world")
mapworld %>% 
  ggplot() +
  #geom_raster(data = elev.china.df, aes(x=x, y=y, fill = alt)) +
  geom_map(aes(map_id = region, fill = region), map = mapworld, color = "grey70", fill = "grey90") +
  scale_x_continuous(expand = c(0,0), limits = c(-10, 50)) +
  scale_y_continuous(expand = c(0,0), limits = c(40, 72)) +
  coord_quickmap() +
  labs(x = "", y = "") +
  theme_map(base_family = "Helvetica") +
  theme(axis.text=element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  ggsave(filename = "~/Documents/seedclimComm/figures/mapEurope.jpg")

# climate space map
my.GR.data %>% 
  mutate(precip = factor(precip, levels = c(600, 1200, 2000, 2700), labels = c("dry", " ", "  ", "wet")), temp = factor(temp, levels = c(6.5, 8.5, 10.5), labels = c("alpine", "sub-alpine", "boreal"))) %>% 
  ggplot(aes(x = Precipitation_level, y = Temperature_level, fill = precip, shape = temp)) +
  geom_point(size = 5) +
  scale_fill_manual(legend.title.climate, values = cbPalette[c(7,2,3,6)]) +
  scale_shape_manual(legend.title.climate, values = c(24,21,25)) +
  scale_y_reverse(lim = c(14, 4)) +
  xlim(c(300, 3400)) +
  labs(x = "Annual precipitation (mm)", y = "Mean summer temperature (°C)") +
  axis.dimLarge +
  theme(legend.position = "none")
ggsave(filename = "~/Documents/seedclimComm/figures/climSpace.png", dpi = 300, height = 4, width = 8.5, bg = "transparent")
