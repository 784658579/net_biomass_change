###########################################################################Spatial distribution------------
rm(list = ls())
cat("\014")
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
df <- fread("BC_Submit_Plot_Index2022.csv")[, c("PLOT_ID","Biome", "LAT", "LONG")] %>%
  filter(Biome %in% c(5, 6)) %>% unique()
df$Biome <- as.factor(df$Biome)
summary(df)
can<-getData('GADM', country="CAN", level=1) # provinces
can
bc <- can[can$NAME_1 %in% c("British Columbia"),]
coordinates(df) = ~ LONG + LAT
proj4string(df) <- st_crs(can)$proj4string
can <- st_as_sf(can)
df <- st_as_sf(df)
bc <- st_as_sf(bc)
##########Plot distribution------------
gca <- ggplot(data = can) +
  geom_sf(fill="antiquewhite1")+
  geom_rect(xmin = -140, xmax = -113, ymin = 48, ymax = 60.5,
            fill = NA, colour = "red", size = 0.7) +
  theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA))
gca

gbc <- ggplot(data = bc)+
  geom_sf(fill="antiquewhite1")+
  geom_sf(data = df, aes(col=Biome, fill=Biome), size=1.5)+
  scale_color_manual(name="Biome",
                     breaks=c(5, 6), 
                     labels=c("Temperate", "Boreal"),
                     values = c("forestgreen","red"))+
  scale_fill_manual(name="Biome",
                    breaks=c(5, 6), 
                    labels=c("Temperate", "Boreal"),
                    values = c("forestgreen","red"))+
  annotate(geom = "text", x = -126, y = 58, label = "British Bolumbia",
           fontface = "italic", color = "grey22", size = 3.5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude")+ ylab("Latitude")+
  theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA))+
  theme(legend.position = "top")
gbc

arrowA <- data.frame(x1 = 14, x2 = 22.5, y1 = 8.5, y2 = 8.5)
g1 <- ggplot() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(gbc), xmin = 0, xmax = 20, ymin = 0, 
                    ymax = 20) +
  annotation_custom(ggplotGrob(gca), xmin = 20, xmax = 28, ymin = 4, 
                    ymax = 14.5) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round") +
  theme_void()
g1
tiff('Boreal and Temperate Forest plots distrition in BC.tiff', units="in", width=10, height=7, res=600, compression = 'lzw')
g1
dev.off()
