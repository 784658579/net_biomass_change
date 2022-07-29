cat('\014')
rm(list = ls())
install.packages(c("devtools", "raster", "data.table", "picante", "nlme", "usethis", "sp", "ape",
                   "vegan", "permute", "lattice", "sf", "rgdal", "maptools"))
install.packages("remotes")
remotes::install_github("azizka/speciesgeocodeR")

library(devtools);library(raster); library(data.table); library(picante); library(nlme)
library(usethis); library(sp); library(ape); library(vegan); library(permute); library(lattice)
library(rgeos);library(rgdal);library(speciesgeocodeR);library(maptools);library(sf)

#### derive biome from wwf shapefile----
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
wwf <- readOGR("wwf_terr_ecos.shp")
geo.proj <- proj4string(wwf)
loc <- fread("BCloc.csv") %>% as.data.table()
names(loc) <- c("PLOT_ID", "longitude", "latitude")
pts <- SpatialPoints(cbind(loc$longitude, loc$latitude), proj4string = crs(geo.proj))
plot.biome <- over(pts, wwf)[["BIOME"]]
plot.eco <- over(pts, wwf)[['ECO_NAME']]
biome.list <- unique(plot.biome)
eco.list <- unique(plot.eco)
biome.subset <- wwf[wwf$BIOME %in% biome.list,]
biome.na <- which(is.na(plot.biome))
## the min distance or length from the vertices of points(pts) to vertices of polygons(wwf)
gDist <- gDistance(pts[biome.na, ], biome.subset, byid = T)
na.mindist <- apply(gDist, 2, which.min)
##reassign biome to NA plots 
plot.biome[biome.na] <- biome.subset$BIOME[na.mindist]
unique(plot.biome)

eco.subset <- wwf[wwf$ECO_NAME %in% eco.list, ]
eco.na <- which(is.na(plot.eco))
gDist <- gDistance(pts[eco.na, ], eco.subset, byid = T)
na.mindist <- apply(gDist, 2, which.min)
plot.eco[eco.na] <- eco.subset$ECO_NAME[na.mindist]
unique(plot.eco)

Plot_biome_eco <- cbind(loc, plot.biome, plot.eco) %>% as.data.frame()
names(Plot_biome_eco)[4] <- "Biome"
names(Plot_biome_eco)[5] <- 'Ecos'
###Biome names add later, 5: Temperate Conifer, 8: Temperate grasslands, savannas and shrubs, 6: Boreal/Taiga, 11: Tundra
fwrite(Plot_biome_eco, "BCloc_biome&eco2022.csv")

##### get climate weather factors-----
######### get annual temp and precipitation----
rm(list = ls())
cat("\014")
install.packages("raster")
library(raster)
install.packages("sp")
library(sp)
climate <- getData('worldclim', var='bio', res=2.5)
setwd("W:/My Drive/PSP/PSP updata2022/BC")
loc <- fread("BCloc_Biome.csv")
library(utils)
xy <- loc[,c("longitude", "latitude")]
sp_xy <- SpatialPoints(xy, proj4string = climate@crs)
sp_xy_cli <- extract(climate, sp_xy)
sp_loc_cli <- cbind(loc, sp_xy_cli)
sp_loc_cli_na <- sp_loc_cli[is.na(bio1),]
## The 7 NA plots are good to remove. BC_4016589_1 located in the river, other 6 located beyond the isolated island. 
##I save file with NAs. And will edit the bio after. 
##BIO1 = Annual Mean Temperature (°C * 10) -- (i.e. 231 represents 23.1 °C)
##BIO12 = Annual Precipitation (mm)
fwrite(sp_loc_cli, "BCloc_Biome_clibio.csv")











