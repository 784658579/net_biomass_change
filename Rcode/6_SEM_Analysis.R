#########SEM model------------------
rm(list = ls())
cat('\014')
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
install.packages("pacman")
library(pacman)
p_load(data.table, codyn, knitr, plyr, dplyr, lme4, lmer, lmerTest, car, tidyverse, grid, gridExtra, 
       gtable, sp, Matrix, carData, xts, zoo, ggpp, vegan, ggplot2, ggeffects, ggpmisc, cowplot, piecewiseSEM, 
       PerformanceAnalytics, lavaan,  sf, spdplyr, maps, reshape2, rgdal, rmapshaper, ggspatial, gapminder,  
       geojsonio, rnaturalearth, rnaturalearthdata, ggrepel, raster, ggsci, hrbrthemes, viridisLite, rcartocolor, socviz )

df <- fread('BC_Submit_Plot_Index2022.csv')
summary(df) 
df <- df[Biome==5 | Biome==6,]
df1 <- df
df1 <- df[IniY >1989, ]
df1.1 <- df1[, c('AGB', 'AGBGI', 'AGBM', 'Dmg_prb', 'SR', 'FDis', 'PD', 'SA', 'Cens.length', 
                 'm.PC1','m.PC2', 'n.cens')]
chart.Correlation(df1.1, histogram=TRUE, pch="+") 
#######################################################SEM1----------------
sem1 = psem(lme(AGB~, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(AGBGI~logSA+FDis+m.PC1+m.PC2+PD, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(AGBM~Dmg_prb+ logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(Dmg_prb~FDis + PD, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(PD~SR, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(FDis~SR,random = ~1|n.cens/Cens.length/FT,  data=df1),
            lme(m.PC1~logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(m.PC2~logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            m.PC2%~~% SR,
            m.PC1%~~% SR,
            m.PC1%~~% m.PC2,
            PD %~~% FDis,
            log(AGBGI) %~~% log(AGBM))

sem1 = psem(lme(FDis~ SR , random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(PD~ SR, random = ~1|n.cens/Cens.length/FT, data=df1),
            PD %~~% FDis,
            lme(Dmg_prb~ PD, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(m.PC1~logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(m.PC2~logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            m.PC2%~~% SR,
            m.PC1%~~% SR,
            m.PC1%~~% m.PC2,
            lme(scale(AGBM)~ PD + m.PC1 + m.PC2 + Dmg_prb+ logSA, random = ~1|n.cens/Cens.length/FT, data=df1),
            lme(scale(AGBGI)~FDis + m.PC1 +m.PC2 + logSA+ Dmg_prb , random = ~1|n.cens/Cens.length/FT, data=df1),
            scale(AGBGI) %~~% scale(AGBM),
            lme(scale(AGB)~ scale(AGBGI)+ scale(AGBM),random = ~1|n.cens/Cens.length/FT,  data=df1))

summary(sem1, .progressBar = F) 