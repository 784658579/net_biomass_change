rm(list=ls())
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
install.packages("pacman")
library(pacman)
p_load(grid, gridExtra, gtable, plyr, dplyr, data.table, tidyverse, spdplyr, maps, reshape2, ggplot2,
       sf, rgdal, sp, rmapshaper, ggspatial, gapminder,
       geojsonio, rnaturalearth, rnaturalearthdata, ggrepel, raster, cowplot,
       ggsci, hrbrthemes, viridisLite, rcartocolor, socviz)

Plot_BC <- fread("BC_Plot_AGB&Damage2022.csv")
loc <- fread("BCloc_biome&eco2022.csv") %>% na.omit() 
#colnames(loc)[c(2, 3)] <- c("LONG","LAT")
#bc <- merge(Plot_BC, loc, by=c('PLOT_ID', 'LONG','LAT')) ##uniqueN(bc$PLOT_ID) #2502
bc_loc <- merge(Plot_BC, loc[, -c('longitude','latitude')],by= "PLOT_ID") 
length(unique(bc_loc$PLOT_ID)) ## 5287 plots
## The coordiantes of some plots have redistributed in matching with Biome.
## Using bc_loc------
diff <- setdiff(loc$PLOT_ID,bc_loc$PLOT_ID ) ## This plot has been removed from Plot_BC. 
#########################################################
Div <- fread("BC_Diversity2022.csv")
FD <- fread("BC_PlotMS_Mean_FD+CWM+PC2022.csv")
PD <- fread("BC_PlotMS_phylogenetic_diversity2022.csv")
Div1 <- Div[, c("PLOT_ID", "FinY", "SR", "Srar", "Srar_PS", "H", "D")]
df1 <- merge(Div1, FD, by=c("PLOT_ID", "FinY"))
PD$SR <- NULL
PD$PLOT_MS <- NULL
PD$SR <- NULL
df2 <- merge(df1, PD, by=c("PLOT_ID", "FinY"))
df2$PLOT_MS <- NULL
df3 <- merge(bc_loc, df2,  by=c("PLOT_ID","FinY")) %>% unique()
length(unique(df3$PLOT_ID))# 5287
head(df3)
#all(df3$PLOT_MS.x==df3$PLOT_MS.y)
fwrite(df3, 'BC_Final_Combined_PLot_Index2022.csv')

################################# Plot level analysis-------------------
rm(list = ls())
BC <- fread('BC_Final_Combined_PLot_Index2022.csv')
BC1 <- BC
BC1$StemN_I[is.na(BC1$StemN_I)] <- 0
BC1$Bio_I[is.na(BC1$Bio_I)] <- 0
BC1$BA_I[is.na(BC1$BA_I)] <- 0
head(BC1)
BC1[is.na(BC1)] <- 0
BC0 <- BC %>% group_by(Prov, PLOT_ID, PLOT_MS, FT) %>% 
  dplyr::summarise(PLOT_ID=unique(PLOT_ID),
                   FT=unique(FT),
                   PS=unique(PS),
                   Origin=unique(Origin),
                   Biome=unique(Biome),
                   Ecos=unique(Ecos),
                   LAT=unique(LAT),
                   LONG=unique(LONG),
                   IniY=unique(IniY),
                   FinY=unique(FinY),
                   Cens.length=unique(Cens.length),
                   n.cens=length(unique(FinY))+1, 
                   SA=mean(Age),
                   logSA=log(SA),
                   #Total_Stem=mean(Total_Stem),
                   StemN_D=mean(StemN_D),
                   StemN_G=mean(StemN_G),
                   StemN_I=mean(StemN_I),
                   FinN=StemN_G+StemN_I,
                   BA_D=mean(BA_D),
                   BA_G=mean(BA_G),
                   BA_I=mean(BA_I),
                   FinBA=BA_G+BA_I,
                   Bio_D=mean(Bio_D),
                   Bio_G=mean(Bio_G),
                   Bio_I=mean(Bio_I),
                   FinBio=Bio_G+Bio_I, 
                   ABAGI=mean(ABAGI), 
                   ABAM=mean(ABAM), 
                   ABA=ABAGI-ABAM, 
                   AGBGI=mean(AGBGI),
                   AGBM=mean(AGBM),
                   AGB=AGBGI-AGBM,
                   Ins=mean(Ins), 
                   Dis=mean(Dis),
                   Dmg=mean(Dmg),
                   Ins_prb=mean(Ins_prob),
                   Dis_prb=mean(Dis_prob),
                   Dmg_prb=mean(Dmg_prb),
                   SR=mean(SR),
                   Srar=mean(Srar),
                   Srar_PS=mean(Srar_PS),
                   H=mean(H),
                   D=mean(D),
                   FDis=mean(FDis),
                   RaoQ=mean(RaoQ),
                   CWM.Nmass=mean(CWM.Nmass),
                   CWM.Pmass=mean(CWM.Pmass),
                   CWM.SLA=mean(CWM.SLA),
                   CWM.Amass=mean(CWM.Amass),
                   CWM.Aarea=mean(CWM.Aarea),
                   CWM.Gs=mean(CWM.Gs),
                   CWM.Ks=mean(CWM.Ks),
                   CWM.WD=mean(CWM.WD),
                   CWM.LL=mean(CWM.LL),
                   CWM.DT=mean(CWM.DT),
                   CWM.ST=mean(CWM.ST),
                   CWM.Habit=mean(CWM.Habit),
                   CWM.Struct=mean(CWM.Struct),
                   m.PC1=mean(m.PC1),
                   m.PC2=mean(m.PC2),
                   PD=mean(PD),
                   PSVs=mean(PSVs),
                   vars=mean(vars),
                   mpd=mean(mpd),
                   mntd=mean(mntd)) %>% as.data.frame()
head(BC0)
fwrite(BC0, 'BC_Submit_Plot_Index2022.csv')





