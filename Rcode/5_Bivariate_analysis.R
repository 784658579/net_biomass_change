rm(list = ls())
cat('\014')
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
install.packages("pacman")
library(pacman)
p_load(data.table, codyn, knitr, plyr, dplyr, lme4, lmer, lmerTest, car, tidyverse, grid, gridExtra, 
       gtable, sp, Matrix, carData, xts, zoo, ggpp, vegan, ggplot2, ggeffects, ggpmisc, cowplot, piecewiseSEM, 
       PerformanceAnalytics, lavaan,  sf, spdplyr, maps, reshape2, rgdal, rmapshaper, ggspatial, gapminder,  
       geojsonio, rnaturalearth, rnaturalearthdata, ggrepel, raster, ggsci, hrbrthemes, viridisLite, rcartocolor, socviz )

install.packages('ggpmisc')
library(ggpmisc)
library(ggpp)

df <- fread('BC_Submit_Plot_Index2022.csv')
summary(df) 
df <- df[Biome==5 | Biome==6,]
df1 <- df
df1 <- df[IniY >1989, ]
stat_fit_alter <- stat_fit_glance(method = 'lm',
                                  method.args = list(formula = y ~ x),
                                  label.x = "right", #added to prevent overplotting
                                  vstep = 0.1,      # sets vertical spacing
                                  mapping = aes(label = sprintf('italic(r)^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                                stat(r.squared), stat(p.value))), parse = TRUE)


#####################biodiversity ~ damage------------
pp1a<-ggplot(df1, aes(x=SR, y=log(Dmg_prb+1) , group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  xlab(" ")+ ylab('Damage probability')+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  theme_classic()+theme(legend.position = "bottom")

pp1a 

pp1b<-ggplot(df1, aes(x=FDis, y=log(Dmg_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp1b

pp1c<-ggplot(df1, aes(x=log(PD+1), y=log(Dmg_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp1c

pp1d<-ggplot(df1, aes(x=SR, y=log(Ins_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('Insect attack probability')  + 
  theme_classic()+ theme(legend.position = "none")

pp1d

pp1e<-ggplot(df1, aes(x=FDis, y=log(Ins_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp1e

pp1f<-ggplot(df1, aes(x=log(PD+1), y=log(Ins_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp1f

pp1g<-ggplot(df1, aes(x=SR, y=log(Dis_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Species richness")+ ylab('Disease pressure prbability')+
  theme_classic()+theme(legend.position = "none")
pp1g

pp1h<-ggplot(df1, aes(x=FDis, y=log(Dis_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Functional diversity")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp1h

pp1i<-ggplot(df1, aes(x=log(PD+1), y=log(Dis_prb+1), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Phylogenetic diversity")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp1i

# get legend from pp2a
legend_b <- get_legend(pp1a)

# remove legend of pp2a
pp1a <- pp1a + theme(legend.position = 'none')

Fig1<-plot_grid( pp1a, pp1b,pp1c, pp1d, pp1e, pp1f,pp1g, pp1h,pp1i, labels = c('a', 'b','c', 'd','e', 'f', 'g', 'h', 'i'), 
                 label_size = 12,align = "hv", ncol=3, vjust = 2)

p <- plot_grid(Fig1, legend_b, ncol = 1,rel_heights = c(1, 0.05) )

tiff('1Bivariate damage.tiff', units="in", width=14, height=13, res=800, compression = 'lzw')
p
dev.off()      

m1<-lm(log(Dmg_prb+1)~log(PD+1)+ FT, df1)
summary(m1)
m1.1 <- lm(log(Dmg_prb+1)~log(PD+1) * FT, df1)
summary(m1.1)
op <- par(mfrow=c(2,2))
plot(m1)
plot(m1.1)
par(op)
########################## By forest type---------------
pp2a<-ggplot(df, aes(x=SR, y=log(AGB) , group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  xlab(" ")+ ylab(expression(paste("Net biomass change (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  theme_classic()+theme(legend.position = "bottom")

pp2a 

pp2b<-ggplot(df, aes(x=FDis, y=log(AGB), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp2b

pp2c<-ggplot(df, aes(x=PD, y=log(AGB), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp2c

pp2d<-ggplot(df, aes(x=SR, y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(expression(paste("Growth (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))  + 
  theme_classic()+ theme(legend.position = "none")

pp2d

pp2e<-ggplot(df, aes(x=FDis, y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp2e

pp2f<-ggplot(df, aes(x=PD, y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp2f

pp2g<-ggplot(df, aes(x=SR, y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Species richness")+ ylab(expression(paste("Mortality (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))+
  theme_classic()+theme(legend.position = "none")
pp2g

pp2h<-ggplot(df, aes(x=FDis, y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Functional diversity")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp2h

pp2i<-ggplot(df, aes(x=PD, y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Phylogenetic diversity")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp2i

# get legend from pp2a
legend_b <- get_legend(pp2a)

# remove legend of pp2a
pp2a <- pp2a + theme(legend.position = 'none')

Fig2<-plot_grid( pp2a, pp2b,pp2c, pp2d, pp2e, pp2f,pp2g, pp2h,pp2i, labels = c('a', 'b','c', 'd','e', 'f', 'g', 'h', 'i'), 
                 label_size = 12,align = "hv", ncol=3, vjust = 2)

p <- plot_grid(Fig2, legend_b, ncol = 1,rel_heights = c(1, 0.05) )

tiff('2Bivariate biomass.tiff', units="in", width=14, height=13, res=800, compression = 'lzw')
p
dev.off()      

m2<-lm(log(AGB)~scale(SR)+ FT, df)
summary(m2)
m2.1 <- lm(log(AGB)~SR * FT, df)
summary(m2.1)
op <- par(mfrow=c(2,2))
plot(m2)
plot(m2.1)
par(op)


###########################Biomass~ damage-----------
pp3a<-ggplot(df1, aes(x=log(Dmg_prb+1), y= log(AGB) , group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  xlab(" ")+ ylab(expression(paste("Net biomass change (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  theme_classic()+theme(legend.position = "bottom")

pp3a 

pp3b<-ggplot(df1, aes(x=log(Ins_prb+1), y=log(AGB), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp3b

pp3c<-ggplot(df1, aes(x=log(Dis_prb+1), y=log(AGB), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(" ")+theme_classic()+
  theme(legend.position = "none")

pp3c

pp3d<-ggplot(df1, aes(x=log(Dmg_prb+1), y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab(expression(paste("Growth (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))  + 
  theme_classic()+ theme(legend.position = "none")

pp3d

pp3e<-ggplot(df1, aes(x=log(Ins_prb+1), y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp3e

pp3f<-ggplot(df1, aes(x=log(Dis_prb+1), y=log(AGBGI), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab(" ")+ ylab('')  + 
  theme_classic()+ theme(legend.position = "none")

pp3f

pp3g<-ggplot(df1, aes(x=log(Dmg_prb+1), y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Damage probability")+ ylab(expression(paste("Mortality (Mg ", " ", ha^{-1}, " ", yr^{-1},")")))+
  theme_classic()+theme(legend.position = "none")
pp3g

pp3h<-ggplot(df1, aes(x=log(Ins_prb+1), y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Insect attack probability")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp3h

pp3i<-ggplot(df1, aes(x=log(Dis_prb+1), y=log(AGBM), group=FT, colour=FT)) + 
  geom_point(shape = 21, alpha = 1/2)+
  geom_smooth(method = lm)+
  stat_poly_eq(formula = y ~ x,
               label.x = "left",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")),
               vstep = 0.1,
               parse = TRUE)+
  stat_fit_alter+
  xlab("Disease pressure probability")+ ylab(" ")+
  theme_classic()+ theme(legend.position = "none")
pp3i

# get legend from pp2a
legend_b <- get_legend(pp3a)

# remove legend of pp2a
pp3a <- pp3a + theme(legend.position = 'none')

Fig3<-plot_grid( pp3a, pp3b,pp3c, pp3d, pp3e, pp3f,pp3g, pp3h,pp3i, labels = c('a', 'b','c', 'd','e', 'f', 'g', 'h', 'i'), 
                 label_size = 12,align = "hv", ncol=3, vjust = 2)

p <- plot_grid(Fig3, legend_b, ncol = 1,rel_heights = c(1, 0.05) )

tiff('1Biomass~ damage.tiff', units="in", width=14, height=13, res=800, compression = 'lzw')
p
dev.off()      



















