library(dplyr);library(data.table);library(tidyr);library(sp)
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
rm(list=ls())
cat("\014")
BC<-fread("TreeMeasurements.csv")%>%unique
BC[,PLOT_ID:=paste(SITE_IDENTIFIER,PLOT_NUMBER,sep="_")]
BC[,.N, by= SUITABLE_FOR_AGE_IND] 
BC[,.N, by= TREE_PLANTED_IND] ## 
BC[,.N, by= TOTAL_AGE] 

##Forest type
SampleSites<-fread("SampleSites.csv")%>%unique
Sites<-SampleSites
Sites[,.N, by= TREATED_STAND_AGE_OF_STOCK]
Sites[,.N, by= TREATED_STAND_PLANTATION_YEAR]
Sites$FT<-0
Sites[, FT := ifelse(is.na(TREATED_STAND_PLANTATION_YEAR), "N", "P")]  ##Plantation vs Natural
Sites<-Sites[,c("SITE_IDENTIFIER", "FT", "SITE_SERIES")]
Sites$SITE_IDENTIFIER<-factor(Sites$SITE_IDENTIFIER)

## derive LAT and LONG from UTM
loc<-SampleSites[,c('SITE_IDENTIFIER','UTM_ZONE','UTM_NORTHING','UTM_EASTING')]
loc<-na.omit(loc)

Zone_8<-unique(loc[UTM_ZONE==8,.(SITE_IDENTIFIER,UTM_EASTING,UTM_NORTHING)])
coordinates(Zone_8)<-~UTM_EASTING+UTM_NORTHING
proj4string(Zone_8)<-CRS("+proj=utm +zone=8 +datum=NAD83")
Zone_8<-spTransform(Zone_8,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
locs8<-data.table(Zone_8@data$SITE_IDENTIFIER, Zone_8@coords)

Zone_9<-unique(loc[UTM_ZONE==9,.(SITE_IDENTIFIER,UTM_EASTING,UTM_NORTHING)])
coordinates(Zone_9)<-~UTM_EASTING+UTM_NORTHING
proj4string(Zone_9)<-CRS("+proj=utm +zone=9 +datum=NAD83")
Zone_9<-spTransform(Zone_9,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
locs9<-data.table(Zone_9@data$SITE_IDENTIFIER,Zone_9@coords)

Zone_10<-unique(loc[UTM_ZONE==10,.(SITE_IDENTIFIER,UTM_EASTING,UTM_NORTHING)])
coordinates(Zone_10)<-~UTM_EASTING+UTM_NORTHING
proj4string(Zone_10)<-CRS("+proj=utm +zone=10 +datum=NAD83")
Zone_10<-spTransform(Zone_10,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
locs10<-data.table(Zone_10@data$SITE_IDENTIFIER,Zone_10@coords)

Zone_11<-unique(loc[UTM_ZONE==11,.(SITE_IDENTIFIER,UTM_EASTING,UTM_NORTHING)])
coordinates(Zone_11)<-~UTM_EASTING+UTM_NORTHING
proj4string(Zone_11)<-CRS("+proj=utm +zone=11 +datum=NAD83")
Zone_11<-spTransform(Zone_11,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
locs11<-data.table(Zone_11@data$SITE_IDENTIFIER,Zone_11@coords)

locs<-rbind(locs8,locs9,locs10,locs11)
setnames(locs,c("SITE_IDENTIFIER","Long","Lat"))

locs[,.N, by=Lat][order(Lat)]

## Visit number, plot area
PlotDetails<-fread("PlotDetails.csv")%>%unique
Plot<-PlotDetails[,c("SITE_IDENTIFIER","VISIT_NUMBER","PLOT_AREA","PLOT_NUMBER" )]
Plot[,PLOT_ID:=paste(SITE_IDENTIFIER,PLOT_NUMBER,sep="_")]
length(unique(Plot$SITE_IDENTIFIER))
length(unique(Plot$PLOT_ID))
Plot[,.N, by=PLOT_AREA][order(PLOT_AREA)]
Plot<-Plot[PLOT_AREA>=0.04] ##drop small regeneration plots
Plot$SITE_IDENTIFIER<-as.factor(Plot$SITE_IDENTIFIER)
Plot[,.N, by=PLOT_NUMBER]
###################check which site has more than oen plot number? ------------
Plot[, .(N=length(unique(PLOT_NUMBER))), by="SITE_IDENTIFIER"][order(N),]


##Visit numbers matching dates (SAMPLE_SITE_VISIT_START_DATE), start-end, 
SampleSiteVisits<-fread("SampleSiteVisits.csv")%>%unique
Visits<-SampleSiteVisits[,c("SITE_IDENTIFIER","VISIT_NUMBER","SAMPLE_SITE_VISIT_START_DATE" )]
Visits$SAMPLE_SITE_VISIT_START_DATE<-as.character(Visits$SAMPLE_SITE_VISIT_START_DATE)
Visits$Year<-year(as.IDate(Visits$SAMPLE_SITE_VISIT_START_DATE,'%Y-%m-%d  '))

Plot<-merge(Plot, Sites, by=("SITE_IDENTIFIER"))
Plot<-merge(Plot, Visits, by=c("SITE_IDENTIFIER","VISIT_NUMBER"))
Plot$SAMPLE_SITE_VISIT_START_DATE<-NULL

length(unique(Plot$SITE_IDENTIFIER))
length(unique(Plot$PLOT_ID))

# Census<-Plot[, .(Census=max(VISIT_NUMBER)), by=list(PLOT_ID)]
# Census[,.N, by=Census]  # 2091 plots have only one measurement, remove
# Plot<-merge(Plot, Census, by="PLOT_ID")
# Plot<-Plot[Census>1,]
# length(unique(Plot$PLOT_ID))
# Plot[,.N, by=Census]  

## determine stand age based on cored trees
Age<-BC[TOTAL_AGE>0] ## note visit number
length(unique(Age$PLOT_ID))
length(unique(BC$PLOT_ID))
Age<-Age[,c("SITE_IDENTIFIER","VISIT_NUMBER","PLOT_ID", "TOTAL_AGE")]
Age1<-Age[, .(Age=mean(TOTAL_AGE)), by=.(SITE_IDENTIFIER, VISIT_NUMBER,PLOT_ID)]
Age1$SITE_IDENTIFIER<-as.factor(Age1$SITE_IDENTIFIER)

Plot<-merge(Plot, Age1,by=c("SITE_IDENTIFIER","VISIT_NUMBER","PLOT_ID"))
Plot[, Origin := Year -Age]
length(unique(Plot$PLOT_ID))  #8122

##unify Origin at the first measurement
count_1 <- Plot[ , .(count = length(unique(Origin))), by = PLOT_ID]
summary(count_1)  

Origin<-Plot[, .SD[1], PLOT_ID][,c("SITE_IDENTIFIER","Origin")]
Plot$Origin<-NULL
Plot<-merge(Plot, Origin, by='SITE_IDENTIFIER')%>%unique

locs$SITE_IDENTIFIER<-as.factor(locs$SITE_IDENTIFIER)
Plot<-merge(Plot, locs, by='SITE_IDENTIFIER')%>%unique

#### Work on tree level 
BC<-BC[OUT_OF_PLOT_IND=="N"] ##remove trees outside the plot
# BC<-BC[TREE_PLANTED_IND!="Y"] ##we can remove planted trees but here we want to include them to test forest type effect

BC[,.N, by=TREE_CLASS_CODE] 
BC[,.N, by=TREE_EXTANT_CODE] ##D and NA = Dead
BC[,.N, by=DIAMETER_SOURCE_CODE]

BC[, MORTALITY := ifelse(TREE_EXTANT_CODE == "D"|is.na(TREE_EXTANT_CODE), 1, 0)]

##take the first row when MORTALITY == 1 to avoid multiple mortality obs for the same tree
output1<-BC[MORTALITY==1&VISIT_NUMBER>1] ##dead tree in the first measurement removed
output0<-BC[MORTALITY==0]
output1<-output1[order(SITE_IDENTIFIER, TREE_NUMBER,VISIT_NUMBER), .SD[1], by = .(SITE_IDENTIFIER, TREE_NUMBER)]
BC<-rbind(output0, output1)[order(SITE_IDENTIFIER, TREE_NUMBER,VISIT_NUMBER),]

BC<-BC[,c('SITE_IDENTIFIER','VISIT_NUMBER','PLOT_ID','TREE_NUMBER','DIAMETER','TREE_SPECIES_CODE', "MORTALITY")]
BC[, `:=`(Census=max(VISIT_NUMBER)), by=list(PLOT_ID)]
BC[,.N, by=Census]  
BC<-BC[Census>1]
length(unique(BC$PLOT_ID))
BC[,.N, by=Census]  

BC<-BC[order(SITE_IDENTIFIER, PLOT_ID, TREE_NUMBER,VISIT_NUMBER),]
BC<-BC[,T_N:=paste(PLOT_ID, TREE_NUMBER,sep="_")]
BC$SITE_IDENTIFIER<-as.factor(BC$SITE_IDENTIFIER)
BC$VISIT_NUMBER<-as.factor(BC$VISIT_NUMBER)
Plot$VISIT_NUMBER<-as.factor(Plot$VISIT_NUMBER)
Plot$SITE_IDENTIFIER<-as.factor(Plot$SITE_IDENTIFIER)

##include insects/disease/animal/abiotic 
insp<-fread("TreeDamageOccurrences.csv") ## for growth causes
insp[,.N, by=DAMAGE_AGENT_CODE][order(N),]  ##143 types, need grouping
insp[,.N, by=SEVERITY_RATING_VALUE,]  ## insufficient to be used 

treeloss<-fread("TreeLossIndicators.csv")  ## for mortality causes
treeloss[,.N, by=TREE_LOSS_INDICATOR_CODE][order(TREE_LOSS_INDICATOR_CODE), ]  ## essentially all disease

treedamage<-fread("treedamage.csv")
insp<-merge(insp, treedamage,by='DAMAGE_AGENT_CODE')
insp[,.N, by=DAMAGE][order(N),]  ##
ins.p<-insp[,c('SITE_IDENTIFIER','VISIT_NUMBER','PLOT_NUMBER','TREE_NUMBER', 'TREE_SPECIES_CODE', 'DAMAGE')]
ins.p$SITE_IDENTIFIER<-as.factor(ins.p$SITE_IDENTIFIER)
ins.p$VISIT_NUMBER<-as.factor(ins.p$VISIT_NUMBER)
ins.p[,PLOT_ID:=paste(SITE_IDENTIFIER,PLOT_NUMBER,sep="_")] 
      
BC<-merge(BC, ins.p, by=c('SITE_IDENTIFIER','VISIT_NUMBER','PLOT_ID','TREE_NUMBER', 'TREE_SPECIES_CODE'),  all=T)
BC$PLOT_NUMBER<-NULL
BC<-merge(BC,Plot, by=c('SITE_IDENTIFIER','PLOT_ID','VISIT_NUMBER'),  allow.cartesian = TRUE)
BC[,Insect:=0]
BC[which(DAMAGE=="Insect"), Insect:=1]
BC[,Disease:=0]
BC[which(DAMAGE=="Disease"), Disease:=1]
BC[, Damage:=0]
BC[which(DAMAGE %in% c("Animal", "Unknown", "Abiotic", "Insect", "Disease")), Damage:=1]
BC$DAMAGE<-NULL
all(BC$Census.x == BC$Census.y)
setnames(BC, "DIAMETER", "DBH")
fwrite(BC, 'BC_examin.csv')

## identify INGROWTH, which has a new tree number in a subsequent measurement
BC<-BC[order(T_N, Year),]##

BC[,":="(prevDBH=data.table::shift(DBH,n=1,fill=NA,type="lag"),
         IniY=data.table::shift(Year,n=1,fill=NA,type="lag")),by=T_N] ##create previous dbh and previous Year columns

BC[, DBH := ifelse(MORTALITY == 1 & DBH<prevDBH, prevDBH, DBH)] ### dead trees shrink; previous DBH shall be used for biomass loss 


####check and correct DBH measurement or recording errors
BC1 <- BC
BC1[, deltaDBH:=DBH-prevDBH] 
BC1[,.N, by= deltaDBH] [order(deltaDBH),] ##why so huge negative and positive values
BC1[, DBH_ag:=deltaDBH/(Year-IniY)] 
##############First step check on data distribution----------
par(mfrow=c(1, 3))
hist(BC1$deltaDBH, main='Histogram')
boxplot(BC1$deltaDBH, main='boxplot')
qqnorm(BC1$deltaDBH, main='Normal Q-Q plot')

neg <- BC1[deltaDBH <0, c('PLOT_ID','IniY', 'Year','Age', 'MORTALITY', 'DBH', 'prevDBH', 'deltaDBH', 'DBH_ag') ]
summary(neg) 
remove(BC1)
##when deltaDBH is in normal range, the alive tree has smaller DBH than prevDBH. 

## Method2-----
## Alive trees are possible to have larger prevDBH than DBH, so I replace without mortality condition
BC2 <- fread('BC_examin.csv')
BC2<-BC2[order(T_N, Year),]##
BC2[,":="(prevDBH=data.table::shift(DBH,n=1,fill=NA,type="lag"),
          IniY=data.table::shift(Year,n=1,fill=NA,type="lag")),by=T_N]
BC2[, DBH:= ifelse(DBH< prevDBH, prevDBH, DBH)] 
BC2[, deltaDBH:=DBH-prevDBH] 
BC2[,.N, by= deltaDBH] [order(deltaDBH),] 
BC2[, DBH_ag:=deltaDBH/(Year-IniY)] 
posBC2 <- BC2[which(BC2$DBH_ag >5), ]#T_N=='4001610_1_118'
summary(BC2)
fwrite(BC2, 'BC_DBH_Check.csv')# I can't find all the issue plot in excel, so turn to work in r. 
par(mfrow=c(1, 3))
hist(BC2$deltaDBH, breaks=2, main='Histogram')
boxplot(BC2$deltaDBH, main='boxplot')
qqnorm(BC2$deltaDBH, main='Normal Q-Q plot')
SBC2 <- BC2[, c('PLOT_ID','IniY', 'Year','Age', 'MORTALITY','T_N',  'DBH', 'prevDBH', 'deltaDBH', 'DBH_ag') ]
summary(SBC2)
SBC2_S <- SBC2[deltaDBH> 20, ]
#BC2.1 <- BC2[PLOT_ID %in% c('4036820_1', '4041848_1','4042260_1','4043946_1','4044764_1','4044806_1','4000432_1',
                            #'4000884_1','4000891_1','4001293_1','4007504_1','4016157_1','4020803_1'),]

#BC2.2 <- BC2[T_N %in% c('4036820_1_55', '4041848_1_107','4042260_1_76','4043946_1_385','4044764_1_284','4044806_1_91','4000432_1_266',
                            #'4000884_1_593','4000891_1_526','4001293_1_371','4007504_1_1','4016157_1_264','4020803_1_361'),]

# It is better to check the DBH increment before shift
BC3 <- fread('BC_examin.csv')
BC3<-BC3[order(T_N, Year),]##
BC3.1 <- BC3[T_N %in% c('4036820_1_55', '4041848_1_107','4042260_1_76','4043946_1_385','4044764_1_284','4044806_1_91','4000432_1_266',
                      '4000884_1_593','4000891_1_526','4001293_1_371','4007504_1_1','4016157_1_264','4020803_1_361'),]

BC3.1 <- BC3.1[, DBH := ifelse(DBH %in% c(99.3, 100.0, 54.1, 10.1, 79.9, 227.0, 124.0, 74.4, 174.0, 122.0,
                                       114.0, 83.0, 102.0,105.0 ), DBH/10, DBH) ]
BC3.2 <- BC3[ !T_N %in% c('4036820_1_55', '4041848_1_107','4042260_1_76','4043946_1_385','4044764_1_284','4044806_1_91','4000432_1_266',
                        '4000884_1_593','4000891_1_526','4001293_1_371','4007504_1_1','4016157_1_264','4020803_1_361'),]
BC3.0 <- rbind(BC3.1, BC3.2)[order(PLOT_ID, T_N), ]
BC3.0 <- BC3.0[DBH>4, ]
summary(BC3.0$DBH)
plot_outlier <- BC3.0[PLOT_ID=='4001610_1',]## I will set the annual increment as 0.8 for T_N=='4001610_1_118'
BC3.0$DBH[BC3.0$Year=='2018' & BC3.0$T_N=='4001610_1_118'] <- 11

## use interquartile range to find outlier-----
### This method is like MAD, not working for our dataset---

BC3.0[,":="(prevDBH=data.table::shift(DBH,n=1,fill=NA,type="lag"),
          IniY=data.table::shift(Year,n=1,fill=NA,type="lag")),by=T_N]
BC3.0[, DBH:= ifelse( DBH< prevDBH, prevDBH, DBH)] ## Alive trees have larger prevDBH than DBH, so I replace without mortality condition
BC3.0[, deltaDBH:=DBH-prevDBH] 
BC3.0[,.N, by= deltaDBH] [order(deltaDBH),] 
BC3.0[, DBH_ag:=deltaDBH/(Year-IniY)] 
par(mfrow=c(1, 3))
hist(BC3.0$deltaDBH, main='Histogram')
boxplot(BC3.0$deltaDBH, main='boxplot')
qqnorm(BC3.0$deltaDBH, main='Normal Q-Q plot')

hist(BC3.0$DBH_ag, main='Histogram')
boxplot(BC3.0$DBH_ag, main='boxplot')
qqnorm(BC3.0$DBH_ag, main='Normal Q-Q plot')

remove(BC)
BC <- BC3.0

FirstM<-BC[VISIT_NUMBER==1] ##NA in the first measurement is not ingrowth
SubM<-BC[VISIT_NUMBER!=1]
FirstM$INGROWTH<-0
SubM[, INGROWTH := ifelse(is.na(prevDBH), 1, 0)]
BC<-rbind(FirstM,SubM)

## Latin names and code matching
BC[,.N, by= TREE_SPECIES_CODE] [order(N,decreasing = TRUE)]
BC_species<-fread("species_BC.csv")[,c(1:2)]%>%unique
BC<-merge(BC, BC_species, by="TREE_SPECIES_CODE")
BC<-BC[order(T_N, Year),]

BC[,.N, by=c('TREE_SPECIES_CODE','LATIN')][order(N,decreasing = TRUE)]

BC$Prov<-"BC"
BC<-BC[,PLOT_MS:=paste(PLOT_ID,Year,sep="_")]
BC<-BC[,PS:=10000*PLOT_AREA]
hist(BC$PLOT_AREA, breaks=100)
BC[,.N, by= PLOT_AREA] [order(PLOT_AREA),]

BC[,.N, by=DBH][order(DBH),]
BC[,.N, by=PS][order(PS),]
length(unique(BC$PLOT_ID))

BC[, PLOT_ID :=paste(Prov,PLOT_ID,  sep="_")]
BC[, PLOT_MS :=paste(Prov,PLOT_MS,sep="_")]
BC[, T_N:=paste(Prov,T_N,sep="_") ]

BC<-BC[,c('Prov','PLOT_ID','PLOT_MS','FT','Year','IniY','Age','Origin','PS','Lat','Long',"SITE_SERIES",'T_N','TREE_SPECIES_CODE','LATIN',  'DBH','prevDBH','INGROWTH','MORTALITY','Insect','Disease', 'Damage')]

setnames(BC, old = c('Prov','PLOT_ID','PLOT_MS','FT','Year','IniY','Age','Origin','PS','Lat','Long',"SITE_SERIES",'T_N','TREE_SPECIES_CODE','LATIN',  'DBH','prevDBH','INGROWTH','MORTALITY','Insect','Disease','Damage'), 
             new = c('Prov','PLOT_ID','PLOT_MS','FT','FinY','IniY','Age','Origin','PS','LAT','LONG',"Drainage",'T_N','SPEC', 'Species','DBH','prevDBH','INGROWTH','MORTALITY','Insect','Disease','Damage'))

BC[which(MORTALITY==1&is.na(DBH)==TRUE),DBH:=prevDBH] 
BC[, LAT := ifelse(LAT<10, LAT*10, LAT)]  #correct typos for two plots with LAT <10

BC[, BA :=(DBH/200)^2*pi]
BC[,deltaBA:=BA-(prevDBH/200)^2*pi]
BC[,Status := ifelse(MORTALITY == 1, "D", ifelse(INGROWTH == 1, "I", "G"))] 

BC[,inc_grow:=abs(INGROWTH-1)]
BC[,  Cens.length:=FinY-IniY, by=.(T_N)]

BC<-BC[Cens.length>0&!is.na(Cens.length)]
summary(BC)
length(unique(BC$PLOT_ID))
length(unique(BC$T_N))
BC[, .N, by='Status'][order(N),]
write.csv(BC,"BC2022.csv",row.names=F)

BC<-fread("BC2022.csv")

##set up a file for SoilGrid data 
BC_loc<-BC[, c("PLOT_ID","LONG","LAT")]%>%unique
BC_loc[,.N, by= LAT][order(LAT),] ## 

write.csv(BC_loc,"BCloc.csv",row.names=F)

length(unique(BC$PLOT_ID))

##obtain stand level values ; long form by tree status? 
bc<-BC[,":="(N=as.numeric(length(DBH)), #length counts gives us number of stems
             BA=sum(BA)/PS,               #sum BA based on proper subset (! cm2/m2 = m2/ha)
             dBA=sum(deltaBA)/PS*10000,
             Insect=mean(Insect),       #proportional stems
             Disease=mean(Disease), 
             Richness=as.numeric(length(unique(SPEC)))),
            by=.(PLOT_ID,FinY,FT, Status)]%>%
  .[,Census:=length(unique(FinY)),by=.(PLOT_ID)]%>%
  .[,.(Prov=Prov,PLOT_ID=PLOT_ID,PLOT_MS=PLOT_MS,FinY=FinY, FT=FT, PS=PS,LAT=LAT,LONG=LONG,
       Status=Status,
       N=N,BA=BA, dBA=dBA, Origin=Origin,
       Insect=Insect, Disease = Disease,
       Richness=Richness)]%>%
  unique%>%
  .[order(Prov,PLOT_ID, FinY)]

length(unique(bc$PLOT_ID))

bc$Age<-bc$FinY-bc$Origin

summary(lm(dBA~scale(Richness), bc))

summary(lm(Insect~scale(Richness), bc[FinY>2000,]))
summary(lm(GrowDisease~scale(Richness), bc[FinY>2000,]))
summary(lm(log(GrowBA+1)~(scale(GrowInsect)+scale(GrowDisease))*FT*log(Age), bc[FinY>2000,]))


##shall consider only insect and disease 

library(ggplot2)
ggplot(bc, aes(x=FinY, y=Disease)) + geom_point()+geom_smooth()  ##recording since 1989
ggplot(bc[FinY>=1989,], aes(x=FinY, y=Insect)) + geom_point()+geom_smooth()  ##recording since 1989
ggplot(bc, aes(x=log(Age), y=GrowBA)) + geom_point()+geom_smooth()  ##recording since 1989

ggplot(bc[FinY>1990,], aes(x=Richness, y=Insect, group =FT)) + geom_point()+geom_smooth()  ##recording since 1989
ggplot(bc[FinY>1990,], aes(x=Richness, y=Disease, group =FT)) + geom_point()+geom_smooth()  ##recording since 1989

bc$Age<-bc$FinY-bc$Origin
ggplot(bc[FinY>1990,], aes(x=Age, y=Insect, group =FT)) + geom_point()+geom_smooth()  ##recording since 1989


##data on insect and disease are more complete since 2010 based on manual and possibly ok since 1989


