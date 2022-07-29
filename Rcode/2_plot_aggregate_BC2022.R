library(dplyr);library(data.table);library(vegan);library(sp)
rm(list=ls())

setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
Ind_BC<-fread("BC_Ind2022.csv")
Ind_BC[,.N, by= Status] [order(Status),]
head(Ind_BC)
summary(Ind_BC)
dt1 <- Ind_BC[, .(StemN=length(DBH)/PS*10000,
                    BA=sum(BA)/PS*10000,##sum BA based on proper subset
                    Bio=sum(Bio)/PS*10), #Bio unit = Mg/ha
                by=list(PLOT_ID, FT, IniY, FinY, Age, Origin, PS, LAT, LONG, Drainage, Status, Cens.length)] %>% unique

dt1_wide <- dcast.data.table(dt1, PLOT_ID+FT+IniY+FinY+Age+Origin+PS+LAT+LONG+Cens.length~Status,
                             value.var=list( "StemN","BA","Bio"))
dt1_wide[is.na(dt1_wide)] <- 0
dt1_wide[,FinN:=StemN_G + StemN_I ]
dt1_wide[, FinBA:= BA_G +BA_I]
dt1_wide[, FinBio:=Bio_G+Bio_I]
dt1_wide[, ":="(
             ABA=FinBA/Cens.length,
             ABAGI=(BA_G+BA_I)/Cens.length,
             ABAM=BA_D/Cens.length,
             AGB=FinBio/Cens.length,
             AGBGI=(Bio_G+Bio_I)/Cens.length,
             AGBM=Bio_D/Cens.length)]

pdmg <- Ind_BC %>% group_by(Prov, PLOT_ID,PLOT_MS,  FT) %>% 
  dplyr::summarise(Prov=unique(Prov),
                   PLOT_ID=unique(PLOT_ID),
                   FT=unique(FT),
                   LAT=unique(LAT),
                   LONG=unique(LONG),
                   IniY=unique(IniY),
                   FinY=unique(FinY),
                   Total_stem=length(T_N),
                   Ins=sum(Insect),
                   Dis=sum(Disease),
                   Dmg=sum(Damage),
                   Ins_prob=Ins/Total_stem,
                   Dis_prob=Dis/Total_stem,
                   Dmg_prb=Dmg/Total_stem) %>% unique()
BC_pdmg <- merge(dt1_wide, pdmg, by=c( "PLOT_ID", "FT", "IniY", "FinY", "LAT", "LONG")) %>%unique()
summary(BC_pdmg)
stem1 <- BC_pdmg[Total_stem==1, ][]
Ind_stem1 <- Ind_BC[PLOT_ID %in% stem1$PLOT_ID, ]
uniqueN(Ind_stem1$T_N)
Ind_stem1[, .(N=length(unique(T_N))), by=c("PLOT_ID",'PLOT_MS','IniY','FinY')]
Ind_stem1[, .(N=length(unique(DBH))), by=c("PLOT_ID",'PLOT_MS','IniY','FinY')]
##########remove BC_4047798_1_2002, and BC_4030386_1_1990------
BC_pdmg <- BC_pdmg[!PLOT_MS %in% c('BC_4047798_1_2002', 'BC_4030386_1_1990'),]
uniqueN(BC_pdmg$PLOT_ID) ##5287
fwrite(BC_pdmg, "BC_Plot_AGB&Damage2022.csv")






## following are optional variables to be added if needed
names_RA<-paste(names,"RA",sep="_")
Plot_BC[,ABAM:=DeadBA/(FinY-IniY)]
Plot_BC[,Mort_Rate:=1-((GrowN/IniN)^(1/(FinY-IniY)))]##Mort Rates
Plot_BC[,RMR:=ABAM/((FinBA+IniBA)/2)]
Plot_BC[,Mort_Stem:=(DeadN/(PS))/(FinY-IniY)]
Plot_BC[,ABAG:=GrowBA/(FinY-IniY)]
Plot_BC[,RGR:=ABAG/((FinBA+IniBA)/2)]
Plot_BC[,Surv_Stem:=(GrowN/(PS))/(FinY-IniY)]
Plot_BC[,ABAI:=IngrBA/(FinY-IniY)]
Plot_BC[,R_Rate:=((FinN/GrowN)^(1/(FinY-IniY)))]##Ingr Rates
Plot_BC[,RIR:=ABAI/((FinBA+IniBA)/2)]
Plot_BC[,Ingr_Stem:=(IngrN/(PS))/(FinY-IniY)]
Plot_BC[,ABAGI:=(GrowBA+IngrBA)/(FinY-IniY)]
Plot_BC[,RGIR:=ABAGI/((FinBA+IniBA)/2)]
Plot_BC[,Shannon:=diversity(Plot_BC[,names_RA,with=FALSE])]
Plot_BC[,NBAC:=(FinBA-IniBA)/(Length)]
Plot_BC[,RNBAC:=NBAC/((FinBA+IniBA)/2)]

Plot_BC<-Plot_BC[Origin>0]
Plot_BC[,MY:=(FinY+IniY)/2]
Plot_BC[,SA:=MY-Origin]
Plot_BC[,logSA:=log(SA)]
Plot_BC[,":="(mod_MY=MY-mean(MY),
              mod_SA=logSA-mean(logSA))]
Plot_BC[,PS:=PS]
Plot_BC[,TotalLength:=(max(FinY)-min(IniY)),by=PLOT_ID]
Plot_BC[,wt:=TotalLength*sqrt(PS)]
Plot_BC[,wt:=wt/max(wt)]
mod<-(lmer(ABAGI~mod_MY*mod_SA+(1|PLOT_ID),Plot_BC))
bg.pts<-resid(mod)+fixef(mod)[1]+(fixef(mod)[2]*Plot_BC$mod_MY)
ggplot(Plot_BC,aes(x=MY,y=bg.pts))+stat_summary(fun.data="mean_cl_boot")
summary(mod)

