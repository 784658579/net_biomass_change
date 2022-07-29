setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")
rm(list = ls())
install.packages(c("permute", "lattice", "ade4","ape", "geometry"))
library(dplyr);library(data.table);library(vegan);library(sp)
library(permute); library(lattice);library(FD)
library(MASS)

rm(list=ls())
Ind_BC<-fread("BC_Ind2022.csv")
Ind_BC[,.N, by= Status] [order(Status),]
FinN <- Ind_BC[!Status=="D", .N, by=c("PLOT_ID","FinY","PLOT_MS" , "Species")]
df_wide <- dcast(FinN, PLOT_ID + FinY + PLOT_MS ~ Species, value.var = "N")
PLOT_MS <- df_wide$PLOT_MS
PLOT_ID <- df_wide$PLOT_ID
FinY <- df_wide$FinY
PS <- merge( Ind_BC,df_wide, by=c("PLOT_ID", "FinY", "PLOT_MS"))##To have PS value
df <- df_wide[, -c("XC", "XH", "PLOT_MS", "PLOT_ID", "FinY")]
df[is.na(df)] <- 0
SR <- specnumber(df)
raremax <- min(rowSums(df))
Srar <- rarefy(df, raremax)
Srar_PS <- rarefy(df, min(PS$PS))
df <- sapply(df, as.numeric) %>% as.data.frame()
H <- diversity(df)
D <- diversity(df, index = "simpson")
df$SR <- SR
df$Srar <- Srar
df$Srar_PS <- Srar_PS
df$H <- H
df$D <- D
df <- sapply(df, as.integer) %>% as.data.frame()
op <- par(mar=c(4,4,4,4))
plot(df$SR, df$Srar_PS, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(df, step = 20, sample = raremax, col = "blue", cex=0.6)
rarecurve(df, step = 20, sample = min(PS$PS), col = "red", cex=0.6)
df <- cbind(PLOT_ID, FinY, PLOT_MS, df)
fwrite(df, "BC_Diversity2022.csv")


#Derive species specific basal area
Plot.sp<-Ind_BC[, .(BA=sum(BA[which(Status!="D")])/PS*10000),  #only live trees considered
                by=list(PLOT_ID, FinY, Species)] %>% unique

Plot.sp <- Plot.sp[ !(Species == 'XH' |Species == 'XC'),] ## drop XH and Xc for
## convert absolute abundance (BA) to relative abundance by species
Plot.BA<-Plot.sp[, .(TBA=sum(BA)),  by=list(PLOT_ID, FinY)]

Plot.spr<-merge(Plot.sp, Plot.BA, by=c("PLOT_ID", "FinY"))
Plot.spr[, RBA:=BA/TBA]
## to wide format
Plot_spr_wide<-dcast.data.table(Plot.spr, PLOT_ID+FinY~Species, value.var="RBA", fun.aggregate=sum)
fwrite(Plot_spr_wide, "BC_Plot_RBA2022.csv")
########################## Functional diveristy ------------------------
########################################################################
rm(list = ls())
traits <- fread("Species_FT1_filled_by_Genus_average_All_0520.csv")
abun <- fread("BC_Plot_RBA2022.csv")
traits <- traits[,":="(Habit=ifelse(Habit=="D", 1, 0), Struct=ifelse(Struct=="N", 1, 0))]
tr1 <- traits
tr1[is.na(tr1)] <- 0
tr1 <- tr1[tr1$Species %in% colnames(abun)]
abun <- abun %>% 
  mutate(sum=rowSums(abun[, 3:41])) %>% 
  filter(sum>0)
summary(abun$sum)
abun$sum <- NULL
PLOT_ID <- abun$PLOT_ID
FinY <- abun$FinY
abun1 <- abun[, -c(1,2)]
nrow(tr1)-ncol(abun1)
tr1 <- tr1[, -c("SPEC", "Genus")][order(Species, )]
colnames(abun1) <- tr1$Species
abun1 <- cbind(PLOT_ID,FinY,  abun1)
fwrite(tr1, "BC_Applied_Species_Traits2022.csv")
fwrite(abun1, "BC_Plot_RBA_match_Traits2022.csv")
##################################################Functional diversity
rm(list=ls())
tr1 <- fread("BC_Applied_Species_Traits2022.csv") 
abun <- fread("BC_Plot_RBA_match_Traits2022.csv")
tr1_matrix <- as.matrix(tr1, rownames = "Species")
abun$PLOT_MS <- paste(abun$PLOT_ID, abun$FinY, sep = "_")
PLOT_MS <- abun$PLOT_MS 
PLOT_ID <- abun$PLOT_ID 
FinY <- abun$FinY 
abun_matrix <- as.matrix(abun[, -c(1, 2)], rownames = "PLOT_MS")
FD1 <- dbFD(tr1_matrix, abun_matrix) %>% as.data.table()
head(FD1)
FD1[,":="(sing.sp=NULL,FRic=NULL,qual.FRic=NULL,FEve=NULL,FDiv=NULL)]
FD1 <- cbind(PLOT_ID, FinY, PLOT_MS, FD1)
summary(FD1)
fwrite(FD1, "BC_FD2022.csv")
###########################################################################
##################################################### mean plot level CWM 
rm(list = ls())
FD <- fread("BC_FD2022.csv")
m.CWM<-FD[,.(CWM.Nmass,CWM.Pmass,CWM.SLA,CWM.Amass,CWM.Aarea,CWM.Gs,CWM.Ks, CWM.WD,CWM.LL, CWM.DT,CWM.ST,CWM.Habit,CWM.Struct)]#Added binary Habit/Struct
m.CWM[,":="(CWM.Habit=as.numeric(CWM.Habit),CWM.Struct=as.numeric(CWM.Struct))]
summary(m.CWM)
m.PCA <- prcomp(m.CWM,scale=TRUE)
summary(m.PCA)#PC1 explains 73.38%; PC2 explains 11.81%; totally 85.19%
m.PC1 <- m.PCA$x[,1]
m.PC2 <- m.PCA$x[,2]
#PLOT_ID <- str_sub(FD$PLOT_MS, 1,nchar(FD$PLOT_MS)-5 ) %>% as.data.frame()
#FinY <- str_sub(FD$PLOT_MS, -4) %>% as.data.frame()
PLOT_ID <- FD$PLOT_ID
FinY <- FD$FinY
PLOT_MS <- FD$PLOT_MS
m.CWM_PCA<-data.table(cbind(PLOT_ID, FinY, PLOT_MS, m.PC1,m.PC2))
FD$FinY <- as.character(FD$FinY)
m.FD_CWM_PCA<-merge(FD,m.CWM_PCA,by=c("PLOT_ID", "FinY", "PLOT_MS"))
summary(m.FD_CWM_PCA)
fwrite(m.FD_CWM_PCA, "BC_PlotMS_Mean_FD+CWM+PC2022.csv")

m.FD_CWM_PCA1 <- m.FD_CWM_PCA %>% 
  group_by(PLOT_ID) %>% 
  summarise(m.nbsp=mean(nbsp),
            m.FDis = mean(FDis),
            m.RaoQ =mean(RaoQ),
            m.CWM.Nmass=mean(CWM.Nmass),
            m.CWM.Pmass=mean(CWM.Pmass),
            m.CWM.SLA=mean(CWM.SLA),
            m.CWM.Amass=mean(CWM.Amass),
            m.CWM.Aarea=mean(CWM.Aarea),
            m.CWM.Gs=mean(CWM.Gs),
            m.CWM.Ks=mean(CWM.Ks),
            m.CWM.WD=mean(CWM.WD),
            m.CWM.LL=mean(CWM.LL),
            m.CWM.DT=mean(CWM.DT),
            m.CWM.ST=mean(CWM.ST),
            m.CWM.Habit=mean(CWM.Habit),
            m.CWM.Struct=mean(CWM.Struct),
            m.PC1=mean(m.PC1),
            m.PC2=mean(m.PC2))
summary(m.FD_CWM_PCA1)
fwrite(m.FD_CWM_PCA1, "BC_Plot_Mean_FD+CWM+PC2022.csv")

################################################################################################################
#########################Phylogenetic diversity

devtools::install_github("jinyizju/V.PhyloMaker")
library(V.PhyloMaker)
rm(list = ls())
dt1 <- fread("Family_Genus_Species_BC.csv")
abun <- fread("BC_Plot_RBA2022.csv")
dt1 <- dt1[Species %in% colnames(abun),]
taxa<-dt1[,c("Species","Genus","Family")]
result <- phylo.maker(taxa, scenarios=c("S1","S2","S3"))
library(ape)
par(mfrow = c(1, 3))
plot.phylo(result$scenario.1, cex = 1.5, main = "scenario.1")
nodelabels(round(branching.times(result$scenario.1), 1), cex = 1)
plot.phylo(result$scenario.2[[1]], cex = 1.5, main = "scenario.2")
nodelabels(round(branching.times(result$scenario.2[[1]]), 1), cex = 1)
plot.phylo(result$scenario.3, cex = 1.5, main = "scenario.3")
nodelabels(round(branching.times(result$scenario.3), 1), cex = 1)

tree<-result$scenario.1
write.tree(tree, file="BC_PhyloTree2022.txt")
phy<-read.tree("BC_PhyloTree2022.txt")
phy
phy$tip.label
comm <-fread("BC_Plot_RBA_match_Traits2022.csv")
comm$PLOT_MS <- comm[, paste(PLOT_ID, FinY, sep = "_")]
PLOT_MS <- comm$PLOT_MS
PLOT_ID <- comm$PLOT_ID
FinY <- comm$FinY
comm <- comm[, -c(1,2)]
comm <- as.matrix(comm, rownames = "PLOT_MS")
dt1 <- dt1[, -c("Family", "Genus")]
dt1 <- as.matrix(dt1, rownames = "Species")
matches <- as.vector(phy$tip.label)
colnames(comm) <- as.character(matches)
library(picante)
library(nlme)
combined <- match.phylo.comm(phy, comm) ##### check for mismatch/missing species
phy <- combined$phy ####replace orginal data with the sorted/matched data
comm <- combined$comm

pd<-pd(comm, phy, include.root=F)
head(pd)    ####single-species stands have NA, meaning 0 
psv<-psv(comm, phy)
head(psv)
mpd<-mpd(comm, cophenetic(phy), abundance.weighted =T)
mntd<-mntd(comm, cophenetic(phy), abundance.weighted = T)

PD<-cbind(PLOT_ID, FinY, PLOT_MS, pd, psv, mpd, mntd)
PD[is.na(PD)] <- 0
fwrite(PD, "BC_PlotMS_Phylogenetic_diversity2022.csv")
################FD











