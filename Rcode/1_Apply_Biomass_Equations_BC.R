rm(list=ls())
library(dplyr);library(data.table);
setwd("D:/Google Drive/My Drive/PSP/PSP_2022/2022/BC")

BC<-fread("BC2022.csv", header=T)

#Calculate biomass by allometric equations (Lambert 2005; Ung 2008 for BC)
#Criteria: if there are one or a few species for one genera, applied these values (e.g., elms->WE, oaks->WO, birches->WB)
#However, if there are multiple species belong to one genera, they went for hardwoods/softwoods eqn (e.g., Pinus/Acer/Abies)
table(BC[,Species])
BC[,.N, by=c('Species','SPEC')][order(N,decreasing = TRUE)]

BC[Species=="Abies amabilis"|Species=="Abies grandis",
    ":="(AGBwood=0.0941*DBH^2.3491,
         AGBbark=0.0323*DBH^2.0761,
         AGBbranches=0.0448*DBH^1.9771,
         AGBfoliage=0.0538*DBH^1.3584)]
BC[Species=="Abies balsamea"|Species=="Abies_sp",
    ":="(AGBwood=0.0534*DBH^2.4309,
         AGBbark=0.0108*DBH^2.3876,
         AGBbranches=0.0121*DBH^2.3519,
         AGBfoliage=0.0251*DBH^2.0389)]
BC[Species=="Abies lasiocarpa",
    ":="(AGBwood=0.0528*DBH^2.4309,
         AGBbark=0.0108*DBH^2.3876,
         AGBbranches=0.0121*DBH^2.3519,
         AGBfoliage=0.0251*DBH^2.0389)]
BC[Species=="Acer rubrum"|Species=="Acer_sp"|Species=="Acer circinatum"|Species=="Acer glabrum"|
      Species=="Acer macrophyllum"|Species=="Acer pensylvanicum"|Species=="Acer pseudoplatanus"|
      Species=="Acer negundo"|Species=="Acer spicatum"|Species=="Acer_sp", #for RM (sbst for other Acer because coefs are between SiM and SM)
    ":="(AGBwood=0.1014*DBH^2.3448,
         AGBbark=0.0291*DBH^2.0893,
         AGBbranches=0.0175*DBH^2.4846,
         AGBfoliage=0.0515*DBH^1.5198)]
BC[Species=="Acer saccharum",
    ":="(AGBwood=0.1315*DBH^2.3129,
         AGBbark=0.0631*DBH^1.9241,
         AGBbranches=0.0330*DBH^2.3741,
         AGBfoliage=0.0393*DBH^1.6930)]
BC[Species=="Acer saccharinum",
    ":="(AGBwood=0.2324*DBH^2.1000,
         AGBbark=0.0278*DBH^2.0433,
         AGBbranches=0.0028*DBH^3.1020,
         AGBfoliage=0.1430*DBH^1.2580)]

BC[Prov=="BC"&(Species=="Betula papyrifera"|Species=="Betula_sp"|Species=="Betula populifolia"),
    ":="(AGBwood=0.0604*DBH^2.4959,
         AGBbark=0.0140*DBH^3.2721,
         AGBbranches=0.0147*DBH^2.5227,
         AGBfoliage=0.0591*DBH^1.6036)]
BC[Prov!="BC"&Species=="Betula papyrifera",
    ":="(AGBwood=0.0593*DBH^2.5026,
         AGBbark=0.0135*DBH^2.4053,
         AGBbranches=0.0135*DBH^2.5532,
         AGBfoliage=0.0546*DBH^1.6351)]
BC[Species=="Betula alleghaniensis",
    ":="(AGBwood=0.1932*DBH^2.1569,
         AGBbark=0.0192*DBH^2.2475,
         AGBbranches=0.0305*DBH^2.4044,
         AGBfoliage=0.1119*DBH^1.3973)]
BC[Species=="Callitropsis nootkatensis"|Species=="Picea_sp",#for EC (sbst for YC although not Thuja but Chamaecyparis)
    ":="(AGBwood=0.0619*DBH^2.3821,
         AGBbark=0.0139*DBH^2.2653,
         AGBbranches=0.0217*DBH^1.9771,
         AGBfoliage=0.0776*DBH^1.6995)]
BC[Species=="Carya cordiformis"|Species=="Juglans cinerea",#for "Hickory" (applied for any Carya; subst for Juglans)
    ":="(AGBwood=0.2116*DBH^2.2013,
         AGBbark=0.0365*DBH^2.1133,
         AGBbranches=0.0087*DBH^2.8927,
         AGBfoliage=0.0173*DBH^1.9830)]
BC[Species=="Fagus grandifolia",
    ":="(AGBwood=0.1478*DBH^2.2986,
         AGBbark=0.0120*DBH^2.2388,
         AGBbranches=0.0370*DBH^2.3680,
         AGBfoliage=0.0376*DBH^1.6164)]
BC[Species=="Fraxinus americana",
    ":="(AGBwood=0.1861*DBH^2.1665,
         AGBbark=0.0406*DBH^1.9946,
         AGBbranches=0.0461*DBH^2.2291,
         AGBfoliage=0.1106*DBH^1.2277)]
BC[Species=="Fraxinus nigra"|Species=="Fraxinus pennsylvanica",#"red ash" = green ash
    ":="(AGBwood=0.1571*DBH^2.1817,
         AGBbark=0.0416*DBH^2.0509,
         AGBbranches=0.0177*DBH^2.3370,
         AGBfoliage=0.1041*DBH^1.2185)]
BC[Species=="Larix laricina"|Species=="Larix_sp"|Species=="Larix occidentalis",#for TL (sbst for sp, WL)
    ":="(AGBwood=0.0625*DBH^2.4475,
         AGBbark=0.0174*DBH^2.1109,
         AGBbranches=0.0196*DBH^2.2652,
         AGBfoliage=0.0801*DBH^1.4875)]
BC[Species=="Ostrya virginiana",#"Hop-hornbeam" = Ironwood
    ":="(AGBwood=0.1929*DBH^1.9672,
         AGBbark=0.0671*DBH^1.5911,
         AGBbranches=0.0278*DBH^2.1336,
         AGBfoliage=0.0293*DBH^1.9502)]
BC[Species=="Picea engelmannii",
    ":="(AGBwood=0.0223*DBH^2.7169,
         AGBbark=0.0118*DBH^2.2733,
         AGBbranches=0.0336*DBH^2.2123,
         AGBfoliage=0.0683*DBH^1.8022)]
BC[Prov=="BC"&(Species=="Picea glauca"|Species=="Picea_sp"),
    ":="(AGBwood=0.0334*DBH^2.5980,
         AGBbark=0.0114*DBH^2.3057,
         AGBbranches=0.0302*DBH^2.0927,
         AGBfoliage=0.1515*DBH^1.5012)]
BC[Prov!="BC"&Species=="Picea glauca",
    ":="(AGBwood=0.0359*DBH^2.5775,
         AGBbark=0.0116*DBH^2.3022,
         AGBbranches=0.0283*DBH^2.0823,
         AGBfoliage=0.1601*DBH^1.4670)]
BC[Prov=="BC"& Species=="Picea mariana",
    ":="(AGBwood=0.0494*DBH^2.5025,
         AGBbark=0.0148*DBH^2.2494,
         AGBbranches=0.0291*DBH^2.0751,
         AGBfoliage=0.1631*DBH^1.4222)]
BC[Prov!="BC"& Species=="Picea mariana",
    ":="(AGBwood=0.0477*DBH^2.5147,
         AGBbark=0.0153*DBH^2.2429,
         AGBbranches=0.0278*DBH^2.0839,
         AGBfoliage=0.1648*DBH^1.4143)]
BC[Species=="Picea rubens",
    ":="(AGBwood=0.0989*DBH^2.2814,
         AGBbark=0.0220*DBH^2.0908,
         AGBbranches=0.0005*DBH^3.2750,
         AGBfoliage=0.0066*DBH^2.4213)]
BC[Species=="Picea sitchensis"|Species=="Picea abies"|Species=="Pinus albicaulis",
    ":="(AGBwood=0.0302*DBH^2.5776,
         AGBbark=0.0066*DBH^2.4433,
         AGBbranches=0.0739*DBH^1.8342,
         AGBfoliage=0.0157*DBH^2.3113)]
BC[Prov=="BC"&Species=="Pinus contorta",
    ":="(AGBwood=0.0323*DBH^2.6825,
         AGBbark=0.6825*DBH^2.1768,
         AGBbranches=0.0144*DBH^2.1772,
         AGBfoliage=0.0209*DBH^1.6432)]
BC[Prov!="BC"&Species=="Pinus contorta",
    ":="(AGBwood=0.0475*DBH^2.5437,
         AGBbark=0.0198*DBH^2.0807,
         AGBbranches=0.0144*DBH^2.1287,
         AGBfoliage=0.0432*DBH^1.7166)]
BC[Species=="Pinus strobus"|Species=="Pinus_sp",#for EWP (because Pinus_sp is from NFImm)
    ":="(AGBwood=0.0997*DBH^2.2709,
         AGBbark=0.0192*DBH^2.2038,
         AGBbranches=0.0056*DBH^2.6011,
         AGBfoliage=0.0284*DBH^1.9375)]
BC[Species=="Pinus banksiana",
    ":="(AGBwood=0.0804*DBH^2.4041,
         AGBbark=0.0184*DBH^2.0703,
         AGBbranches=0.0079*DBH^2.4155,
         AGBfoliage=0.0389*DBH^1.7290)]
BC[Species=="Pinus resinosa"|Species=="Pinus albicaulis"|Species=="Pinus flexilis"
   |Species=="Pinus monticola"|Species=="Pinus nigra"|Species=="Pinus sylvestris",#for RP (sbst PP),
    ":="(AGBwood=0.0564*DBH^2.4465,
         AGBbark=0.0188*DBH^2.0527,
         AGBbranches=0.0033*DBH^2.7515,
         AGBfoliage=0.0212*DBH^2.0690)]
BC[Prov=="BC"&(Species=="Populus balsamifera"|Species=="Arbutus menziesii"),#somehow this is for both BP & RA
    ":="(AGBwood=0.0460*DBH^2.4312,
         AGBbark=0.0074*DBH^2.4442,
         AGBbranches=0.0086*DBH^2.7326,
         AGBfoliage=0.0114*DBH^2.0860)]
BC[Prov!="BC"&Species=="Populus balsamifera",
    ":="(AGBwood=0.0510*DBH^2.4529,
         AGBbark=0.0297*DBH^2.1131,
         AGBbranches=0.0120*DBH^2.4165,
         AGBfoliage=0.0276*DBH^1.6215)]
BC[Species=="Populus grandidentata"|Species=="Populus_sp",
    ":="(AGBwood=0.0959*DBH^2.3430,
         AGBbark=0.0308*DBH^2.2240,
         AGBbranches=0.0047*DBH^2.6530,
         AGBfoliage=0.0080*DBH^2.0149)]
BC[Prov=="BC"&Species=="Populus tremuloides",
    ":="(AGBwood=0.0608*DBH^2.4735,
         AGBbark=0.0159*DBH^2.4123,
         AGBbranches=0.0082*DBH^2.5139,
         AGBfoliage=0.0235*DBH^1.6656)]
BC[Prov!="BC"&(Species=="Populus tremuloides"|Species=="Populus deltoides"),
    ":="(AGBwood=0.0605*DBH^2.4750,
         AGBbark=0.0168*DBH^2.3949,
         AGBbranches=0.0080*DBH^2.5214,
         AGBfoliage=0.0261*DBH^1.6304)]
BC[Species=="Prunus pensylvanica"|Species=="Prunus emarginata"|Species=="Prunus serotina"|Species=="Prunus virginiana"|Species=="Prunus",
    ":="(AGBwood=0.3743*DBH^2.3491,
         AGBbark=0.0323*DBH^2.0761,
         AGBbranches=0.0448*DBH^1.9771,
         AGBfoliage=0.0538*DBH^1.3584)]
BC[Species=="Pseudotsuga menziesii"|Species=="Pseudotsuga menziesii subsp. glauca"|Species=="Pseudotsuga menziesii menziesii",
    ":="(AGBwood=0.0204*DBH^2.6974,
         AGBbark=0.0069*DBH^2.5462,
         AGBbranches=0.0404*DBH^2.1388,
         AGBfoliage=0.1233*DBH^1.6636)]
BC[Species=="Quercus alba"|Species=="Quercus bicolor"|Species=="Quercus macrocarpa"|Species=="Quercus robur",
    ":="(AGBwood=0.0762*DBH^2.3335,
         AGBbark=0.0338*DBH^1.9845,
         AGBbranches=0.0113*DBH^2.6211,
         AGBfoliage=0.0188*DBH^1.7881)]
BC[Species=="Quercus rubra"|Species=="Quercus rubra var. borealis",
    ":="(AGBwood=0.1754*DBH^2.1616,
         AGBbark=0.0381*DBH^2.0991,
         AGBbranches=0.0085*DBH^2.7790,
         AGBfoliage=0.0373*DBH^1.6740)]
BC[Species=="Thuja plicata"|Species=="Thuja occidentalis",  # Eastern white cedar?  
    ":="(AGBwood=0.0111*DBH^2.8027,
         AGBbark=0.0003*DBH^3.2721,
         AGBbranches=0.1158*DBH^1.7196,
         AGBfoliage=0.1233*DBH^1.5152)]
BC[Species=="Tsuga canadensis",
    ":="(AGBwood=0.0562*DBH^2.4102,
         AGBbark=0.0302*DBH^2.0976,
         AGBbranches=0.0230*DBH^2.2382,
         AGBfoliage=0.0288*DBH^1.6378)]
BC[Species=="Tsuga heterophylla"|Species=="Tsuga mertensiana", #Western hemlock
    ":="(AGBwood=0.0141*DBH^2.8668,
         AGBbark=0.0025*DBH^2.8062,
         AGBbranches=0.0703*DBH^1.9547,
         AGBfoliage=0.1676*DBH^1.4339)]
BC[Species=="Ulmus americana"|Species=="Ulmus thomasii"|Species=="Ulmus rubra",#for white elm (subst for RE, SE)
    ":="(AGBwood=0.0402*DBH^2.5804,
         AGBbark=0.0073*DBH^2.4859,
         AGBbranches=0.0401*DBH^2.1826,
         AGBfoliage=0.0750*DBH^1.3436)]

BC[Species=="Betula populifolia",
    ":="(AGBwood=0.0720*DBH^2.3885,
         AGBbark=0.0168*DBH^2.2569,
         AGBbranches=0.0088*DBH^2.5689,
         AGBfoliage=0.0099*DBH^1.8985)]
BC[Species=="Carpinus caroliniana"|Species=="Amelanchier"|Species=="Amelanchier_sp"|
    Species=="Alnus incana"|Species=="Alnus incana ssp. rugosa"|Species=="Alnus  viridis"|Species=="Alnus rubra"|
    Species=="Carya ovata"|Species=="Cornus nuttallii"|Species=="Cornus rugosa"|Species=="Crataegus_sp"|
    Species=="Malus"|Species=="Malus_sp"|Species=="Malus fusca"|
    Species=="Rhamnus_sp"|Species=="Rhamnus"|Species=="Salix_sp"|Species=="Salix"|Species=="Salix scouleriana"|
    Species=="Sorbus americana"|Species=="Sorbus decora"|
    Species=="XH",
    ":="(AGBwood=0.0871*DBH^2.3702,#hardwoods coefs from Lambert 2005(because only few speceis coefs included in Ung2008)
         AGBbark=0.0241*DBH^2.1969,
         AGBbranches=0.0167*DBH^2.4807,
         AGBfoliage=0.0390*DBH^1.6229)]

BC[Species=="Taxus brevifolia"|Species=="Tilia americana"|Species=="XC",#softwoods coefs from Lambert 2005
    ":="(AGBwood=0.0648*DBH^2.3927,
         AGBbark=0.0162*DBH^2.1959,
         AGBbranches=0.0156*DBH^2.2916,
         AGBfoliage=0.0861*DBH^1.6261)]
table(BC[is.na(AGBwood),Species])#Hard-/soft-woods coefs from Lambert2005 were applied for these species

BC[,.N, by=Species][order(Species),]

#Sum all body parts to get Tree_Bio
BC[,Bio:=AGBwood+AGBbark+AGBbranches+AGBfoliage] 

BC<-BC[order(T_N, FinY),]
# BC[, prevDBH:=shift(DBH,n=1,type="lag"),by=T_N] 
# BC[, deltaDBH:=DBH-prevDBH] 
# BC[,.N, by= deltaDBH] [order(deltaDBH),]

# negBC<-BC[ which(BC$deltaDBH< -0.5), ]  
# posBC<-BC[ which(BC$deltaDBH> 5), ]  
                  
#BC[,BA:=(DBH/200)^2*pi]  ##in m2
# BC[,prevBA:=shift(BA,n=1,type="lag"),by=T_N]
#BC[, deltaBA:=BA-prevBA] 

BC[,prevBio:=data.table::shift(Bio,1,type="lag"), by=T_N] 
BC[,deltaBio:=Bio-prevBio,  by=T_N] 
BC[,.N, by= deltaBio] [order(deltaBio),] ##why so huge negative and positive values

##need to use quantitle to correct measurement errors

BC[, deltaBio := ifelse(!is.na(deltaBio) & deltaBio>quantile(is.na(deltaBio), 0.99), quantile(is.na(deltaBio), 0.99), deltaBio)]
BC[, deltaBio := ifelse(!is.na(deltaBio) & deltaBio<quantile(is.na(deltaBio), 0.01), quantile(is.na(deltaBio), 0.01), deltaBio)]

BC[, deltaBA := ifelse(!is.na(deltaBA) & deltaBA>quantile(is.na(deltaBA), 0.99), quantile(is.na(deltaBA), 0.99), deltaBA)]
BC[, deltaBA := ifelse(!is.na(deltaBA) & deltaBA<quantile(is.na(deltaBA), 0.01), quantile(is.na(deltaBA), 0.01), deltaBA)]

BC[,.N, by= Age] [order(Age),]

#BC<-BC[Age>10|is.na(Age) ==TRUE]
#BC<-BC[deltaBio>=0|is.na(deltaBio)==TRUE]
hist(BC$DBH, breaks=1000)

BC0<-BC[, c("AGBwood","AGBbark","AGBbranches","AGBfoliage"):=NULL]

summary(BC0)

write.csv(BC0, "BC_Ind2022.csv",row.names=F)

##consider insect and disease influenced basal area
