title: "Unintended Changes caused by Dicamba Drift"

```{r}
Dicamba = read.csv("~/Desktop/Dicamba20192018_.csv", fill = TRUE)

Dicamba$TRT <- as.factor(Dicamba$TRT)
Dicamba$POP <- as.factor(Dicamba$POP)
Dicamba$BLK <- as.factor(Dicamba$BLK)
Dicamba$ID <- as.factor(Dicamba$ID)
Dicamba$Year <- as.factor(Dicamba$Year)
Dicamba$ML <- as.factor(Dicamba$ML)

Licor = read.csv("~/Desktop/LICOR_C_2018.csv", fill = TRUE)
Licor$Trt <- as.factor(Licor$Trt)
Licor$Leaf <- as.factor(Licor$Leaf)
Licor$BLK <- as.factor(Licor$BLK)
Licor$ML <- as.factor(Licor$ML)
Licor$Herbicide.1 <- as.factor(Licor$Herbicide.1)
```

install.packages("ggplot2")
library("ggplot2")
install.packages("plyr")
library("plyr")
install.packages("tidyr")
library("tidyr")
install.packages("car")
library("car")
install.packages("lsmeans")
library("lsmeans")
install.packages("lmerTest")
library("lmerTest")
install.packages("rcompanion")
library("rcompanion")

######################### Example from Gina #########################################################

lmm <- lmer(SeedTotal ~ selftrt*slnline + blk + (1|mom), data=seeds2)

plot(lmm)
qqnorm(resid(lmm))

summary(lmm)
anova(lmm)
Anova(lmm, type = 'III')
Anova(lmm, type = 'II')

lsmeansLT(lmm, "selftrt")
lsmeansLT(lmm, "slnline")
lsmeansLT(lmm, "blk")
(2468.3-2121.7)/2468.3

test <-lsmeansLT(lmm, test.effs="selftrt:slnline")
test2 <- test$lsmeans.table


############################### ADDED CALCUTAIONS IN DATASET############################################
#Resistance Estimates
Dicamba$WflyResistance <- 1-Dicamba$Wfly2
Dicamba$HerbResistance <- 1-Dicamba$HerbDamage
Dicamba$GrowthRate <- ((Dicamba$Leaf3 - Dicamba$Leaf1) + (Dicamba$Height3 - Dicamba$Height1)) / 16

#For Genteic Correlations
Dicamba_aveg2 <- ddply(DY2019, .(ML, POP), summarise, 
                      meanHerbRes=mean(HerbResistance, na.rm=TRUE), meanWflyRes=mean(WflyResistance, na.rm=TRUE),
                      meanGrowthRate=mean(GrowthRate, na.rm=TRUE), meanRelativeFit=mean(RelFitness, na.rm=TRUE) )
Dicamba_aveg3 <- ddply(DY2018, .(ML), summarise, 
                       meanHerbRes=mean(HerbResistance, na.rm=TRUE), meanWflyRes=mean(WflyResistance, na.rm=TRUE),
                       meanGrowthRate=mean(GrowthRate, na.rm=TRUE), meanRelativeFit=mean(RelFitness, na.rm=TRUE) )
Dicamba_aveg4 <- ddply(DY, .(Year, ML), summarise, 
                       meanHerbRes=mean(StdHerbRes, na.rm=TRUE), meanWflyRes=mean(StdWflyRes, na.rm=TRUE),
                       meanGrowthRate=mean(StdGrowth, na.rm=TRUE), meanRelativeFit=mean(StdFit, na.rm=TRUE) )
Dicamba_aveg5 <- ddply(DY, .(Year, ML), summarise, 
                       meanHerbRes=mean(HerbResistance, na.rm=TRUE), meanWflyRes=mean(WflyResistance, na.rm=TRUE),
                       meanGrowthRate=mean(RelGrowth, na.rm=TRUE), meanRelativeFit=mean(RelFitness, na.rm=TRUE) )

#Relative Fitness and Realtive Growth Rate
Dicamba_aveg <- ddply(Dicamba, .(BLK), summarise, 
                   meanFitness=mean(Seeds, na.rm=TRUE))
Dicamba_aveg1 <- ddply(Dicamba, .(BLK), summarise, 
                       meanGrowthRate=mean(GrowthRate, na.rm=TRUE))

Lic_aveg <- ddply(Lic, .(Trt, Leaf), summarise, 
                       meanA1=mean(Photo, na.rm=TRUE), meanC=mean(Ci, na.rm=TRUE))

Dicamba <- merge(Dicamba, Dicamba_aveg, by=c("BLK"))
Dicamba <- merge(Dicamba, Dicamba_aveg1, by=c("BLK"))
Lic <- merge(Lic_aveg, Lic, by=c("Trt", "Leaf"))

Dicamba$RelFitness <- Dicamba$Seeds/(Dicamba$meanFitness)
Dicamba$RelGrowth <- Dicamba$GrowthRate/(Dicamba$meanGrowthRate)

Lic$ACI <- Lic$Photo/Lic$Ci


############################### SUBSETS ##############################################################
#By Year with all Treatments
Y2018 = subset(Dicamba, Year%in%c("2018"))
Y2019 = subset(Dicamba, Year%in%c("2019"))

#By Year and Dicamba only Treatments 
DY2018 = subset(Dicamba, TRT%in%c("1", "0.5") & Year%in%c("2018"))
DY2019 = subset(Dicamba, TRT%in%c("1", "0.5") & Year%in%c("2019"))

DTRT = subset(Dicamba, TRT%in%c("0", "0.5") & Year%in%c("2018"))
DTRT1 = subset(Dicamba, TRT%in%c("1", "0.5") & Year%in%c("2018"))
DTRT2 = subset(Dicamba, TRT%in%c("1", "0") & Year%in%c("2018"))

DTRT3 = subset(Dicamba, TRT%in%c("0", "0.5") & Year%in%c("2019"))
DTRT4 = subset(Dicamba, TRT%in%c("1", "0.5") & Year%in%c("2019"))
DTRT5 = subset(Dicamba, TRT%in%c("1", "0") & Year%in%c("2019"))

#Removed Block 4
DY = subset(Dicamba, TRT%in%c("1", "0.5"))
DY0 = subset(Dicamba, TRT%in%c("0"))
DY05 = subset(Dicamba, TRT%in%c("0.5"))
DY1 = subset(Dicamba, TRT%in%c("1"))

#POP 1 and block 4 removed 
DWY = subset(Dicamba, TRT%in%c("1", "0.5") & BLK%in%c( "3" , "5") & POP%in%c( "2", "3", "4", "5", "6"))

#Both Years and Control Only Treatments
TRT019 = subset(Dicamba, TRT%in%c("0"))
TRT018 = subset(Dicamba, TRT%in%c("0") & Year%in%c("2018"))
TRT019 = subset(Dicamba, TRT%in%c("0") & Year%in%c("2019"))

#Licor Dicamba Treatments only 
Lic = subset(Licor, Trt%in%c("0.5", "1", "0"))
Lic.5 = subset(Lic, Trt%in%c("0.5"))
Lic1 = subset(Lic, Trt%in%c("1"))
Lic0 = subset(Lic, Trt%in%c("0"))
Lic15 = subset(Lic, Trt%in%c("0.5", "1"))
###################### TRANSFORMATIONS ##############################################################
#Whitefly Abundance Transformed for all treatments
WflyAb1 <- transformTukey(Y2018$Wfly2) #p = 0.0047
WflyAb2 <- transformTukey(Y2019$Wfly2) #p = 8.798e-05

WflyAb3 <- transformTukey(DTRT2$Wfly2)
WflyAb4 <- transformTukey(DTRT3$Wfly2)
WflyAb5 <- transformTukey(DTRT4$Wfly2)
WflyAb6 <- transformTukey(DTRT5$Wfly2)

#Whitefly Resistance Transformed for all treatments for phenotypic selection 
WflyRes <- transformTukey(DY$WflyResistance) #p = 0.0001961
WflyRes1 <- transformTukey(Y2018$WflyResistance) # p = 0.002052
WflyRes2 <- transformTukey(Y2019$WflyResistance) # p = 0.001147

#Whitefly Resistance Transformed for Dicamba Treatments for phenotypic correlations 
WflyRes3 <- transformTukey(DY2018$WflyResistance) # p =  0.02048
WflyRes4 <- transformTukey(DY2019$WflyResistance) # p =   0.07559

#Chewing Damage Transformed for all treatments
ChewDam <- transformTukey(DTRT$ChewDamage) # 0.0001972
ChewDam1 <- transformTukey(Y2018$ChewDamage)

#Relative Fitness for All 
Rel_Fit <- transformTukey(Dicamba$RelFitness) # p = 6.225e-08

#Relative Fitness for Dicamba Treatments
Rel_Fit1 <- transformTukey(DY$RelFitness) # p =  8.399e-06

#Relative Fitness Transformed for Dicamba by Year
Rel_Fit3 <- transformTukey(DY2018$RelFitness) # P = 0.08927
Rel_Fit4 <- transformTukey(DY2019$RelFitness) # p = 8.415e-06

#Relative Fitness Transformed by year  
Rel_Fit5 <- transformTukey(Y2018$RelFitness) # p = 0.02807
Rel_Fit6 <- transformTukey(Y2019$RelFitness) # p = 7.963e-08

# By individual treatments

Rel_fit6 <- transformTukey(DY0$RelFitness) # p = 0.0002705
Rel_fit7 <- transformTukey(DY05$RelFitness) # p = 0.000896
Rel_fit8 <- transformTukey(DY1$RelFitness) # p = 0.0001901

Rel_Fit7 <- transformTukey(DWY$RelFitness) # p =  0.04219

#GrowthRate Dicamba by Year 
Growth1 <- transformTukey(DY$RelGrowth) # p = 0.449 
Growth2 <- transformTukey(DY2019$RelGrowth) # p = 0.7895

#Growth Rate by Year all
Growth3 <- transformTukey(Y2018$RelGrowth) # p = 0.3998
Growth4 <- transformTukey(Y2019$RelGrowth) # p = 0.3487

HerbRes <- transformTukey(DY$HerbResistance) #p = 7.156e-07

A<-transformTukey(Lic$ACI) #p = 6.972e-10
A2<-transformTukey(Lic15$ACI) #p = 8.005e-09

################### 1) HERBIVORY RESPONSE TO HERBICIDE DRIFT ON HOST #################################
#Linear mixed model for Whitefly Adundance Analysis of Variables for 2018 *GUCCI*
linmmodl1 <- lmer(WflyAb1 ~  TRT * BLK + (1|ML) *TRT + (1|ML), data = Y2018, na.action = na.exclude)
linmmodl1.1 <- lmer(WflyAb1 ~  TRT * BLK + (1|ML) *TRT, data = Y2018, na.action = na.exclude)
linmmodl1.2 <- lmer(WflyAb1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
linmmodl1.3 <- lm(WflyAb1 ~  TRT * BLK, data = Y2018, na.action = na.exclude)
anova (linmmodl1,linmmodl1.1, linmmodl1.2, linmmodl1.3, test= "Chisq")
Anova(linmmodl1, type = "III", test.statistic="F") # sig trt effects p = 2.331e-06
summary(linmmodl1) 
hist(resid(linmmodl1.2))
shapiro.test(resid(linmmodl1.2)) #p = 0.09017
#differences bteween dicamba treatments
t.test(transformTukey(DY2018$Wfly) ~ TRT, data =DY2018) # p = 0.01255

#Linear mixed model for Chewing Damage Analysis of Variables for 2018 *GUCCI-ish*
linmmodl2 <- lmer(ChewDam1 ~  TRT * BLK + (1|ML)*TRT + (1|ML), data = Y2018, na.action = na.exclude)
linmmodl2.1 <- lmer(ChewDam1 ~  TRT * BLK  + (1|ML)*TRT, data = Y2018, na.action = na.exclude)
linmmodl2.2 <- lmer(ChewDam1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
linmmodl2.3 <- lm(ChewDam1 ~  TRT * BLK , data = Y2018, na.action = na.exclude)
anova (linmmodl2,linmmodl2.1, linmmodl2.2, linmmodl2.3, test= "Chisq")
Anova(linmmodl2, type = "III", test.statistic="F")
summary(linmmodl2) #Significant block effects 
hist(resid(linmmodl2.3))
shapiro.test(resid(linmmodl2.3)) #p = 0.05613

#Linear mixed model for Whitefly Abundance Analysis of Variables for 2019 *GUCCI*
linmmodl5 <- lmer(WflyAb2 ~  TRT * BLK + (1 |POP:ML) * TRT + (1 |POP:ML) + (1|POP) + (1|POP)*TRT, data = Y2019, na.action = na.exclude)
linmmodl5.1 <- lmer(WflyAb2 ~  TRT * BLK + (1 |POP:ML) * TRT + (1 |POP:ML), data = Y2019, na.action = na.exclude)
linmmodl5.2 <- lmer(WflyAb2 ~  TRT * BLK + (1 |POP)*TRT, data = Y2019, na.action = na.exclude)
linmmodl5.3 <- lm(WflyAb2 ~  TRT * BLK , data = Y2019, na.action = na.exclude)
anova (linmmodl5,linmmodl5.1, linmmodl5.2, linmmodl5.3, test= "Chisq")
Anova(linmmodl5.1, type = "III", test.statistic="F") # sig trt effects p = 0.003372
summary(linmmodl5)
hist(resid(linmmodl5))
shapiro.test(resid(linmmodl5)) #p = 0.2971
#differences bteween dicamba treatments
t.test(transformTukey(DY2019$Wfly) ~ TRT, data =DY2019) # p = 0.04722

linmmodl5 <- lmer(WflyRes2 ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
linmmodl5.1 <- lm(WflyRes2 ~  TRT * BLK , data = Y2019, na.action = na.exclude)
anova (linmmodl5,linmmodl5.1, test= "Chisq")
Anova(linmmodl5, type = "III", test.statistic="F") # sig trt effects p = 0.003372
summary(linmmodl5)
hist(resid(linmmodl5))
shapiro.test(resid(linmmodl5)) #p = 0.2971
#differences bteween dicamba treatments
t.test(transformTukey(DY2019$Wfly) ~ TRT, data =DY2019) # p = 0.04722

linmmodl5 <- lm(Herb_Res ~  POP, data = DWY, na.action = na.exclude)
linmmodl5.1 <- lm(WflyAb2 ~  TRT * BLK , data = Y2019, na.action = na.exclude)
anova (linmmodl5,linmmodl5.1, test= "Chisq")
Anova(linmmodl5, type = "III", test.statistic="F") # sig trt effects p = 0.003372
summary(linmmodl5)
hist(resid(linmmodl5))
shapiro.test(resid(linmmodl5)) #p = 0.2971
#differences bteween dicamba treatments
t.test(transformTukey(DY2019$Wfly) ~ TRT, data =DY2019) # p = 0.04722



################### 2) PLANT PERFORMANCE RESPONSE TO HERBICIDE DRIFT BY PLANT ################################


#Height for yr1 - not super normal residuals
hist(Y2018$Height3)
Height1 <- transformTukey(Y2018$Height3) #p = 0.1897
lmer3 <- lmer(Height1 ~  TRT * BLK + (1 | ML) *TRT, data = Y2018, na.action = na.exclude)
lmer3.1 <- lmer(Height1 ~  TRT * BLK + (1 | ML), data = Y2018, na.action = na.exclude)
lm3.2 <- lm(Height1 ~  TRT * BLK , data = Y2018, na.action = na.exclude)
anova (lmer3,lmer3.1, lm3.2, test= "Chisq") #random effect not sig
Anova(lmer3, type = "III", test.statistic="F") 
summary(lmer3) 

hist(resid(lm3.2))
shapiro.test(resid(lmer3)) # p = 0.03952

t.test(DY$TRT, DY$Height3)

#Height for yr2 - not super normal residuals
hist(Y2019$Height3)
Height2 <- transformTukey(Y2019$Height3) #p = 0.01059
lmer4 <- lmer(Height2 ~  TRT * BLK + (1|POP) * TRT + (1 |POP:ML)* TRT, data = Y2019, na.action = na.exclude)
lmer4.1 <- lmer(Height2 ~  TRT * BLK + (1 |POP:ML)* TRT, data = Y2019, na.action = na.exclude)
lmer4.2 <- lmer(Height2 ~  TRT * BLK + (1 |POP)* TRT, data = Y2019, na.action = na.exclude)
lmer4.3 <- lmer(Height2 ~  TRT * BLK + (1 |POP), data = Y2019, na.action = na.exclude)
lmer4.4 <- lmer(Height2 ~  TRT * BLK + (1 |POP:ML), data = Y2019, na.action = na.exclude)
lm4.2 <- lm(Height2 ~  TRT * BLK, data = Y2019, na.action = na.exclude)
anova (lmer4,lmer4.1, lmer4.2, lmer4.3, lmer4.3, lmer4.4, lm4.2, test= "Chisq") #random effect not sig
Anova(lmer4, type = "III", test.statistic="F") 
summary(lmer4) 
hist(resid(lmer4.2))
shapiro.test(resid(lmer4)) # p = 0.03228

#Leaf count for yr 1
hist(Y2018$Leaf3)
Leaf1 <- transformTukey(Y2018$Leaf3) #p =  0.03505
lmer1 <- lmer(Leaf1 ~  BLK* TRT + (1 | ML) * TRT, data = Y2018, na.action = na.exclude)
lmer1.1 <- lmer(Leaf1 ~  BLK* TRT + (1 | ML), data = Y2018, na.action = na.exclude)
lm1.2 <- lm(Leaf1 ~  TRT * BLK , data = Y2018, na.action = na.exclude)
anova (lmer1,lmer1.1, lm1.2, test= "Chisq") 
Anova(lmer1, type = "III", test.statistic="F") 
summary(lmer1)
hist(resid(lmer1))
shapiro.test(resid(lmer1)) #p-value = 0.107

#Leaf count for yr 2
hist(Y2019$Leaf3)
Leaf2 <- transformTukey(Y2019$Leaf3) #p = 0.0364
lmer2 <- lmer(Leaf2 ~  TRT * BLK +   (1|POP)*TRT + (1|POP:ML)*TRT, data = Y2019, na.action = na.exclude)
lmer2.1 <- lmer(Leaf2 ~  TRT * BLK +  (1|POP)*TRT , data = Y2019, na.action = na.exclude)
lmer2.2 <- lmer(Leaf2 ~  TRT * BLK + (1|POP:ML)*TRT , data = Y2019, na.action = na.exclude)
lmer2.3 <- lmer(Leaf2 ~  TRT * BLK + (1|POP) + (1|POP:ML), data = Y2019, na.action = na.exclude)
lmer2.4 <- lmer(Leaf2 ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
lm2.4 <- lm(Leaf2 ~  TRT * BLK  , data = Y2019, na.action = na.exclude)
anova (lmer2,lmer2.1, lmer2.2, lmer2.3,lmer2.4, lm2.4, test= "Chisq") 
Anova(lmer2, type = "III", test.statistic="F") 
summary(lmer2.2) 
hist(resid(lmer2))
shapiro.test(resid(lmer2)) #p-value = 0.4521

#Width for yr1 
hist(Y2018$Width.Avg)
Width1 <- transformTukey(Y2018$Width.Avg) #p = 0.6734
lmer5 <- lmer(Width1 ~  TRT * BLK + (1|ML) * TRT, data = Y2018, na.action = na.exclude)
lmer5.1 <- lmer(Width1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
lm5.2 <- lm(Width1 ~  TRT * BLK , data = Y2018, na.action = na.exclude)
anova (lmer5,lmer5.1,lm5.2, test= "ChiSq") 
Anova(lmer5, type = "III", test.statistic="F") 
summary(lm5.2)
hist(resid(lmer5))
shapiro.test(resid(lmer5)) # p = 0.9769

#Width for yr2 
hist(Y2019$Width.Avg)
Width2 <- transformTukey(Y2019$Width.Avg) #p = 0.9049
lmer6 <- lmer(Width2 ~  TRT * BLK +   (1|POP)*TRT + (1|POP:ML)*TRT, data = Y2019, na.action = na.exclude)
lmer6.1 <- lmer(Width2 ~  TRT * BLK +  (1|POP)*TRT , data = Y2019, na.action = na.exclude)
lmer6.2 <- lmer(Width2 ~  TRT * BLK + (1|POP:ML)*TRT , data = Y2019, na.action = na.exclude)
lmer6.3 <- lmer(Width2 ~  TRT * BLK + (1|POP) + (1|POP:ML), data = Y2019, na.action = na.exclude)
lmer6.4 <- lmer(Width2 ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
lm6.4 <- lm(Width2 ~  TRT * BLK  , data = Y2019, na.action = na.exclude)
anova (lmer6 ,lmer6.1, lmer6.2, lmer6.3, lmer6.4, lm6.4, test= "Chisq") 
Anova(lmer6, type = "III", , test.statistic="F") 
summary(lm6.2)
hist(resid(lm6.2))
shapiro.test(resid(lmer6.1)) # p = 0.8886

#Flower Number for yr1 
hist(Y2018$Flower)
Flower1 <- transformTukey(Y2018$Flower) #p = 0.04166
lmer7 <- lmer(Flower1 ~  TRT * BLK + (1|ML) * TRT, data = Y2018, na.action = na.exclude)
lmer7.1 <- lmer(Flower1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
lm7.2 <- lm(Flower1 ~  TRT * BLK, data = Y2018, na.action = na.exclude)
anova (lmer7,lmer7.1,lm7.2, test= "Chisq") 
Anova(lmer7, type = "III", test.statistic="F")  #trt p = 1.407e-05 block p = 0.003048 and trt:blk p = 0.002668
summary(lm7.2)
hist(resid(lmer7))
shapiro.test(resid(lmer7)) # p = 0.3161

#Flower Number for yr2 -> non- normal 
hist(Y2019$Flower)
Flower2 <- transformTukey(Y2019$Flower) #p = 9.023e-19
lmer8 <- lmer(Flower2 ~  TRT * BLK +   (1|POP)*TRT + (1|POP:ML)*TRT, data = Y2019, na.action = na.exclude)
lmer8.1 <- lmer(Flower2 ~  TRT * BLK +  (1|POP)*TRT , data = Y2019, na.action = na.exclude)
lmer8.2 <- lmer(Flower2 ~  TRT * BLK + (1|POP:ML)*TRT , data = Y2019, na.action = na.exclude)
lmer8.3 <- lmer(Flower2 ~  TRT * BLK + (1|POP) , data = Y2019, na.action = na.exclude)
lmer8.4 <- lmer(Flower2 ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
lm8.4 <- lm(Flower2 ~  TRT * BLK  , data = Y2019, na.action = na.exclude)
anova (lmer8, lmer8.2, lmer8.3, lmer8.4,lm8.4, test= "Chisq") 
Anova(lmer8, type = "III", test.statistic="F")  #trt p = 0.001697 
summary(lmer8)
hist(resid(lmer8))
shapiro.test(resid(lmer8)) # p = 1.823e-07

#Seed Cont for yr1
hist(Y2018$Seeds)
Seed1 <- transformTukey(Y2018$Seeds) #p = 0.01245
lmer9 <- lmer(Seed1 ~  TRT * BLK + (1|TRT:ML), data = Y2018, na.action = na.exclude)
lmer9.1 <- lmer(Seed1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
lm9.2 <- lm(Seed1 ~  TRT * BLK, data = Y2018, na.action = na.exclude)
anova (lmer9,lmer9.1, lm9.2, test= "Chisq") 
Anova(lmer9, type = "III", test.statistic="F") 
summary(lmer9) #only block effect
hist(resid(lmer9))
shapiro.test(resid(lmer9)) #p = 0.4109

#Seed Count for yr2
hist(Y2019$Seeds)
Seed2 <- transformTukey(Y2019$Seeds) #p = 1.24e-12
lmer10 <- lmer(Seed2 ~  TRT * BLK + (1|TRT:POP:ML), data = Y2019, na.action = na.exclude)
lmer10.1 <- lmer(Seed2 ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
lm10.2 <- lm(Seed2 ~  TRT * BLK , data = Y2019, na.action = na.exclude)
anova (lmer10,lmer10.1, lm10.2, test= "Chisq") 
Anova(lmer10, type = "III", test.statistic="F") 
summary(lmer10) #only block effect
hist(resid(lmer10))
shapiro.test(resid(lmer10)) #p = 5.574e-08

#C:N for Yr 2 
hist(Y2019$C.N)
CN <- transformTukey(Y2019$C.N) #p =  0.6339
lmer11 <- lmer(CN ~  TRT * BLK + (1|POP:ML), data = Y2019, na.action = na.exclude)
lmer11.1 <- lm(CN ~  TRT * BLK, data = Y2019, na.action = na.exclude)
anova (lmer11,lmer11.1, test= "Chisq") 
Anova(lmer11, type = "III", test.statistic="F")  #blk p = 0.001901
summary(lmer11) # sig intercept at control
hist(resid(lmer11))
shapiro.test(resid(lmer11)) # p = 0.8048

# Photosythetic Assimilation
lmer12 <- lmer(A ~ Trt * BLK + (1|Trt:ML), data = Lic, na.action = na.exclude)
lmer12.1 <- lmer(A ~ Trt * BLK + (1|ML), data = Lic, na.action = na.exclude)
lm12.2 <- lm(A ~  Trt  * BLK,  data = Lic, na.action = na.exclude)
anova (lmer12,lmer12.1,lm12.2, test= "Chisq")
Anova(linmmodl6, type = "III", test.statistic="F") # sig trt effects p = 0.003372
summary(linmmodl6)
hist(resid(linmmodl6))
shapiro.test(resid(linmmodl6))

linmmodl7 <- lmer(A2 ~ Trt  * Leaf * BLK + (1|ML), data = Lic15, na.action = na.exclude)
linmmodl7.1 <- lm(A2 ~  Trt  * Leaf * BLK, data = Lic15, na.action = na.exclude)
anova (linmmodl7,linmmodl7.1, test= "Chisq")
Anova(linmmodl7, type = "III", test.statistic="F") # sig trt effects p = 0.003372
summary(linmmodl7)
hist(resid(linmmodl7))
shapiro.test(resid(linmmodl7))

#Growth Rate 2018
Growth1 <- transformTukey(Y2018$RelGrowth)
lmer13 <- lmer(Growth1 ~  TRT * BLK  + (1|ML)*TRT, data = Y2018, na.action = na.exclude)
lmer13.4 <- lmer(Growth1 ~  TRT * BLK + (1|ML), data = Y2018, na.action = na.exclude)
lm13.4 <- lm(Growth1 ~  TRT * BLK  , data = Y2018, na.action = na.exclude)
anova (lmer13,lmer13.4, lm13.4, test= "Chisq") 
Anova(lmer13, type = "III", test.statistic="F")  #trt p = 0.001697 
summary(lmer13) 
hist(resid(lmer13.1))

#Growth Dicamba all
Growth2 <- transformTukey(DY$GrowthRate)
lmer13 <- lmer(Growth2 ~  TRT * BLK + (1|POP)*TRT + (1|POP:ML)*TRT, data = DY, na.action = na.exclude)
lmer13.1 <- lmer(Growth2 ~  TRT * BLK +  (1|POP)*TRT , data = DY, na.action = na.exclude)
lmer13.2 <- lmer(Growth2 ~  TRT * BLK + (1|POP:ML)*TRT , data = DY, na.action = na.exclude)
lmer13.3 <- lmer(Growth2 ~  TRT * BLK + (1|POP) , data = DY, na.action = na.exclude)
lmer13.4 <- lmer(Growth2 ~  TRT * BLK + (1|POP:ML), data = DY, na.action = na.exclude)
lm13.4 <- lm(Growth2 ~  TRT * BLK  , data = DY, na.action = na.exclude)
anova (lmer13.1, lmer13.2,lmer13.3, lmer13.4,lm13.4, test= "Chisq") 
Anova(lmer13, type = "III", test.statistic="F") 
summary(lmer14)
hist(resid(lmer14))

#Whitefly Resistance Dicamba all
WflyRes <- transformTukey(DY$WflyResistance)
lmer14 <- lmer(WflyRes ~  TRT * BLK + (1|POP)*TRT + (1|POP:ML)*TRT, data = DY, na.action = na.exclude)
lmer14.1 <- lmer(WflyRes ~  TRT * BLK +  (1|POP)*TRT , data = DY, na.action = na.exclude)
lmer14.2 <- lmer(WflyRes ~  TRT * BLK + (1|POP:ML)*TRT , data = DY, na.action = na.exclude)
lmer14.3 <- lmer(WflyRes ~  TRT * BLK + (1|POP) , data = DY, na.action = na.exclude)
lmer14.4 <- lmer(WflyRes ~  TRT * BLK + (1|POP:ML), data = DY, na.action = na.exclude)
lm14.4 <- lm(WflyRes ~  TRT * BLK  , data = DY, na.action = na.exclude)
Anova(lmer14, type = "III", test.statistic="F") 
anova (lmer14,lmer14.1, lmer14.2, lmer14.3, lmer14.4, lm14.4, test= "Chisq") 

summary(lmer14)
hist(resid(lmer14))


#Herb Resistance Dicamba all
HerbRes <- transformTukey(DY$HerbResistance)
lmer15 <- lmer(HerbRes ~  TRT * BLK  + (1|POP)*TRT + (1|POP:ML)*TRT, data = DY, na.action = na.exclude)

lmer15.1 <- lmer(HerbRes ~  TRT * BLK +  (1|POP)*TRT , data = DY, na.action = na.exclude)
lmer15.2 <- lmer(HerbRes ~  TRT * BLK + (1|POP:ML)*TRT , data = DY, na.action = na.exclude)
lmer15.3 <- lmer(HerbRes ~  TRT * BLK + (1|POP) , data = DY, na.action = na.exclude)
lmer15.4 <- lmer(HerbRes ~  TRT * BLK + (1|POP:ML), data = DY, na.action = na.exclude)
lm15.4 <- lm(HerbRes ~  TRT * BLK  , data = DY, na.action = na.exclude)
anova (lmer15, lmer15.1, lmer15.2, lmer15.2, lmer15.3, lmer15.4, lm15.4, test= "Chisq") 
Anova(lmer15, type = "III", test.statistic="F") 
summary(lmer14)
hist(resid(lmer14))


#Growth Rate Dicamba 2018 
lmer15 <- lmer(Growth1 ~  TRT * BLK + (1|TRT:ML), data = DY2018, na.action = na.exclude)
lmer15.1 <- lmer(Growth1 ~  TRT * BLK + (1|ML), data = DY2018, na.action = na.exclude)
lm15.2 <- lm(Growth1 ~  TRT * BLK , data = DY2018, na.action = na.exclude)
anova (lmer15,lmer15.1, test= "Chisq") 
Anova(lmer15.1, type = "III", test.statistic="F")  #trt p = 0.001697 
summary(lmer15.1)
hist(resid(lmer15.1))

#Growth Dicamba 2019 
lmer16 <- lmer(Growth2 ~  TRT + (1|POP/ML), data = DY2019, na.action = na.exclude)
lmer16.1 <- lm(Growth2 ~  TRT  , data = DY2019, na.action = na.exclude)
anova (lmer16,lmer16.1, test= "Chisq") #chisq 0.9861
Anova(lmer16, type = "III", test.statistic="F") 
summary(lmer16)
hist(resid(lmer16.1))

################### 3) GENETIC VARITION AMONG POPULATIONS #####################################
#Variation for Whitefly Resistance 
WflySec <-lmer(WflyRes ~  TRT * BLK + (1|POP) , data = DY, na.action = na.exclude)
WflySec2 <-lm(WflyRes ~  TRT * BLK , data = DY, na.action = na.exclude)
anova(WflySec, WflySec2) #Not Sig

Anova(WflySec2, type = "III", test.statistic="F") 
summary(WflySec2)
hist(resid(WflySec2))
shapiro.test(resid(WflySec2))

#Variation for Herbicide Resistance 
HerbSec <-lmer(HerbRes ~  TRT * BLK  + (1|POP) , data = DY, na.action = na.exclude)
HerbSec1 <-lm(HerbRes ~  TRT * BLK  , data = DY, na.action = na.exclude)
anova(HerbSec, HerbSec1) #Sign

Anova(HerbSec, type = "III", test.statistic="F") 
summary(HerbSec)
hist(resid(HerbSec))
shapiro.test(resid(HerbSec))

#Variation for Growth Rate
CSec <-lmer(ChewDam ~  TRT * BLK  + (1|ML) , data = DTRT, na.action = na.exclude)
CSec1 <-lm(ChewDam ~  TRT * BLK , data = DTRT, na.action = na.exclude)
anova(CSec, CSec1) #Sign

Anova(CSec1, type = "III", test.statistic="F") # sign wfly:herb:trt p =0.05
summary(CSec1)
hist(resid(CSec))
shapiro.test(resid(GSec))


############################ 4) CORRELATIONS #########################
corrplot(data.corr$r, method = "circle", tl.col="black", tl.cex=.9, tl.srt=70)
corrplot(data.corr$r, add=T, type="lower", method="number",
         col="black", diag=F, tl.pos="n", cl.pos="n", tl.cex=0.7)
col <- colorRampPalette(c("coral", "white", "skyblue"))

#All ML Standardized
data <- data.frame(Dicamba_aveg4$meanHerbRes, Dicamba_aveg4$meanGrowthRate, 
                   Dicamba_aveg4$meanWflyRes, Dicamba_aveg4$meanRelativeFit)
data.corr = rcorr(as.matrix(data),type = c("pearson"))
colnames(data.corr$r) <- c( "Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")
rownames(data.corr$r) <- c("Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")

data.p = data.corr$P
corrplot1 <- corrplot(data.corr$r, method="color", col=col(200),  
                         type="upper", order="hclust", 
                         addCoef.col = "black", # Add coefficient of correlation
                         tl.col="black", tl.srt=50, #Text label color and rotation
                         p.mat = data.corr$P , sig.level = 0.055, insig = "blank",
                         # hide correlation coefficient on the principal diagonal
                         diag=FALSE) 

p#All ML 
data1 <- data.frame(Dicamba_aveg5$meanHerbRes, Dicamba_aveg5$meanGrowthRate, 
                    Dicamba_aveg5$meanWflyRes, Dicamba_aveg5$meanRelativeFit)
data.corr2 = rcorr(as.matrix(data1),type = c("pearson"))
colnames(data.corr2$r) <- c( "Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")
rownames(data.corr2$r) <- c("Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")

data.p2 = data.corr2$P
corrplot2 <- corrplot(data.corr2$r, method="color", col=col(200),  
                      type="upper", order="hclust", 
                      addCoef.col = "black", # Add coefficient of correlation
                      tl.col="black", tl.srt=50, #Text label color and rotation
                      p.mat = data.corr2$P , sig.level = 0.055, insig = "blank",
                      # hide correlation coefficient on the principal diagonal
                      diag=FALSE) 
#All Dicamba

data3 <- data.frame(DY$HerbResistance, DY$RelGrowth, 
                    DY$WflyResistance, DY$RelFitness)
data.corr3 = rcorr(as.matrix(data3),type = c("pearson"))
colnames(data.corr3$r) <- c( "Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")
rownames(data.corr3$r) <- c("Herbicide Resistance","Relative Growth", "Whitefly Resistance","Relative Fitness")

data.p3 = data.corr3$P
corrplot3 <- corrplot(data.corr3$r, method="color", col=col(200),  
                      type="upper", order="hclust", 
                      addCoef.col = "black", # Add coefficient of correlation
                      tl.col="black", tl.srt=50, #Text label color and rotation
                      p.mat = data.corr3$P , sig.level = 0.055, insig = "blank",
                      # hide correlation coefficient on the principal diagonal
                      diag=FALSE) 

############################### Adaptive Landscape for Growth and Herbicide Resistance #################
adapt1 <- data.frame(DY$RelGrowth, DY$HerbRes, DY$RelFitness)
adapt2<- na.omit(adapt1)
adapt3 <- data.frame(adapt2$DY.RelGrowth, adapt2$DY.HerbRes)
adapt1.corr = as.matrix(adapt3)
colnames(adapt1.corr) <- c("Relative Growth", "Herbicide Resistance")
Herb.Growth<- Tps(adapt1.corr, adapt2$DY.RelFitness, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
    miles = TRUE, method = "GCV", GCV = TRUE)

pal <- wes_palette("Zissou1", 100, type = "continuous")

#surface plot
Herb.surface.Growth <- surface(Herb.Growth, type="p", xlab="Growth", ylab="Herbicide Resistance", zlab="RelFit", add.legend = FALSE, col= pal, border = NA)
#contour plot
Herb.contour.Growth <- surface(Herb.Growth, type="C", xlab="Growth", ylab="Herbicide Resistance", zlab="RelFit", add.legend = TRUE, col = pal)
points(xFGac, pch = 2, col = "gray20", cex = .8)

####################### Adaptive Landscape for Growth and Whitefly Resistance in presence of Dicamba
a1 <- data.frame(DY$RelGrowth, DY$WflyRes, DY$RelFitness)
a2<- na.omit(a1)
a3 <- data.frame(a2$DY.RelGrowth, a2$DY.WflyRes)
a1.mat = as.matrix(a3)
colnames(a1.mat) <- c("Relative Growth", "Whitefly Resistance")
Wfly.Growth<- Tps(a1.mat, a2$DY.RelFitness, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
                  miles = TRUE, method = "GCV", GCV = TRUE)

pal <- wes_palette("Zissou1", 100, type = "continuous")

#surface plot
Wfly.surface.Growth <- surface(Wfly.Growth, type="p", xlab="Growth", ylab="Whitefly Resistance", zlab="RelFit", add.legend = FALSE, col= pal, border = NA)
#contour plot
Wfly.contour.Growth <- surface(Wfly.Growth, type="C", xlab="Growth", ylab="Whitefly Resistance", zlab="RelFit", add.legend = TRUE, col = pal)
points(xFGac, pch = 2, col = "gray20", cex = .8)

#################################### Adaptive Landscape for Growth and Whitefly Resistance in absence of Dicamba
b1 <- data.frame(DY0$RelGrowth, DY0$WflyRes, DY0$RelFitness)
b2<- na.omit(b1)
b3 <- data.frame(b2$DY0.RelGrowth, b2$DY0.WflyRes)
b1.mat = as.matrix(b3)
colnames(b1.mat) <- c("Relative Growth", "Whitefly Resistance")
Wfly.Growth2<- Tps(b1.mat, b2$DY0.RelFitness, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
                  miles = TRUE, method = "GCV", GCV = TRUE)

pal <- wes_palette("Zissou1", 100, type = "continuous")

#surface plot
Wfly.surface.Growth2 <- surface(Wfly.Growth2, type="p", xlab="Growth", ylab="Whitefly Resistance", zlab="RelFit", add.legend = FALSE, col= pal, border = NA)
#contour plot
Wfly.contour.Growth2 <- surface(Wfly.Growth2, type="C", xlab="Growth", ylab="Whitefly Resistance", zlab="RelFit", add.legend = TRUE, col = pal)
points(Wfly.contour.Growth2, pch = 2, col = "gray20", cex = .8)



########################## 3) PHENOTYPIC SELECTION ANAYLSIS #######################################
#Standarize traits of interest - Resistance Traits
Dicamba$StdWflyRes<-(Dicamba$WflyResistance-mean(Dicamba$WflyResistance,na.rm=TRUE))/sd(Dicamba$WflyResistance, na.rm=TRUE)
Dicamba$StdHerbRes<-(Dicamba$HerbResistance-mean(Dicamba$HerbResistance,na.rm=TRUE))/sd(Dicamba$HerbResistance, na.rm=TRUE)
Dicamba$StdChewDam<-(Dicamba$ChewDamage-mean(Dicamba$ChewDamage,na.rm=TRUE))/sd(Dicamba$ChewDamage, na.rm=TRUE)

#Plant traits 
Dicamba$StdLeaf<-(Dicamba$Leaf3-mean(Dicamba$Leaf3,na.rm=TRUE))/sd(Dicamba$Leaf3, na.rm=TRUE)
Dicamba$StdHeight<-(Dicamba$Height3-mean(Dicamba$Height3,na.rm=TRUE))/sd(Dicamba$Height3, na.rm=TRUE)
Dicamba$StdWidth<-(Dicamba$Width.Avg-mean(Dicamba$Width.Avg,na.rm=TRUE))/sd(Dicamba$Width.Avg, na.rm=TRUE)
Dicamba$StdFlower<-(Dicamba$Flower-mean(Dicamba$Flower,na.rm=TRUE))/sd(Dicamba$Flower, na.rm=TRUE)
Dicamba$StdFit<-(Dicamba$RelFitness-mean(Dicamba$RelFitness,na.rm=TRUE))/sd(Dicamba$RelFitness, na.rm=TRUE)

####################################### Mixed linear models 
#Resistance and Genetic Variation

#Linear Selection for all dicamba treatments   
sec1 <-lmer(RelFitness ~ StdWflyRes + StdHerbRes + StdGrowth + (1|POP) + (1|POP:ML), 
            data = DY, na.action = na.exclude)
sec1.2 <-lm(RelFitness ~  StdWflyRes + StdHerbRes + StdGrowth, 
            data = DY, na.action = na.exclude)
              
anova(sec1,sec1.2, test= "Chisq")
Anova(sec1, type = "III", test.statistic="F")
summary(sec1)
hist(resid(sec1))
shapiro.test(resid(sec1))

#Selection Absence of Drift
sec2.2 <-lm(RelFitness ~ StdWflyRes + StdGrowth +StdHerbRes + I(StdWflyRes^2) * I(StdGrowth^2) * I(StdHerbRes^2), 
            data = DY, na.action = na.exclude)
summary(sec2.2)
hist(resid(sec2.2))

#Selecyion in Drift
sec1.2 <-lm(RelFitness ~ StdWflyRes + StdGrowth + I(StdWflyRes^2) * I(StdGrowth^2),
            data = DY0, na.action = na.exclude)
sec1.2 <-lmer(RelFitness ~ StdWflyRes + StdGrowth + I(StdWflyRes^2) * I(StdGrowth^2)* Dicamba  + (1|POP) + (1|POP:ML),
              data = Dicamba, na.action = na.exclude)
summary(sec1.2)
hist(resid(sec1.2))

#Quadratic Selection for all treatments - Whitefly Resistance Focus 

#With random effects 
sec3 <-lm(RelFitness ~ StdWflyRes + StdHerbRes + StdGrowth * Dicamba + Dicamba,
          data = Dicamba, na.action = na.exclude)
sec3 <-lm(RelFitness ~ StdWflyRes * Dicamba + StdGrowth + Dicamba,
          data = Dicamba, na.action = na.exclude)

sec3 <-lmer(RelFitness ~ StdWflyRes + StdGrowth + (1|POP) + (1|POP:ML),
          data = DY0, na.action = na.exclude)
sec3 <-lm(RelFitness ~ StdWflyRes + StdHerbRes + StdGrowth,
          data = DY0, na.action = na.exclude)
sec3.2 <-lm(RelFitness ~ StdWflyRes + StdGrowth  + StdHerbRes + 
              I(StdHerbRes^2) * I(StdWflyRes^2) * I(StdGrowth^2)*Dicamba + Dicamba , data = Dicamba, na.action = na.exclude)
anova (sec3,sec3.2, test= "Chisq")
Anova(sec3, type = "III", test.statistic="F")
summary(sec3)
hist(resid(sec3.2))
shapiro.test(resid(sec3.2))


#Without random effects 
sec4 <-lmer(Rel_fit6 ~   StdWflyRes * StdHerbRes * StdGrowth * (1|POP) , data = DY0, na.action = na.exclude)
Anova(sec4, type = "III", test.statistic="F")
summary(sec4)
anova(sec3, sec4)
hist(resid(sec4))
shapiro.test(resid(sec4))

#Linear Selection for dicamba treatments - Whitefly Resistance and Herbicide Resistance Focus 

#With random effects 
sec5 <-lm(Rel_fit6 ~  BLK + StdWflyRes * StdHerbRes * 
            StdGrowth + StdFlower , data = DY0, na.action = na.exclude)
Anova(sec5, type = "III", test.statistic="F") 
summary(sec5)
hist(resid(sec5))
shapiro.test(resid(sec5))

anova(sec8, sec5)
AIC(sec5)
#Without random effects ***
sec6 <-lm(RelFitness ~ BLK + TRT + StdWflyRes * StdHerbRes *
            StdLeaf * StdHeight * StdFlower, data = Dicamba, na.action = na.exclude)
Anova(sec6, type = "III", test.statistic="F")
summary(sec6)
hist(resid(sec6))
shapiro.test(resid(sec6))

#Quadratic Selection for DICAMBA treatments 

#With random effects 
sec7 <-lmer(Rel_Fit1 ~ BLK + TRT * I(StdWflyRes^2) * I(StdHerbRes^2)*
            StdGrowth + StdFlower + (1|POP), data = DY, na.action = na.exclude)
Anova(sec7, type = "III", test.statistic="F")
summary(sec7)
hist(resid(sec7))
shapiro.test(resid(sec7))

#Without random effects 
sec8 <-lm(Rel_Fit1 ~  BLK + TRT * I(StdWflyRes^2) * I(StdHerbRes^2) *
          StdGrowth + StdFlower, data = DY, na.action = na.exclude)
Anova(sec8, type = "III", test.statistic="F")
anova (sec7, sec8)
summary(sec8)
hist(resid(sec8))
shapiro.test(resid(sec8))

