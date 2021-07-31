
################### 1) HERBIVORY RESPONSE TO HERBICIDE DRIFT ON HOST #################################
#HERBIVORY DAMAGE  BY TREATMENTS
ggplot(Y2018, aes(x=TRT, y=log(ChewDamage), fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("log (%Chewing Damage)")+
  xlab("Treatment")

ggplot(Y2018, aes(x=TRT, y=Wfly, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Whitefly Abundance (%)")+
  xlab("Treatment")

ggplot(Y2019, aes(x=TRT, y=Wfly, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Whitefly Abundance (%)")+
  xlab("Treatment")
#Relative Fitness vs Herb Res 

ggplot(DY, aes(x=HerbResistance, y=Rel_Fit)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + ylab("Relative Fitness")+
  xlab("Herbicide Resistance")
################### 2) PHYSIOLOGY RESPONSE TO HERBICIDE DRIFT #########################################
#Height
ggplot(Y2018, aes(x=TRT, y=Height3, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+ 
  theme_bw(base_size = 20) + ylab("Height")+
  xlab("Treatment")
ggplot(Y2019, aes(x=TRT, y=Height3, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Height")+
  xlab("Treatment")
#Leaf Number
ggplot(Y2018, aes(x=TRT, y=Leaf3, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Leaf Number")+
  xlab("Treatment")
ggplot(Y2019, aes(x=TRT, y=Leaf3, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Leaf Number")+
  xlab("Treatment")

#Leaf Width 
ggplot(Y2018, aes(x=TRT, y=Width.Avg, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Leaf Width")+
  xlab("Treatment")
ggplot(Y2019, aes(x=TRT, y=Width.Avg, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Leaf Width")+
  xlab("Treatment")

#Flower Number
ggplot(Y2018, aes(x=TRT, y=Flower, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Flower Number")+
  xlab("Treatment")
ggplot(Y2019, aes(x=TRT, y=Flower, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Flower Number")+
  xlab("Treatment")

#Net Carbon Assimilation
ggplot(Lic0, aes(x=Ci, y=Photo, fill = ID)) +
  stat_smooth(aes(group = 1, colour = "Leaf"), 
                   method = lm, formula = y ~poly(x,2), level = 0.95) +
  theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

#Curves
dat = bind_rows(Lic$Photo, Lic$Ci, .id="Lic") %>% 
  mutate(avg = approx(x,y,xout=x)$y)

ggplot(Lic.5, aes(x=Ci, y=Photo, group = Leaf, color = Leaf)) + 
  stat_smooth(size= 1.5, aes(group = Leaf), method = lm, formula = y ~poly(x,2), level = 0.95, 
  fullrange = TRUE, se = FALSE) + theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

ggplot(Lic1, aes(x=Ci, y=Photo, group = Leaf, color = Leaf)) + 
  stat_smooth(size= 1.5, aes(group = Leaf), method = lm, formula = y ~poly(x,2), level = 0.95, 
              fullrange = TRUE, se = FALSE) + theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

ggplot(Lic0, aes(x=Ci, y=Photo, group = Leaf, color = Leaf)) + 
  stat_smooth(size= 1.5, aes(group = Leaf), method = lm, formula = y ~poly(x,2), level = 0.95, 
              fullrange = TRUE, se = FALSE) + theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

ggplot(Lic1, aes(x=Ci, y=Photo, group = Leaf, color = Leaf)) +
  geom_point(aes(colour=Leaf)) +
  geom_line(aes(y=)) +
  geom_smooth(se=FALSE, colour="red", span=0.3, linetype="11") +
  theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

ggplot(Lic, aes(x=Ci, y=Photo, fill = Trt)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(Leaf))) +
  geom_line(aes(color=factor(ID)))+
  theme_bw(base_size = 20) + ylab("A")+
  xlab("Ci")

################### 3) GENETIC VARTION FOR RESISTANCE AND GROWTH RATE ############################################
#Resistance
ggplot(Dicamba, aes(x=POP, y=WflyResistance, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(POP))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab("Whitefly Resistance")+
  xlab("Population")

ggplot(DY, aes(x=POP, y=HerbResistance, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(POP))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw(base_size = 20) + ylab(" Herbicide Resistance ")+
  xlab("Population")

#Growth Rate
ggplot(DY2019, aes(x=POP, y=RelGrowth, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(POP))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Population")

ggplot(Y2019, aes(x=TRT, y=RelGrowth, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Treatment")

ggplot(Y2018, aes(x=TRT, y=RelGrowth, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Treatment")
################### 3) PHENOTYPIC SELECTION
################### 3a) GROWTH RATE RESPONSE TO HERBICIDE DRIFT ON HOST #################################

#Relative Growth by Population
ggplot(DY2019, aes(x=POP, y=RelGrowth, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(POP))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Population")

ggplot(Y2019, aes(x=TRT, y=RelGrowth, fill = POP)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Treatment")

ggplot(Y2018, aes(x=TRT, y=RelGrowth, fill = TRT)) +
  geom_point(position=position_jitterdodge(),alpha=0.3, aes(color=factor(TRT))) +
  geom_boxplot(alpha=0.3, outlier.size = 0)+
  theme_bw() + ylab("Relative Growth")+
  xlab("Treatment")

#Correlation with Stressors in Dicamba Treatments
ggplot(DY2018, aes(x=WflyResistance, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm") +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("Whitefly Resistance")

ggplot(DY2019, aes(x=WflyResistance, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm", formula = y ~ x + I(x^2) , size = 1) +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("Whitefly Resistance")

ggplot(DY2018, aes(x=HerbResistance, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm") +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("Herbicide Resistance")

ggplot(DY2019, aes(x=HerbResistance, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("Herbicide Resistance")

ggplot(DY, aes(x=RelFitness, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("Relative Fitness")

#Correlations with Plant Phys
ggplot(DY2019, aes(x=C.N, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm") +
  theme_bw() + ylab("Relative Growth Rate")+
  xlab("C:N")

ggplot(Licor, aes(y=meanPhoto, x=RelGrowth)) +
  geom_point(alpha = 0.3, aes(color=factor(POP))) + stat_smooth(method = "lm") +
  theme_bw() + xlab("Relative Growth Rate")+
  ylab("Photosynthetic Assimilation")

ggplot(Licor, aes(y=meanPhoto, x=WflyResistance)) +
  geom_point(alpha = 0.3, aes(color=factor(POP))) + stat_smooth(method = "lm") +
  theme_bw() + xlab("Wfly Resistance")+
  ylab("Photosynthetic Assimilation")
################### 4) PHENOTYPIC SELECTION ###############################################
#Correlation with Stressors in Control Treatments
ggplot(DY, aes(x=WflyResistance, y=RelFitness)) +
  geom_point(alpha = .5, size = 10) + xlim(NA, 0.95) + ylim(NA, 8) +
  theme_classic(base_size = 50) + ylab("Relative Fitness")+
  xlab("Whitefly Resistance")

ggplot(DY0, aes(x=WflyResistance, y=RelFitness)) +
  geom_point(alpha = .5, size = 10) + xlim(NA, 0.9) + ylim(NA, 8) +
  theme_classic(base_size = 50) + ylab("Relative Fitness")+
  xlab("Whitefly Resistance")

ggplot(DY, aes(x=HerbResistance, y=RelFitness)) +
  geom_point(alpha = .5, size = 10) + xlim(NA, 0.9) + ylim(NA, ) +
  stat_smooth(method = lm, formula= y~x, size=1.7, color = "black", se=FALSE, fullrange=TRUE) +
  theme_classic(base_size = 50) + ylab("Relative Fitness")+
  xlab("Herbicide Resistance")

ggplot(DY, aes(x=RelGrowth, y=WflyResistance)) +
  geom_point(alpha = 0.3,  position = position_jitter() , aes(color=factor(POP))) + 
  stat_smooth(method = "lm") +
  theme_bw() + ylab("Wfky Resistance")+
  xlab("Relative Growth")

ggplot(DY, aes(x=RelGrowth, y=HerbResistance)) +
  geom_point(alpha = 0.3,  position = position_jitter() , aes(color=factor(POP))) + 
  stat_smooth(method = "lm") +
  theme_bw() + ylab("Herb Resistance")+
  xlab("Relative Growth")

ggplot(DY, aes(x=WflyResistance, y=HerbResistance)) +
  geom_point(alpha = .5, size = 10) + xlim(NA, 1) + ylim(NA, 1) +
  stat_smooth(method = lm, formula= y~x, size=1.7, color = "black", se=FALSE, fullrange=TRUE) +
  theme_classic(base_size = 50) + ylab("Herbicide Resistance")+
  xlab("Whitefly Resistance")

########################## 3D Scatter Plot ###################################
scatter3D(Dicamba$WflyResistance, Dicamba$HerbResistance, Dicamba$RelFitness, pch=17,
           cex=1.5, theta=30, main="Fitness Response to Resistance", 
           xlab = "Whitefly Resistance", ylab="Herbicide Resistance", zlab="Fitness")

scatter3D(DY$StdWflyRes,DY$StdHerbRes, DY$RelGrowth, colvar = DY$RelFitness,
          col = ramp.col(c("lightblue" , "gold", "red"), alpha = .75), clim = c(0,11), NAcol = "NA", pch=17,
          cex=1.6, theta=30, clab = "Relative Fitness", 
          xlab = c("Whitefly Resistance"),  ylab="Herbicide Resistance", zlab="Relative Growth" )
  
##########################Correlations #####################################################

 z <- DY$RelFitness
ggplot(DY, aes(x=WflyResistance, y=HerbResistance, z = RelFitness)) +
  geom_point(alpha = .3, size = 5, pch=17, aes(colour = z)) + ylim(NA, .9) + xlim(NA, .9) + colvar = DY$RelFitness +
  geom_line(stat = y~x,  size=1) + scale_fill_gradientn(limits = c(0,10), 
  colours=c("navyblue", "darkmagenta", "darkorange1"),breaks=c(0, 5, 10), labels=format(c(0, 5, 10))) +
  theme_classic(base_size = 50) + ylab("Herbicide Resistance")+
  xlab("Whitefly Resistance")

######### Resistance Correlations using Standarzied Values from model 
#Relative Fitness Model 
visreg2d(sec3, x= "StdWflyRes", y = "StdHerbRes", plot.type= "gg", color=c( "skyblue1", "lightgreen", "white", "orange")) + 
  geom_point(aes(StdWflyRes, StdHerbRes),data = DY, alpha = .7, size = 2, pch=19)  +
  theme_classic(base_size = 20) + ylab("Herbicide Resistance") + 
  xlab("Whitefly Resistance")

#Relative Growth Model
visreg2d(sec3.2, x= "StdWflyRes", y = "StdHerbRes", plot.type= "gg", color=c( "skyblue1", "lightgreen", "white", "orange")) + 
  geom_point(aes(StdWflyRes, StdHerbRes),data = DY, alpha = .7, size = 2, pch=19)  +
  theme_classic(base_size = 20) + ylab("Herbicide Resistance") + 
  xlab("Whitefly Resistance")
  
visreg2d(sec3, x= "StdGrowth", y = "StdHerbRes", plot.type= "gg", color=c("skyblue1", "lightgreen", "lightsalmon1")) + 
  geom_point(aes(StdGrowth, StdHerbRes),data = DY, alpha = .7, size = 2, pch=19) +
  theme_classic(base_size = 20) 

visreg2d(sec1.2, x= "StdWflyRes", y = "RelFitness", plot.type= "gg") + stat_smooth(method = "lm", size = 1) +
  geom_point(aes(StdWflyRes, RelFitness),data = DY, alpha = .7, size = 2, pch=19) +
  theme_classic(base_size = 20) ci

######Regular Corrplot no colors cotat
ggplot(DY, aes(x=StdWflyRes, y=StdHerbRes)) +
  geom_point(alpha = 0.3,  size = 2.5, position = position_jitter()) + stat_smooth(method = "lm", size = 1, colour = "black", se=FALSE) +
  theme_classic(base_size = 20) + ylab("Herbicide Resistance")+
  xlab("Whitefly Resistance")

ggplot(DY, aes(x=StdWflyRes, y=RelGrowth)) +
  geom_point(alpha = 0.5,  size = 2.5, position = position_jitter()) + stat_smooth(method = "lm", size = 1, colour = "black", se=FALSE) +
  theme_classic(base_size = 20) + ylab("Relative Growth")+
  xlab("Whitefly Resistance")

ggplot(DY, aes(x=HerbResistance, y=RelGrowth)) +
  geom_point(alpha = 0.3,  size = 2.5, position = position_jitter()) +
  theme_classic(base_size = 20) + ylab("Relative Growth") + xlab("Herbicide Resistance")

ggplot(DY2019, aes(x=CN, y=RelGrowth)) +
  geom_point(alpha = 0.3,  position = position_jitter(), aes(color=factor(POP))) + stat_smooth(method = "lm", size = 1) +
  theme_bw() + ylab("Herbicide Resistance")+
  xlab("Whitefly Resistance")

#Correlation plot
corrplot(data.corr$r, method = "circle", tl.col="black", tl.cex=.9, tl.srt=70)
corrplot(data.corr$r, add=T, type="lower", method="number",
         col="black", diag=F, tl.pos="n", cl.pos="n", tl.cex=0.7)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(data.corr$r, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=50, #Text label color and rotation
         p.mat = data.corr$P , sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE) 

