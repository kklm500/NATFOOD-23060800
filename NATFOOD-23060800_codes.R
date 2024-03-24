library(MuMIn)
library(patchwork)
library(nlme)
library(tidyverse)
library(piecewiseSEM)
library(ggpubr)
library(vegan)
library(car)
library(pracma)
library(MASS)

ad.R<- function(x){
  n= x$dims[[1]]
  p= x$dims[[2]]
  R.squared<- rsquared(x)[,5]
  ad.R= 1 - ((1 - R.squared) * (n - 1) / (n - p - 1))
  return(ad.R)
}

scaleFUN <- function(x) sprintf("%.2f", x) 
scaleFUN1 <- function(x) sprintf("%.1f", x) 


# 
global_cv_1<- global_crop_data%>%
  gather(Order,Value,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10)%>%
  separate(Order,c("Y","Order"), sep= "_")%>%
  select(-Y)%>%
  filter(Element != "Production")%>%
  spread(Element, Value)
global_cv_1$Order<- as.numeric(global_cv_1$Order) 

global_cv_2<- global_cv_1%>%
  arrange(desc(Order))%>%
  group_by(Country,Decade, Order)%>%
  summarise(Area.T = sum(Area)/1e+05,
            P_mcal.T = sum(Production_mCal),
            Richness = n(),
            Diversity = diversity(Area, index = "shannon"))%>%
  mutate(EH = exp(Diversity))
global_cv_3<- global_cv_2%>%
  mutate(Yield = P_mcal.T/Area.T)%>%
  ungroup()%>%
  group_by(Country,Decade) %>%
  summarise(mean.yield = mean(Yield),
            sd.yield = sd(detrend(Yield)),
            Area.h = mean(Area.T),
            Richness = mean(Richness),
            EH = mean(EH),
            D = mean(Diversity))%>%
  mutate(CV = sd.yield/mean.yield,
         Area.avg = Area.h/Richness)

global_cv_4<- global_cv_1%>%
  left_join(global_cv_2[,c(1:4)], by = c("Country", "Decade","Order"))%>%
  mutate(yield = Production_mCal/Area.T)%>%
  group_by(Country,Decade, Item)%>%
  arrange(Order)%>%
  summarise(sd.yield.crop = sd(detrend(yield)),
            mean.yield.crop = mean(yield))%>%
  ungroup()%>%
  left_join(global_cv_3[,c(1:3)], by = c("Country", "Decade"))%>%
  mutate(cv.crop = sd.yield.crop/mean.yield.crop,
         weight = mean.yield.crop/mean.yield)%>%
  mutate(cv.w = cv.crop*weight,
         cv.w.a = weight * (1/cv.crop))%>%
  group_by(Country,Decade)%>%
  summarise(sd.sum = sum(sd.yield.crop),
            cv.avg = sum(cv.w))%>%
  left_join(global_cv_3, by = c("Country", "Decade"))%>%
  mutate(phi= sd.yield/sd.sum)%>%
  mutate(cv = cv.avg*phi)

Nation_CV_full <- global_cv_4%>%
  mutate(Yield.S = 1/cv,
         Avg.crop.S = 1/cv.avg,
         asy = 1/phi)%>%
  select(Country,Decade,mean.yield,Area.h,Area.avg,Richness,EH,D,Yield.S,Avg.crop.S,asy)%>%
  na.omit() 

Nation_CV_full[Nation_CV_full$Country == "Ethiopia PDR",]$Country <- "Ethiopia"
Nation_CV_full[Nation_CV_full$Country == "Belgium-Luxembourg",]$Country <- "Belgium"
Nation_CV_full[Nation_CV_full$Country == unique(Nation_CV_full$Country)[47],]$Country <- "Cote d'Ivoire"
Nation_CV_full<- Nation_CV_full%>%
  group_by(Country)%>%
  mutate(time = 1:n())
Nation_CV<- Nation_CV_full%>%
  left_join(country_name[,c(2,4,6)], by = c("Country"= "Areaname_FAO"))%>%
  left_join(Land_area, by =c("Country" ,  "Decade"))%>%
  left_join(irrigation, by =c("Country" ,"Decade"))%>%
  left_join(FertilizerUse, by =c("Country" ,"Decade"))%>%
  left_join(precipitation, by =c("Country" = "Areaname_FAO", "Decade"= "Decade"))%>%
  left_join(Temperature, by =c("Country" = "Areaname_FAO", "Decade"= "Decade"))%>%
  left_join(soildiversity[,c(4,2)], by =c("Country" = "Areaname_FAO"))%>%
  left_join(WorldSOC[,c(4,2)], by =c("Country" = "Areaname_FAO"))%>%
  left_join(warfare_region, by =c("Region","Decade"))%>%
  left_join(Framavg_total, by =c("Code"="ISO3_CODE"))%>%
  na.omit()%>%
  left_join(AHDI[,c(5,2,3)], by =c("Country" = "Areaname_FAO", "Decade"= "Decade"))

quantile(log10(Nation_CV$Landarea))

Nation_CV_log<- Nation_CV%>%
  group_by(Country,Decade)%>%
  summarise(Richness = Richness,
            EH =EH,
            lgYield = log10(mean.yield),
            lgAreaH = log10(Area.h),
            lgArea.avg = log10(Area.avg),
            lgYieldS = log10(Yield.S),
            lgAvgS = log10(Avg.crop.S),
            lgAsy= log10(asy),
            lgIrrigation = log10(Irrigation),
            lgNuse = log10(N.use),
            lgCropland = log10(Cropland),
            lgLandarea = log10(Landarea),
            lgPreCV = log10(pre.cv),
            lgTmpCV = log10(tmp.cv),
            lgfarmN = log10(farm.number),
            lgLpreN = log10(LPre.n),
            lgSOC = log10(SOC),
            lgwar= log10(Warfare))%>%
  mutate(Size.four = cut (lgLandarea,c(-2.5,-1.0385504,-0.5255361,-0.1130132,1.25) , c("Small","Mid-small","Middle","Large")))


# figure 1
figure1a_model<- gls(log10(Yield.S)~log10(Area.h), correlation=corAR1(form=~1|Country), Nation_CV,method = "ML")

summary(figure1a_model)
anova(figure1a_model)
rsquared(figure1a_model)

intervals(figure1a_model)
pred_figure1a_model_dt <- predict(figure1a_model, Nation_CV, level = 0.05,se.fit=T)  
figure1a<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(Yield.S)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_classic()+
  ylim(0.3,3.4)+
  labs(x = "log10(Total harvested area)", y = "log10(National yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_figure1a_model_dt$fit-1.96*pred_figure1a_model_dt$se.fit ,ymax=pred_figure1a_model_dt$fit+1.96*pred_figure1a_model_dt$se.fit), fill = "grey",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_figure1a_model_dt$fit),  color = "black",span=1, size=2,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",size=1.0),
        axis.line.y=element_line(linetype=1,color="black",size=1.0),
        legend.position = "none")

figure1b_SEM<- psem(
  #lm(EH~ log10(Area.h), Nation_CV),
  gls(log10(pre.cv) ~ log10(Area.h),correlation=corAR1(form=~1|Country),  Nation_CV),
  gls(log10(LPre.n) ~ log10(Area.h),correlation=corAR1(form=~1|Country),  Nation_CV),
  gls(log10(asy) ~ EH+log10(tmp.cv),correlation=corAR1(form=~1|Country), Nation_CV),
  gls(log10(Avg.crop.S) ~ log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(LPre.n)+log10(Irrigation), correlation=corAR1(form=~1|Country),Nation_CV),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy), Nation_CV),
  log10(Avg.crop.S) %~~% log10(asy),log10(tmp.cv) %~~% log10(pre.cv),log10(Irrigation)%~~% log10(pre.cv),log10(LPre.n)%~~% log10(pre.cv),EH %~~% log10(pre.cv), data = as.data.frame(Nation_CV)
)
summary(figure1b_SEM)

totalsize.df<- data.frame(Item= c("Diversity","Area","Irrigation"), Size = c(0.2040,0.1869,0.0948))

figure1c<-ggplot(totalsize.df, aes(x= Item, y = Size))+geom_bar(fill = "#BABABA",color = "black", stat = "identity", position = "dodge", width = 0.6)+
  scale_x_discrete(limits = c("Diversity","Area","Irrigation"))+
  ylab("Total effects")+
  ylim(0,0.28)+
  theme_classic()+theme(axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                        axis.title.x=element_blank(),
                        axis.text.x=element_text(size=24),
                        axis.text.y=element_text(size=22),
                        legend.key.size = unit(1.2, 'cm'),
                        axis.line.x=element_line(linetype=1,color="black",size=1),
                        axis.line.y=element_line(linetype=1,color="black",size=1),
                        axis.ticks.length = unit(0.2, 'cm'),
                        legend.position="none")

#spatial dependency of the relationship between crop diversity and national yield stability (ED Table 3)
Yieldstability_spatialdependency_landarea<- gls(log10(Yield.S)~EH+EH:log10(Landarea), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
Yieldstability_spatialdependency_harvestedarea<- gls(log10(Yield.S)~EH+EH:log10(Area.h), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
Yieldstability_spatialdependency_geocroplandarea<- gls(log10(Yield.S)~EH+EH:log10(Cropland), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
plot(ACF(Yieldstability_spatialdependency_landarea, resType = "normalized"))
anova(Yieldstability_spatialdependency_landarea)
summary(Yieldstability_spatialdependency_landarea)

# figure 2a model,  temporal autocorrelation testing,  plotting within country groups

figure2a_model_group<- gls(lgAsy~EH* as.factor(Size.four), correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML")
pre_figure2a_model_group<- predict(figure2a_model_group,Nation_CV_log, group = as.factor(Size.four), level = 0.05,se.fit=T)
figure2a_1<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgAsy),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.00,1.0))+
  geom_ribbon(aes(x=EH, ymin=pre_figure2a_model_group$fit-1.96*pre_figure2a_model_group$se.fit ,ymax=pre_figure2a_model_group$fit+1.96*pre_figure2a_model_group$se.fit, fill = as.factor(Size.four)),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = EH, y=pre_figure2a_model_group$fit, color =as.factor(Size.four),linetype=as.factor(Size.four)),alpha=0.2,span=1, size=2,method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(1,1,1,1))+
  xlab("")+
  ylab("log10(Crop asynchrony)")+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))
figure2a_2<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgAsy',
                          shape = 21, color = "NA", alpha = 0.0,
                          palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                          ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                         axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                         axis.text.x=element_text(vjust=0,size=22),
                                                         axis.text.y=element_text(hjust=0,size=22),
                                                         legend.key.size = unit(1.2, 'cm'),
                                                         axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                         axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                          margin.plot = "histogram",
                          margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                          margin.plot.size = 0.6,
                          xlab = "Crop diversity",
                          ylab = "log10(Crop asynchrony)",
                          legend = c("NA"),
                          ggp = figure2a_1)

#spatial dependency of the relationship between crop diversity and species asynchrony
Asynchrony_spatialdependency_landarea<- gls(log10(asy)~EH+EH:log10(Landarea), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
Asynchrony_spatialdependency_harvestedarea<- gls(log10(asy)~EH+EH:log10(Area.h), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
Asynchrony_spatialdependency_geocroplandarea<- gls(log10(asy)~EH+EH:log10(Cropland), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
plot(ACF(Asynchrony_spatialdependency_landarea, resType = "normalized"))
anova(Asynchrony_spatialdependency_landarea)
summary(Asynchrony_spatialdependency_landarea)
# figure 2b model,  temporal autocorrelation testing,  plotting within country groups
figure2b_model_group<- gls(lgAsy~EH*as.factor(Size.four), correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML")
plot(ACF(figure2b_model_group, resType = "normalized"))
pred_figure2b_model_group <- predict(figure2b_model_group,Nation_CV_log, level = 0.05,se.fit=T)  

Plot2.2a<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgAsy),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  geom_ribbon(aes(x=EH, ymin=pred_figure2b_model_group$fit-1.96*pred_figure2b_model_group$se.fit,ymax=pred_figure2b_model_group$fit+1.96*pred_figure2b_model_group$se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x=EH, y=pred_figure2b_model_group$fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(1,1,1,1))+
  xlab("")+
  ylab("log10(Crop asynchrony)")+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))
Plot2.2b<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgAsy',
                        shape = 21, color = "NA", alpha = 0.0,
                        palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                        ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                       axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                       axis.text.x=element_text(vjust=0,size=22),
                                                       axis.text.y=element_text(hjust=0,size=22),
                                                       legend.key.size = unit(1.2, 'cm'),
                                                       axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                       axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                        margin.plot = "histogram",
                        margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                        margin.plot.size = 0.6,
                        xlab = "Crop diversity",
                        ylab = "log10(Crop asynchrony)",
                        legend = c("NA"),
                        ggp = Plot2.2a)

#spatial dependency of the relationship between crop diversity and average crop stability (ED Table 3)
AvgcropS_spatialdependency_landarea<- gls(log10(Avg.crop.S)~EH+EH:log10(Landarea), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
AvgcropS_spatialdependency_harvestedarea<- gls(log10(Avg.crop.S)~EH+EH:log10(Area.h), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
AvgcropS_spatialdependency_geocroplandarea<- gls(log10(Avg.crop.S)~EH+EH:log10(Cropland), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML")
plot(ACF(AvgcropS_spatialdependency_landarea, resType = "normalized"))
summary(AvgcropS_spatialdependency_landarea)
# figure 2b model,  temporal autocorrelation testing,  plotting within country groups
figure2c_model_group<- gls(lgAvgS~EH*as.factor(Size.four), correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML")
plot(ACF(figure2b_model_group, resType = "normalized"))
pred_figure2c_model_group <- predict(figure2c_model_group,Nation_CV_log, level = 0.05,se.fit=T)  

figure2c_1<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgAvgS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  geom_ribbon(aes(x=EH, ymin=pred_figure2c_model_group$fit-1.96*pred_figure2c_model_group$se.fit,ymax=pred_figure2c_model_group$fit+1.96*pred_figure2c_model_group$se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x=EH, y=pred_figure2c_model_group$fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(2,2,2,1))+
  scale_color_manual(values = colors )+
  xlab("Crop diversity")+
  ylab("log10(Average crop stability)")+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))
figure2c_2<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgAvgS',
                        shape = 21, color = "NA", alpha = 0.0,
                        palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                        ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                       axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                       axis.text.x=element_text(vjust=0,size=22),
                                                       axis.text.y=element_text(hjust=0,size=22),
                                                       legend.key.size = unit(1.2, 'cm'),
                                                       axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                       axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                        margin.plot = "histogram",
                        margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                        margin.plot.size = 0.6,
                        xlab = "Crop diversity",
                        ylab = "log10(Crop asynchrony)",
                        legend = c("NA"),
                        ggp = figure2c_1)

#figure 2c-f
betacoef.yieldS.m <- summary(gls(lgYieldS~EH:Size.four+Size.four-1, correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML"))
betacoef.yieldS <- cbind(group = c("Small", "Mid-small", "Middle" ,"Super-large & Large"), data.frame(betacoef.yieldS.m$tTable[ 5:8, 1:2]))
betacoef.asy.m <-summary(gls(lgAsy~EH:Size.four+Size.four-1, correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML"))
betacoef.asy <- cbind(group = c("Small", "Mid-small", "Middle" ,"Super-large & Large"), data.frame(betacoef.asy.m$tTable[ 5:8, 1:2]))
betacoef.Avg.crop.S.m <-summary(gls(lgAvgS~EH:Size.four+Size.four-1, correlation=corAR1(form=~1|Country), Nation_CV_log, method = "ML"))
betacoef.Avg.crop.S <- cbind(group = c("Small", "Mid-small", "Middle" ,"Super-large & Large"), data.frame(betacoef.Avg.crop.S.m$tTable[ 5:8, 1:2]))

figure_2d<- ggplot(betacoef.yieldS,aes(x = group, y = Value, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Value+Std.Error,ymin=Value-Std.Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-large & Large", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(-0.005,0.065)+
  ylab("coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(angle =30, hjust=0.7, vjust=0.8, size=21),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")
figure_2e<- ggplot(betacoef.asy,aes(x = group, y = Value, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Value+Std.Error,ymin=Value-Std.Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-large & Large", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(0.00,0.04)+
  ylab("coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(angle =30, hjust=0.7, vjust=0.8, size=21),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")
figure_2f<- ggplot(betacoef.Avg.crop.S,aes(x = group, y = Value, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Value+Std.Error,ymin=Value-Std.Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-large & Large", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(-0.03,0.04)+
  ylab("coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(angle =30, hjust=0.7, vjust=0.8, size=21),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")

#figure 3
modelwithincountry<- lme(scale(log10(Yield.S)) ~ scale(EH),
                 random = list(Country= pdIdent(~ 1), Country = pdIdent(~ scale(EH))),  
                 correlation=corAR1(form=~time),
                 data = Nation_CV)
modelwithincountry<-lmer(scale(log10(Yield.S)) ~ scale(EH)+(scale(EH)||Country), data = Nation_CV)
summary(modelwithincountry)
ranef(modelwithincountry)
plot(ACF(modelwithincountry), resType = "normalized" )
stabilizingeffect<-data.frame(Country = row.names(ranef(modelwithincountry)[2]$Country), 
                              Intercept.R = (ranef(modelwithincountry)[1]$Country+ranef(modelwithincountry)[2]$Country[,1])[,1], 
                              EH_slope.R = ranef(modelwithincountry)[2]$Country[,2])%>%
  rowwise()%>%
  mutate(Intercpt = Intercept.R + summary(modelwithincountry)$coefficients$fixed[1],
         EH_slope = EH_slope.R + summary(modelwithincountry)$coefficients$fixed[2])

stabilizingeffect<- stabilizingeffect%>%
  left_join(country_name[,c(6,5)], by = c("Country"= "Areaname_FAO"))%>%
  left_join(aggregate(Landarea~Country, data = Nation_CV, mean), by = "Country")%>%
  left_join(precipitationCV_sixdecades[,c(1,3)], by = c("Country"= "Areaname_FAO"))%>%
  left_join(temperatureCV_sixdecades[,c(1,2)], by = c("Country"= "Areaname_FAO"))%>%
  left_join(aggregate(Irrigation~Country, data = Nation_CV, mean), by = "Country")%>%
  left_join(aggregate(SOC~Country, data = Nation_CV, mean), by = "Country")%>%
  left_join(aggregate(Warfare~Country, data = Nation_CV, mean), by = "Country")
S.E._model<-lm(scale(EH_slope)~scale(log10(Landarea))+scale(log10(Irrigation))+scale(log10(tmp.cv))+scale(log10(pre.cv))+scale(log10(SOC))+scale(log10(Warfare)), stabilizingeffect)
vif(S.E._model)
summary(S.E._model)
S.E._model_df<- data.frame(summary(S.E._model)$coefficients)[-1,]%>%
  mutate(Item = c("log10(Land area)","log10(Irrigation)","log10(Annual temp. CV)","log10(Annual prcp. CV)","log10(SOC)", "log10(Warfare)"))
figure_3a<-ggplot(S.E._model_df)+geom_bar(aes(x= Item, y = Estimate, fill = Item),color = "black", stat = "identity", position = "dodge", width = 0.8, alpha = 0.75)+
  geom_errorbar(aes(x = Item, ymax=Estimate+Std..Error,ymin=Estimate-Std..Error),position=position_dodge(0.9),width=0.15)+
  scale_fill_manual(values =  c("#BABABA","#BABABA","#BABABA","#9E0000","#BABABA","#BABABA"))+
  scale_x_discrete(limits = c("log10(Land area)","log10(Annual temp. CV)","log10(Annual prcp. CV)","log10(Irrigation)","log10(SOC)", "log10(Warfare)"))+
  ylab("Standardized regression coefficient")+
  ylim(-0.4,0.99)+
  theme_classic()+theme(axis.title.y=element_text(hjust=1.1, vjust=2,size=26),
                        axis.title.x=element_blank(),
                        axis.text.x=element_text(angle = 45,vjust = 0.92,hjust = 0.92,size=20),
                        axis.text.y=element_text(size=24),
                        legend.key.size = unit(1.2, 'cm'),
                        axis.line.x=element_line(linetype=1,color="black",size=1),
                        axis.line.y=element_line(linetype=1,color="black",size=1),
                        axis.ticks.length = unit(0.2, 'cm'),
                        legend.position="none")

#world.polygon<- readOGR( "E:/Croparea-stability/AgriculturalStability-master/spatial/countries_global.shp")
world.polygon@data<- left_join(world.polygon@data, stabilizingeffect[,c(6,5)], by= c("Area"="Areaname_worldmap"))
world.polygon_df<- fortify(world.polygon)
world.ID<- world.polygon@data
world.ID$id<- 0:(nrow(world.ID)-1)
world.ID$id<- as.character(world.ID$id)
world.polygon_df<- left_join(world.polygon_df,world.ID[,c(2,3)], by = "id")
figure_3b<- ggplot(world.polygon_df)+
  geom_polygon(aes(x = long, y = lat, group = group, fill = EH_slope), colour="#616161",size = 0.35) +
  coord_map(projection = "mollweide")+
  scale_fill_gradientn(values = c (0,0.34,0.5,0.65,0.75,1.0),colors =rev(c("#A31E21","#D96241","#E1872799","#7C878EFF","#357EBDFF")), na.value = "white")+
  theme_light() +theme(
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(1.2,"cm"))


#ED figure 1

EDfigure1a_model <- gls(Richness~ log10(Area.h),  correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
intervals(EDfigure1a_model)
summary(EDfigure1a_model)
anova(EDfigure1a_model)
rsquared(EDfigure1a_model)
pred_EDfigure1a_model_dt <- predict(EDfigure1a_model, Nation_CV, level = 0.05,se.fit=T)  
EDfigure1a<-ggplot(Nation_CV)+ geom_point(aes(x= log10(Area.h), y=Richness),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,125)+
  labs(x = "log10(Total harvested area)", y = "Richness")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure1a_model_dt$fit-1.96*pred_EDfigure1a_model_dt$se.fit ,ymax=pred_EDfigure1a_model_dt$fit+1.96*pred_EDfigure1a_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure1a_model_dt$fit),  color = "black",span=1, size=2,linetype=1 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

EDfigure1b_model <- gls(EH~ log10(Area.h),  correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
intervals(EDfigure1b_model )
summary(EDfigure1b_model )
anova(EDfigure1b_model )
rsquared(EDfigure1b_model )
pred_Plot2.2model_dt <- predict(EDfigure1b_model , Nation_CV, level = 0.05,se.fit=T) 
EDfigure1b<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=EH),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,35)+
  labs(x = "log10(Total harvested area)", y = "Crop diversity")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_Plot2.2model_dt$fit-1.96*pred_Plot2.2model_dt$se.fit ,ymax=pred_Plot2.2model_dt$fit+1.96*pred_Plot2.2model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_Plot2.2model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure1c_model <- gls(log10(asy)~ log10(Area.h), correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
summary(EDfigure1c_model)
anova(EDfigure1c_model)
intervals(EDfigure1c_model)
rsquared(EDfigure1c_model)
pred_EDfigure1c_model_dt <- predict(EDfigure1c_model, Nation_CV, level = 0.05,se.fit=T)  
EDfigure1c<-ggplot(Nation_CV)+ geom_point(aes(x= log10(Area.h), y=log10(asy)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,1.4)+
  labs(x = "log10(Total harvested area)", y = "log10(Crop asynchrony)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure1c_model_dt$fit-1.96*pred_EDfigure1c_model_dt$se.fit ,ymax=pred_EDfigure1c_model_dt$fit+1.96*pred_EDfigure1c_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure1c_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

EDfigure1d_model <- gls(log10(pre.cv)~ log10(Area.h),  correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
summary(EDfigure1d_model)
anova(EDfigure1d_model)
rsquared(EDfigure1d_model)
intervals(EDfigure1d_model)
pred_EDfigure1d_model_dt <- predict(EDfigure1d_model, Nation_CV, level = 0.05,se.fit=T) 
EDfigure1d<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y= log10(pre.cv)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(-4.2,1.3)+
  labs(x = "log10(Total harvested area)", y = "log10(Annual precipitation CV)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure1d_model_dt$fit-1.96*pred_EDfigure1d_model_dt$se.fit ,ymax=pred_EDfigure1d_model_dt$fit+1.96*pred_EDfigure1d_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure1d_model_dt$fit),  color = "black",span=1, size=2,linetype=1 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure1e_model <- gls(log10(tmp.cv)~log10(Area.h) , correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
summary(EDfigure1e_model)
anova(EDfigure1e_model)
rsquared(EDfigure1e_model)
intervals(EDfigure1e_model)
pred_EDfigure1e_model_dt <- predict(EDfigure1e_model, Nation_CV, level = 0.05,se.fit=T)  
EDfigure1e<-ggplot(Nation_CV)+ geom_point(aes(x= log10(Area.h), y=log10(tmp.cv)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(-3,0.8)+
  labs(x = "log10(Total harvested area)", y = "log10(Annual tempature CV)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure1e_model_dt$fit-1.96*pred_EDfigure1e_model_dt$se.fit ,ymax=pred_EDfigure1e_model_dt$fit+1.96*pred_EDfigure1e_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure1e_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

EDfigure1f_model <- gls(log10(Avg.crop.S)~ log10(Area.h),  correlation=corAR1(form=~1|Country), Nation_CV, method = "ML") 
summary(EDfigure1f_model)
intervals(EDfigure1f_model)
anova(EDfigure1f_model)
rsquared(EDfigure1f_model)
pred_EDfigure1f_model_dt <- predict(EDfigure1f_model, Nation_CV, level = 0.05,se.fit=T) 
EDfigure1f<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(Avg.crop.S)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,2.8)+
  labs(x = "log10(Total harvested area)", y = "log10(Average crop stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure1f_model_dt$fit-1.96*pred_EDfigure1f_model_dt$se.fit ,ymax=pred_EDfigure1f_model_dt$fit+1.96*pred_EDfigure1f_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure1f_model_dt$fit),  color = "black",span=1, size=2,linetype=1 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 
EDfigure1a+EDfigure1b+EDfigure1c+EDfigure1d+EDfigure1e+EDfigure1f


# the relationship between harvested area and yield stability of major crops (ED figure 2)
Major_crops<- global_crop_data%>%
  filter(Item %in% c("Barley","Maize","Rice, paddy","Soybeans","Wheat","Sorghum"))%>%
  gather(Order,Value,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10)%>%
  separate(Order,c("Y","Order"), sep= "_")%>%
  select(-Y)%>%
  spread(Element, Value)%>%
  select(-Production)%>%
  mutate(Order = as.numeric(Order),
         Yield = Production_mCal/Area)%>%
  arrange(Order)%>%
  group_by(Country,Item,Decade)%>%
  summarise(Area.h = mean(Area)/1e+05,
            mean.yield = mean(Yield),
            sd.yield = sd(detrend(Yield)))%>%
  filter(sd.yield != 0)%>%
  mutate(YieldS = mean.yield/sd.yield)
Major_crops$nation_year<- paste(Major_crops$Country, Major_crops$Decade, sep="_")
nation_year2<- paste(Nation_CV$Country, Nation_CV$Decade, sep = "_")
Major_crops_filter<-Major_crops%>% filter(nation_year %in% nation_year2)

EDfigure2a_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Wheat",], method = "ML") 
summary(EDfigure2a_model )
intervals(EDfigure2a_model )
anova(EDfigure2a_model )
rsquared(EDfigure2a_model )
pred_EDfigure2a_model_dt <- predict(EDfigure2a_model, Major_crops_filter[Major_crops_filter$Item == "Wheat",], level = 0.05,se.fit=T)  
EDfigure2a<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Wheat",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure2a_model_dt$fit-1.96*pred_EDfigure2a_model_dt$se.fit ,ymax=pred_EDfigure2a_model_dt$fit+1.96*pred_EDfigure2a_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure2a_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")

EDfigure2b_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Rice, paddy",], method = "ML") 
summary(EDfigure2b_model)
intervals(EDfigure2b_model)
anova(EDfigure2b_model)
rsquared(EDfigure2b_model)
pred_EDfigure2b_model_dt <- predict(EDfigure2b_model, Major_crops_filter[Major_crops_filter$Item == "Rice, paddy",], level = 0.05,se.fit=T) 
EDfigure2b<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Rice, paddy",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure2b_model_dt$fit-1.96*pred_EDfigure2b_model_dt$se.fit ,ymax=pred_EDfigure2b_model_dt$fit+1.96*pred_EDfigure2b_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure2b_model_dt$fit),  color = "black",span=1, size=2,linetype=1 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure2c_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Maize",], method = "ML") 
anova(EDfigure2c_model)
intervals(EDfigure2c_model)
summary(EDfigure2c_model)
rsquared(EDfigure2c_model)
pred_EDfigure2c_model_dt <- predict(EDfigure2c_model, Major_crops_filter[Major_crops_filter$Item == "Maize",], level = 0.05,se.fit=T) 
EDfigure2c<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Maize",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure2c_model_dt$fit-1.96*pred_EDfigure2c_model_dt$se.fit ,ymax=pred_EDfigure2c_model_dt$fit+1.96*pred_EDfigure2c_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure2c_model_dt$fit),  color = "black",span=1, size=2,linetype=1 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure2d_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Soybeans",], method = "ML") 
summary(EDfigure2d_model)
intervals(EDfigure2d_model)
anova(EDfigure2d_model)
rsquared(EDfigure2d_model)
pred_EDfigure2d_model_dt <- predict(EDfigure2d_model, Major_crops_filter[Major_crops_filter$Item == "Soybeans",], level = 0.05,se.fit=T) 
EDfigure2d<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Soybeans",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure2d_model_dt$fit-1.96*pred_EDfigure2d_model_dt$se.fit ,ymax=pred_EDfigure2d_model_dt$fit+1.96*pred_EDfigure2d_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure2d_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure2e_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Barley",], method = "ML") 
anova(EDfigure2e_model)
intervals(EDfigure2e_model)
summary(EDfigure2e_model)
rsquared(EDfigure2e_model)
pred_EDfigure2e_model_dt <- predict(EDfigure2e_model, Major_crops_filter[Major_crops_filter$Item == "Barley",], level = 0.05,se.fit=T) 
EDfigure2e<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Barley",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=pred_EDfigure2e_model_dt$fit-1.96*pred_EDfigure2e_model_dt$se.fit ,ymax=pred_EDfigure2e_model_dt$fit+1.96*pred_EDfigure2e_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=pred_EDfigure2e_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

EDfigure2f_model <- gls(log10(YieldS)~log10(Area.h),  correlation=corAR1(form=~1|Country), Major_crops_filter[Major_crops_filter$Item == "Sorghum",], method = "ML") 
anova(EDfigure2f_model)
intervals(EDfigure2f_model)
summary(EDfigure2f_model)
rsquared(EDfigure2f_model)
EDfigure2f_model_dt <- predict(EDfigure2f_model, Major_crops_filter[Major_crops_filter$Item == "Sorghum",], level = 0.05,se.fit=T) 
EDfigure2f<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Sorghum",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_ribbon(aes(x=log10(Area.h), ymin=EDfigure2f_model_dt$fit-1.96*EDfigure2f_model_dt$se.fit ,ymax=EDfigure2f_model_dt$fit+1.96*EDfigure2f_model_dt$se.fit), fill = "black",alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = log10(Area.h), y=EDfigure2f_model_dt$fit),  color = "black",span=1, size=2,linetype=2 ,method = 'lm',formula=y~x)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

EDfigure2a+EDfigure2b+EDfigure2c+EDfigure2d+EDfigure2e+EDfigure2f

#ED figure 3

EDfigure3a_model<- gls(EH~lgfarmN*Size.four, correlation=corAR1(form=~1|Country), na.omit(Nation_CV_log), method = "ML")
pred_EDfigure3a_model_dt <- predict(EDfigure3a_model, na.omit(Nation_CV_log), level = 0.05,se.fit=T)  
EDfigure3a_1<- ggplot(na.omit(Nation_CV_log))+geom_point(aes(x= lgfarmN, y=EH),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN1,limits=c(0, 27))+
  geom_ribbon(aes(x=lgfarmN, ymin=pred_EDfigure3a_model_dt $fit-1.96*pred_EDfigure3a_model_dt $se.fit ,ymax=pred_EDfigure3a_model_dt $fit+1.96*pred_EDfigure3a_model_dt $se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = lgfarmN, y=pred_EDfigure3a_model_dt $fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,
              method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(2,1,2,1))+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))
EDfigure3a_2<-ggscatterhist(na.omit(Nation_CV_log), x= 'lgfarmN', y= 'EH',
                          shape = 21, color = "NA", alpha = 0.0,
                          palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                          ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                         axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                         axis.text.x=element_text(vjust=0,size=22),
                                                         axis.text.y=element_text(hjust=0,size=22),
                                                         legend.key.size = unit(1.2, 'cm'),
                                                         axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                         axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                          margin.plot = "histogram",
                          margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                          margin.plot.size = 0.6,
                          xlab = "log10(Number of farms)",
                          ylab = "Effective diversity",
                          legend = c("NA"),
                          ggp = EDfigure3a_1)

EDfigure3b_model<- gls(lgArea.avg~EH*Size.four, correlation=corAR1(form=~1|Country), na.omit(Nation_CV_log), method = "ML")
pre_EDfigure3b_model_dt <- predict(EDfigure3b_model,na.omit(Nation_CV_log), level = 0.05,se.fit=T)  
EDfigure3b_1<- ggplot(na.omit(Nation_CV_log))+geom_point(aes(x= EH, y=lgArea.avg),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(-2.5,2))+
  geom_ribbon(aes(x=EH, ymin=pre_EDfigure3b_model_dt$fit-1.96*pre_EDfigure3b_model_dt$se.fit ,ymax=pre_EDfigure3b_model_dt$fit+1.96*pre_EDfigure3b_model_dt$se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = EH, y=pre_EDfigure3b_model_dt$fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,
              method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(1,1,1,1))+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))

EDfigure3b_2<-ggscatterhist(na.omit(Nation_CV_log), x= 'EH', y= 'lgArea.avg',
                          shape = 21, color = "NA", alpha = 0.0,
                          palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                          ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                         axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                         axis.text.x=element_text(vjust=0,size=22),
                                                         axis.text.y=element_text(hjust=0,size=22),
                                                         legend.key.size = unit(1.2, 'cm'),
                                                         axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                         axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                          margin.plot = "histogram",
                          margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                          margin.plot.size = 0.6,
                          xlab = "crop diversity",
                          ylab = "log10(Average area of crops)",
                          legend = c("NA"),
                          ggp = EDfigure3b_1)


EDfigure3c_model<- gls(lgAvgS~lgfarmN*Size.four,  correlation=corAR1(form=~1|Country), na.omit(Nation_CV_log), method = "ML")
pred_EDfigure3c_model_dt <- predict(EDfigure3c_model,na.omit(Nation_CV_log), level = 0.05,se.fit=T)  
EDfigure3c_1<- ggplot(na.omit(Nation_CV_log))+geom_point(aes(x= lgfarmN, y=lgAvgS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.2,2.1))+
  geom_ribbon(aes(x=lgfarmN, ymin=pred_EDfigure3c_model_dt$fit-1.96*pred_EDfigure3c_model_dt$se.fit ,ymax=pred_EDfigure3c_model_dt$fit+1.96*pred_EDfigure3c_model_dt$se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = lgfarmN, y=pred_EDfigure3c_model_dt$fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,
              method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(2,2,1,1))+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26))
EDfigure3c_2<-ggscatterhist(na.omit(Nation_CV_log), x= 'lgfarmN', y= 'lgAvgS',
                          shape = 21, color = "NA", alpha = 0.0,
                          palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                          ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                         axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                         axis.text.x=element_text(vjust=0,size=22),
                                                         axis.text.y=element_text(hjust=0,size=22),
                                                         legend.key.size = unit(1.2, 'cm'),
                                                         axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                         axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                          margin.plot = "histogram",
                          margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                          margin.plot.size = 0.6,
                          xlab = "log10(Number of farms)",
                          ylab = "log10(Average crop stability)",
                          legend = c("NA"),
                          ggp = EDfigure3c_1)

EDfigure3d_model<- gls(lgAvgS~lgArea.avg*Size.four,  correlation=corAR1(form=~1|Country), na.omit(Nation_CV_log))
pred_EDfigure3d_model_dt <- predict(EDfigure3d_model,na.omit(Nation_CV_log), level = 0.05,se.fit=T)  
EDfigure3d_1<- ggplot(na.omit(Nation_CV_log))+geom_point(aes(x= lgArea.avg, y=lgAvgS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.4)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.2,2.1))+
  geom_ribbon(aes(x=lgArea.avg, ymin=pred_EDfigure3d_model_dt$fit-1.96*pred_EDfigure3d_model_dt$se.fit ,ymax=pred_EDfigure3d_model_dt$fit+1.96*pred_EDfigure3d_model_dt$se.fit, fill = Size.four),alpha = 0.1,linetype=0)+
  geom_smooth(aes(x = lgArea.avg, y=pred_EDfigure3d_model_dt$fit, color =Size.four,linetype=Size.four),alpha=0.2,span=1, size=2,
              method = 'lm',formula=y~x)+
  scale_linetype_manual(values = c(2,2,1,1))+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26))
EDfigure3d_2<-ggscatterhist(na.omit(Nation_CV_log), x= 'lgArea.avg', y= 'lgAvgS',
                          shape = 21, color = "NA", alpha = 0.0,
                          palette = c("#067A8F","#357EBDFF","#E1872799","#BA3822"),
                          ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                         axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                         axis.text.x=element_text(vjust=0,size=22),
                                                         axis.text.y=element_text(hjust=0,size=22),
                                                         legend.key.size = unit(1.2, 'cm'),
                                                         axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                         axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                          margin.plot = "histogram",
                          margin.params = list(fill = "Size.four", color = "Black", size = 0.2),
                          margin.plot.size = 0.6,
                          xlab = "log10(Average area of crops)",
                          ylab = "log10(Average crop stability)",
                          legend = c("NA"),
                          ggp = EDfigure3d_1)

#multivariate analyses & stepwise selection (ED table 1)

Nation_CV[Nation_CV$Warfare == 0,]$Warfare<- min(Nation_CV[Nation_CV$Warfare != 0,]$Warfare)/10
fullmodel.ED.T1.NYS<- gls(log10(Yield.S)~EH+log10(Area.h)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),  Nation_CV[is.na(Nation_CV$AHDI)==0,], method="ML")
stepAIC(fullmodel.ED.T1.NYS,direction="both",trace=0)
model.ED.T1.NYS<- gls(log10(Yield.S)~EH+log10(Area.h)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(Warfare), correlation=corAR1(form=~1|Country),  Nation_CV,method="ML")
stepAIC(model.ED.T1.NYS,direction="both",trace=0)
summary(model.ED.T1.NYS)
vif(model.ED.T1.NYS)
ad.R(model.ED.T1.NYS)

fullmodel.ED.T1.ACS<- gls(log10(Avg.crop.S)~EH+log10(Area.h)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(fullmodel.ED.T1.ACS,direction="both",trace=0)
model.ED.T1.ACS<- gls(log10(Avg.crop.S)~log10(Area.h)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(SOC), correlation=corAR1(form=~1|Country),   Nation_CV, method="ML")
stepAIC(model.ED.T1.ACS,direction="both",trace=0)
summary(model.ED.T1.ACS)
vif(model.ED.T1.ACS)
ad.R(model.ED.T1.ACS)

fullmodel.ED.T1.ASY<- gls(log10(asy)~EH+log10(Area.h)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(fullmodel.ED.T1.ASY,direction="both",trace=0)
model.ED.T1.ASY<- gls(log10(asy)~EH+log10(Area.h)+log10(LPre.n)+log10(tmp.cv)+log10(SOC)+soildiversity+log10(Warfare)+Decade, correlation=corAR1(form=~1|Country),   Nation_CV,method="ML")
stepAIC(model.ED.T1.ASY,direction="both",trace=0)
model.ED.T1.ASY<- gls(log10(asy)~EH+log10(LPre.n)+log10(tmp.cv)+log10(SOC)+soildiversity+log10(Warfare)+Decade, correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(model.ED.T1.ASY,direction="both",trace=0)
summary(model.ED.T1.ASY)
anova(model.ED.T1.ASY)
vif(model.ED.T1.ASY)
rsquared(model.ED.T1.ASY)
ad.R(model.ED.T1.ASY)

#multivariate analyses & stepwise selection (ED table 3)

fullmodel.ED.T2.NYS<- gls(log10(Yield.S)~EH+log10(Cropland)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),  Nation_CV[is.na(Nation_CV$AHDI)==0,], method="ML")
stepAIC(fullmodel.ED.T2.NYS,direction="both",trace=0)
model.ED.T2.NYS<- gls(log10(Yield.S)~EH+log10(Cropland)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(Warfare), correlation=corAR1(form=~1|Country),  Nation_CV,method="ML")
stepAIC(model.ED.T2.NYS,direction="both",trace=0)
summary(model.ED.T2.NYS)
vif(model.ED.T2.NYS)
ad.R(model.ED.T2.NYS)

fullmodel.ED.T2.ACS<- gls(log10(Avg.crop.S)~EH+log10(Cropland)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(fullmodel.ED.T2.ACS,direction="both",trace=0)
model.ED.T2.ACS<- gls(log10(Avg.crop.S)~log10(Cropland)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(SOC), correlation=corAR1(form=~1|Country),   Nation_CV, method="ML")
stepAIC(model.ED.T2.ACS,direction="both",trace=0)
summary(model.ED.T2.ACS)
vif(model.ED.T2.ACS)
ad.R(model.ED.T2.ACS)

fullmodel.ED.T2.ASY<- gls(log10(asy)~EH+log10(Cropland)+log10(pre.cv)+log10(LPre.n)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade+log10(Warfare)+log10(SOC)+log10(Htmp.n)+soildiversity+log10(AHDI), correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(fullmodel.ED.T2.ASY,direction="both",trace=0)
model.ED.T2.ASY<- gls(log10(asy)~EH+log10(LPre.n)+log10(tmp.cv)+log10(SOC)+soildiversity+log10(Warfare)+Decade, correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(model.ED.T2.ASY,direction="both",trace=0)
model.ED.T2.ASY<- gls(log10(asy)~EH+log10(LPre.n)+log10(tmp.cv)+log10(SOC)+log10(Warfare)+Decade, correlation=corAR1(form=~1|Country),   Nation_CV[is.na(Nation_CV$AHDI)==0,],method="ML")
stepAIC(model.ED.T2.ASY,direction="both",trace=0)
summary(model.ED.T2.ASY)
anova(model.ED.T2.ASY)
vif(model.ED.T2.ASY)
rsquared(model.ED.T2.ASY)
ad.R(model.ED.T2.ASY)
