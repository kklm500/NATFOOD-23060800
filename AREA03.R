#Crop_filter
Crop<-na.omit(cropCalories_global)
world_crop<- Production_Crops_Livestock_E_All_Data_Normalized_%>% filter(Area == "World") %>% filter(Element == "Production")
world_crop<- na.omit(world_crop[world_crop$Item %in% unlist(Crop[,1]),])
world_crop<- aggregate(world_crop[,10],by= list(world_crop$Item),mean)
write.csv(world_crop,"world_crop.csv")

#Crop_global
group<- unique(Production_Crops_Livestock_E_All_Area_Groups$Area)

Crop_global <- Production_Crops_Livestock_E_All_Data_Normalized_ %>%
  filter(!Area %in% group)%>%
  filter(Item %in% cropCalories_global$Item)%>%
  #filter(Value > 0)%>%
  filter(Element != "Yield")%>%
  select(Area,Item,Element,Year,Value)%>%
  spread(Element,Value)%>%
  na.omit()%>%
  left_join(cropCalories_global, by = "Item")%>%
  mutate(Production_mCal = Production * Calories_kcal_t/1000)
Crop_global$Decade<- NA
Crop_global[Crop_global$Year >1960 & Crop_global$Year <1971,]$Decade<- 1960
Crop_global[Crop_global$Year >1970 & Crop_global$Year <1981,]$Decade<- 1970
Crop_global[Crop_global$Year >1980 & Crop_global$Year <1991,]$Decade<- 1980  
Crop_global[Crop_global$Year >1990 & Crop_global$Year <2001,]$Decade<- 1990
Crop_global[Crop_global$Year >2000 & Crop_global$Year <2011,]$Decade<- 2000
Crop_global[Crop_global$Year >2010 & Crop_global$Year <2021,]$Decade<- 2010

Crop_global$Order<-paste("Y",Crop_global$Year - Crop_global$Decade,sep = "_")
colnames(Crop_global)[1]<- "Country"
colnames(Crop_global)[4]<- "Area"

Crop_global_T <- Crop_global%>%
  filter(Area > 0)%>%
  select(-Year,-Calories_kcal_t)%>%
  gather(Element,Value, Area, Production, Production_mCal)%>%
  spread(Order, Value)%>%
  select(Country,Item,Element,Decade,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10)%>%
  na.omit()

China_Crop<-Crop_global_T %>%
  filter(Country == "China, Hong Kong SAR"|Country == "China, Macao SAR"|Country == "China, mainland"|Country == "China, Taiwan Province of")%>%
  select(-Country)
China_Crop<-aggregate(China_Crop[,4:13],by = list(Item = China_Crop$Item, Element = China_Crop$Element, Decade = China_Crop$Decade), sum)%>%
  mutate(Country = "China")%>%
  select(Country,Item,Element,Decade,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10)

Crop_global_T<-Crop_global_T%>%
  filter(Country != "China, Hong Kong SAR"& 
           Country != "China, Macao SAR"&
           Country != "China, mainland"&
           Country != "China, Taiwan Province of"&
           Country != "China")%>%
  rbind(China_Crop)
Crop_global_T<- na.omit(Crop_global_T)
length(unique(Crop_global_T$Country))

global_cv_1<- Crop_global_T%>%
  gather(Order,Value,Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10)%>%
  separate(Order,c("Y","Order"), sep= "_")%>%
  select(-Y)%>%
  spread(Element, Value)%>%
  select(-Production)
global_cv_1$Order<- as.numeric(global_cv_1$Order) 

global_cv_2<- global_cv_1%>%
  arrange(desc(Order))%>%
  group_by(Country,Decade, Order)%>%
  summarise(Area.T = sum(Area),
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
            EH = mean(EH))%>%
  mutate(CV = sd.yield/mean.yield)

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
  mutate(cv.w = cv.crop*weight)%>%
  group_by(Country,Decade)%>%
  summarise(sd.sum = sum(sd.yield.crop),
            cv.avg = sum(cv.w))%>%
  left_join(global_cv_3, by = c("Country", "Decade"))%>%
  mutate(phi= sd.yield/sd.sum)%>%
  mutate(cv = cv.avg*phi)

Nation_CV_full <- global_cv_4%>%
  mutate(Yield.S = 1/cv,
         Avg.crop.S = 1/cv.avg)%>%
  select(Country,Decade,mean.yield,Area.h,Richness,EH,Yield.S,Avg.crop.S,phi)

#adding Irrigation information
irrigation<- Environment_LandUse_E_All_Data_Normalized_%>%
  filter(Item == "Land area equipped for irrigation",
         Element == "Share in Cropland")%>%
  select(Area,Year,Value)
irrigation$Decade<- NA
irrigation[irrigation$Year >1960 & irrigation$Year <1971,]$Decade<- 1960
irrigation[irrigation$Year >1970 & irrigation$Year <1981,]$Decade<- 1970
irrigation[irrigation$Year >1980 & irrigation$Year <1991,]$Decade<- 1980  
irrigation[irrigation$Year >1990 & irrigation$Year <2001,]$Decade<- 1990
irrigation[irrigation$Year >2000 & irrigation$Year <2011,]$Decade<- 2000
irrigation[irrigation$Year >2010 & irrigation$Year <2021,]$Decade<- 2010
colnames(irrigation)<- c("Country", "Year", "Irrigation","Decade")
irrigation<- irrigation%>%
  group_by(Country,Decade)%>%
  summarise(Irrigation = mean(Irrigation))

Nation_CV_full<- left_join(Nation_CV_full, irrigation, by = c("Country","Decade"))

#adding N fertilizer information
fertilizer<- Environment_Fertilizers_E_All_Data_Normalized_%>%
  filter(Item == "Nutrient nitrogen N (total)")%>%
  select(Area,Year,Value)
fertilizer$Decade<- NA
fertilizer[fertilizer$Year >1960 & fertilizer$Year <1971,]$Decade<- 1960
fertilizer[fertilizer$Year >1970 & fertilizer$Year <1981,]$Decade<- 1970
fertilizer[fertilizer$Year >1980 & fertilizer$Year <1991,]$Decade<- 1980  
fertilizer[fertilizer$Year >1990 & fertilizer$Year <2001,]$Decade<- 1990
fertilizer[fertilizer$Year >2000 & fertilizer$Year <2011,]$Decade<- 2000
fertilizer[fertilizer$Year >2010 & fertilizer$Year <2021,]$Decade<- 2010
colnames(fertilizer)<- c("Country", "Year", "N.use","Decade")
fertilizer<- fertilizer%>%
  group_by(Country,Decade)%>%
  summarise(N.use = mean(N.use))
Nation_CV_full<- left_join(Nation_CV_full, fertilizer, by = c("Country","Decade"))

#adding Area information
#Unified country name
FAOSTAT_data_landarea[FAOSTAT_data_landarea$Area == "Türkiye",]$Area<-"Turkey"
FAOSTAT_data_landarea[FAOSTAT_data_landarea$Area == "Côte d'Ivoire",]$Area<-"Cote d'Ivoire"
FAOSTAT_data_landarea[FAOSTAT_data_landarea$Area == "Réunion",]$Area<-"Reunion"

Nation_CV_full[Nation_CV_full$Country == "Ethiopia PDR",]$Country <- "Ethiopia"
Nation_CV_full[Nation_CV_full$Country == "Belgium-Luxembourg",]$Country <- "Belgium"
Nation_CV_full[Nation_CV_full$Country == Nation_CV_full[791,]$Country,]$Country <- "Reunion"
Nation_CV_full[Nation_CV_full$Country == Nation_CV_full[146,]$Country,]$Country <- "Cote d'Ivoire"


Land.area<- FAOSTAT_data_landarea%>%
  select(Area,Year,Item,Value)%>%
  spread(Item, Value)
Land.area$Decade<- NA
Land.area[Land.area$Year >1960 & Land.area$Year <1971,]$Decade<- 1960
Land.area[Land.area$Year >1970 & Land.area$Year <1981,]$Decade<- 1970
Land.area[Land.area$Year >1980 & Land.area$Year <1991,]$Decade<- 1980  
Land.area[Land.area$Year >1990 & Land.area$Year <2001,]$Decade<- 1990
Land.area[Land.area$Year >2000 & Land.area$Year <2011,]$Decade<- 2000
Land.area[Land.area$Year >2010 & Land.area$Year <2021,]$Decade<- 2010
colnames(Land.area)<- c("Country", "Year", "Cropland","landarea","Decade")
Landarea2000<- FAOSTAT_data_landarea%>%
  select(Area,Year,Item,Value)%>%
  spread(Item, Value)%>%
  filter(Year %in% c(2001:2010))%>%
  group_by(Area)%>%
  summarise(landarea = mean(`Land area`, na.rm= T))
colnames(Landarea2000)<-c("Country", "Landarea")

Land.area<- Land.area%>%
  group_by(Country,Decade)%>%
  summarise(Cropland = mean(Cropland))
Nation_CV_full<- left_join(Nation_CV_full, Land.area, by = c("Country","Decade"))
Nation_CV_full<- left_join(Nation_CV_full, Landarea2000, by = "Country")


#filtering nations

Country.rm<-  c("Egypt", "Guinea",  "Kenya", "Mozambique", "Netherlands", "Zambia")
Nation_CV<- Nation_CV_full%>% filter(!Country %in% Country.rm)%>% na.omit()
length(unique(Nation_CV$Country))

#worldmap and cropland
world.polygon<- readOGR( "E:/Croparea-stability/AgriculturalStability-master/spatial/countries_global.shp")
country<- country_name[country_name$Areaname_FAO %in% unique(Nation_CV$Country),]$Areaname_worldmap
world.polygon2<-world.polygon[world.polygon$Area %in% country,]

world.ID<- data.frame(ID = 1:length(world.polygon2@data[,1]),Areaname_worldmap=world.polygon2@data[,1], Lat = centroid(world.polygon2)[,2])
world.ID$hemisphere<- NA
world.ID[world.ID$Lat> 0,]$hemisphere <- "N"
world.ID[world.ID$Lat<= 0,]$hemisphere <- "S"
cropland2000<- raster("E:/Croparea-stability/Cropland2000_5m.tif")
cropland2000<- aggregate(cropland2000, fact= 0.5/res(cropland2000), fun = mean, na.rm=TRUE)
world<- rasterize(world.polygon2, cropland2000)
cropland2000<- brick(world,cropland2000)
cropland2000.df<-data.frame(cropland2000@data@values)


#crop calendar
#distinguishing between the northern and southern hemispheres
crop.calendar.list<- list.files("E:/Croparea-stability/ALL_CROPS_netCDF_0.5deg_unfilled")
setwd("E:/Croparea-stability/ALL_CROPS_netCDF_0.5deg_unfilled")

for (i in 1:19) {
  crop.plant.calendar.i<- stack(crop.calendar.list[i], varname = "plant")
  crop.plant.calendar.i<- aggregate(crop.plant.calendar.i,fact=0.5/(res(crop.plant.calendar.i))[1], fun = mean)
  crop.plant.calendar<-stack(crop.plant.calendar, crop.plant.calendar.i)
}
crop.plant.calendar.min<- calc(crop.plant.calendar,min)

crop.harvest.calendar<- stack()
for (i in 1:19) {
  crop.harvest.calendar.i<- stack(crop.calendar.list[i], varname = "harvest")
  crop.harvest.calendar.i<- aggregate(crop.harvest.calendar.i,fact=0.5/(res(crop.harvest.calendar.i))[1], fun = mean)
  crop.harvest.calendar<-stack(crop.harvest.calendar, crop.harvest.calendar.i)
}
crop.harvest.calendar.max<- calc(crop.harvest.calendar,max)
crop.calendar<- stack(crop.plant.calendar.min, crop.harvest.calendar.max)

#filtering cropland
crop.calendar<- stack(crop.calendar,cropland2000)
crop.calendar<- brick(crop.calendar)

daymonth<- function(x){
  if(x<=31){y = 1} 
  else if(x>31 & x<=59) {y = 2}
  else if(x>59 & x<=90) {y = 3}
  else if(x>90 & x<=120) {y = 4}
  else if(x>120 & x<=151) {y = 5}
  else if(x>151 & x<=181) {y = 6}
  else if(x>181 & x<=212) {y = 7}
  else if(x>212 & x<=243) {y = 8}
  else if(x>243 & x<=273) {y = 9}
  else if(x>273 & x<=304) {y = 10}
  else if(x>304 & x<= 334) {y = 11}
  else if(x>334 & x<=365) {y = 12}
  else if(x >= 366) {y <- NA}
  return(y)
}


calendar.df<-data.frame(crop.calendar@data@values)
colnames(calendar.df)<- c("plant", "harvest","ID","cropland")
calendar.df<- calendar.df%>% filter(cropland>0, ID>0)

calendar.df<- calendar.df%>%
  group_by(ID)%>%
  summarise(plant.d = mean(plant, na.rm=TRUE),
            harvest.d = mean(harvest, na.rm=TRUE))
calendar.df[is.na(calendar.df)]<- 366

c.m.df<-c()
for (i in 1:nrow(calendar.df)) {
  plant.m = daymonth(calendar.df[i,]$plant.d)
  harvest.m = daymonth(calendar.df[i,]$harvest.d)
  c.m.df<- rbind(c.m.df,c(plant.m,harvest.m))
}
calendar.df<- cbind(calendar.df,c.m.df)
colnames(calendar.df)[4:5]<-c("plant.m","harvest.m")
calendar.df<- left_join(calendar.df,world.ID[,c(1,4)], by = "ID")
#defaulting the growing season of nation that data lost is whole year
calendar.df$plant.m[is.na(calendar.df$plant.m)]<- 1
calendar.df$harvest.m[is.na(calendar.df$harvest.m)]<- 12
growingseason<- list()
for (i in 1:nrow(calendar.df)) {
  calendar.df.i<- calendar.df[i,]
  if(calendar.df.i$hemisphere == "N" & calendar.df.i$plant.m < calendar.df.i$harvest.m){growingseason.i<- c(calendar.df.i$plant.m:calendar.df.i$harvest.m)}
  else if(calendar.df.i$hemisphere == "N" & calendar.df.i$plant.m >= calendar.df.i$harvest.m){growingseason.i<- c(1:calendar.df.i$harvest.m, calendar.df.i$plant.m:12)}
  else if(calendar.df.i$hemisphere == "S" & calendar.df.i$plant.m >= calendar.df.i$harvest.m){growingseason.i<- c(calendar.df.i$plant.m:12, 1:calendar.df.i$harvest.m)}
  else if(calendar.df.i$hemisphere == "S" & calendar.df.i$plant.m < calendar.df.i$harvest.m){growingseason.i<- c(1:12)}
  growingseason[[i]]<- growingseason.i
}
names(growingseason)<- calendar.df$ID

#precipitation
library(ncdf4)
setwd("E:/Croparea-stability/Weat_data/precipitation")
pre.1<- stack("cru_ts4.05.1961.1970.pre.dat.nc",varname = "pre")
pre.2<- stack("cru_ts4.05.1971.1980.pre.dat.nc",varname = "pre")
pre.3<- stack("cru_ts4.05.1981.1990.pre.dat.nc",varname = "pre")
pre.4<- stack("cru_ts4.05.1991.2000.pre.dat.nc",varname = "pre")
pre.5<- stack("cru_ts4.05.2001.2010.pre.dat.nc",varname = "pre")
pre.6<- stack("cru_ts4.05.2011.2020.pre.dat.nc",varname = "pre")
list.pre<- list(pre.1,pre.2,pre.3,pre.4,pre.5,pre.6)

Pre<-data.frame()
Decade<- c("1960","1970","1980","1990","2000","2010")
for (i in 1:6) {
  pre.x <- list.pre[[i]]
  pre.x<- stack(pre.x,cropland2000)# filtering cropland and ID
  for (j in 1:10) {
    pre.x.j<- pre.x[[c(((j-1)*12+1): (j*12),121,122)]]
    pre.x.j<-brick(pre.x.j)
    pre.x.j.df<- na.omit(data.frame(pre.x.j@data@values))
    colnames(pre.x.j.df)<- c(1,2,3,4,5,6,7,8,9,10,11,12,"ID","cropland")
    pre.x.j.df<- pre.x.j.df %>% filter(cropland > 0) %>% arrange(ID)
    for (k in 1:length(unique(pre.x.j.df$ID))) {
      ID = unique(pre.x.j.df$ID)[k]
      pre.x.j.df.k <- pre.x.j.df[pre.x.j.df$ID == ID,]%>%
        gather(Month, pre, 1:12)%>%
        mutate(pre.W = pre*cropland)%>%
        filter(Month %in% growingseason[[k]])%>%
        group_by(Month)%>%
        summarise(pre.W = sum(pre.W),
                  cropland = sum(cropland))%>%
        mutate(pre = pre.W/cropland)
      pre.annual = sum(pre.x.j.df.k$pre) 
      Pre <- rbind(Pre, c(Decade[i],j,ID,pre.annual))
    }
    }}
colnames(Pre)<- c("Decade","Order","ID","pre.annual")
Pre<- apply(Pre,2, as.numeric)%>% as.data.frame()
Pre.cv<- Pre%>%
  left_join(world.ID[,1:2], by = "ID")%>%
  left_join(country_name, by = "Areaname_worldmap")%>%
  group_by(Decade, Areaname_FAO)%>% 
  summarise(pre.mean = mean(pre.annual),
            pre.sd = sd(pre.annual))%>%
  mutate(pre.cv = pre.sd/pre.mean)%>%
  select(Areaname_FAO,Decade,pre.cv)


setwd("E:/Croparea-stability/Weat_data/temperature")
tmp.1<- stack("cru_ts4.05.1961.1970.tmp.dat.nc",varname = "tmp")
tmp.2<- stack("cru_ts4.05.1971.1980.tmp.dat.nc",varname = "tmp")
tmp.3<- stack("cru_ts4.05.1981.1990.tmp.dat.nc",varname = "tmp")
tmp.4<- stack("cru_ts4.05.1991.2000.tmp.dat.nc",varname = "tmp")
tmp.5<- stack("cru_ts4.05.2001.2010.tmp.dat.nc",varname = "tmp")
tmp.6<- stack("cru_ts4.05.2011.2020.tmp.dat.nc",varname = "tmp")
list.tmp<- list(tmp.1,tmp.2,tmp.3,tmp.4,tmp.5,tmp.6)

Tmp<-data.frame()
Decade<- c("1960","1970","1980","1990","2000","2010")
for (i in 1:6) {
  tmp.x <- list.tmp[[i]]
  tmp.x<- stack(tmp.x,cropland2000)# filtering cropland and ID
  for (j in 1:10) {
    tmp.x.j<- tmp.x[[c(((j-1)*12+1): (j*12),121,122)]]
    tmp.x.j<-brick(tmp.x.j)
    tmp.x.j.df<- na.omit(data.frame(tmp.x.j@data@values))
    colnames(tmp.x.j.df)<- c(1,2,3,4,5,6,7,8,9,10,11,12,"ID","cropland")
    tmp.x.j.df<- tmp.x.j.df %>% filter(cropland > 0) %>% arrange(ID)
    for (k in 1:length(unique(tmp.x.j.df$ID))) {
      ID = unique(tmp.x.j.df$ID)[k]
      tmp.x.j.df.k <- tmp.x.j.df[tmp.x.j.df$ID == ID,]%>%
        gather(Month, tmp, 1:12)%>%
        mutate(tmp.W = tmp*cropland)%>%
        filter(Month %in% growingseason[[k]])%>%
        group_by(Month)%>%
        summarise(tmp.W = sum(tmp.W),
                  cropland = sum(cropland))%>%
        mutate(tmp = tmp.W/cropland)
      tmp.annual = mean(tmp.x.j.df.k$tmp) 
      Tmp <- rbind(Tmp, c(Decade[i],j,ID,tmp.annual))
    }
  }}
colnames(Tmp)<- c("Decade","Order","ID","tmp.annual")
Tmp<- apply(Tmp,2, as.numeric)%>% as.data.frame()
Tmp.cv<- Tmp%>%  
  left_join(world.ID[,1:2], by = "ID")%>%
  left_join(country_name, by = "Areaname_worldmap")%>%
  group_by(Decade, Areaname_FAO)%>%
  summarise(tmp.mean = mean(tmp.annual),
            tmp.sd = sd(tmp.annual))%>%
  mutate(tmp.cv = tmp.sd/tmp.mean)%>%
  select(Areaname_FAO,Decade,tmp.cv)  
  
Nation_CV<- left_join(Nation_CV,Pre.cv, by =c("Country" = "Areaname_FAO", "Decade"= "Decade"))
Nation_CV<- left_join(Nation_CV,Tmp.cv, by =c("Country" = "Areaname_FAO", "Decade"= "Decade"))
Nation_CV<- na.omit(Nation_CV)
Nation_CV$Area.avg<- Nation_CV$Area.h/Nation_CV$EH

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
            lgTmpCV = log10(tmp.cv))
Q<- fivenum(Nation_CV_log$lgLandarea)
quantile(Nation_CV_log$lgLandarea)
length(Nation_CV_log$lgLandarea[Nation_CV_log$lgLandarea > 4.886987])
length(Nation_CV_log$lgLandarea[Nation_CV_log$lgLandarea >= 4.474464 & Nation_CV_log$lgLandarea<= 4.886987])
length(Nation_CV_log$lgLandarea[Nation_CV_log$lgLandarea >=  Q[2] & Nation_CV_log$lgLandarea<= Q[3]])
length(Nation_CV_log$lgLandarea[Nation_CV_log$lgLandarea >= 2.710117 & Nation_CV_log$lgLandarea<= 3.961450])
Nation_CV_log$Country.Size<- NA
Nation_CV_log[Nation_CV_log$lgLandarea >= Q[4]& Nation_CV_log$lgLandarea <= Q[5],]$Country.Size <- 4
Nation_CV_log[Nation_CV_log$lgLandarea >= Q[3]& Nation_CV_log$lgLandarea < Q[4],]$Country.Size <- 3
Nation_CV_log[Nation_CV_log$lgLandarea >= Q[2]& Nation_CV_log$lgLandarea < Q[3],]$Country.Size <- 2
Nation_CV_log[Nation_CV_log$lgLandarea >= Q[1]& Nation_CV_log$lgLandarea < Q[2],]$Country.Size <- 1
Nation_CV_log$Country.Size.f<- as.factor(Nation_CV_log$Country.Size)
Nation_CV<- left_join(Nation_CV[,-17],Nation_CV_log[,c(1,2,17)], by = c("Country", "Decade"))


Nation_CV_log<-arrange(Nation_CV_log, (Country.Size))

plot1.1<-ggplot(Nation_CV_log)+geom_point(aes(x= lgAreaH, y=lgYieldS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0.3,3.0)+
  labs(x = "log10(Total harvested area)", y = "log10(National yield stability)")+
  geom_smooth(aes(x= lgAreaH, y=lgYieldS),method = "lm",formula = y ~ I(x), size = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",size=1.0),
        axis.line.y=element_line(linetype=1,color="black",size=1.0))

anova(lm(log10(Yield.S)~ log10(Area.h), Nation_CV))
ggsave("harvestedarea~yieldS.PNG",device = png, dpi = 300)

scaleFUN <- function(x) sprintf("%.2f", x) 
plot.1<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgYieldS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.3, 3.8))+
  #ylim(0.3,3.2)+
  #geom_smooth(aes(x= EH,y=lgYieldS),method = "lm",formula = y ~ I(x), size = 2.0,color = "Black", fill = NA)+
  geom_smooth(aes(x= EH,y=lgYieldS, color = Country.Size.f , fill = Country.Size.f),alpha = 0.2, method = "lm",formula = y ~ I(x),size = 1.5)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))

plot.2<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgYieldS',
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
                      margin.params = list(fill = "Country.Size.f", color = "Black", size = 0.2),
                      margin.plot.size = 0.6,
                      xlab = "Effective diversity",
                      ylab = "log10(National yield stability)",
                      legend = c("NA"),
                      ggp = plot.1)
ggsave("Figure2-1.PNG",device = png,width = 3350, height = 4300,units = c("px"), dpi = 600)
anova(lme(log10(Yield.S)~EH+EH:Country.Size,random = ~1|Country, Nation_CV))
anova(lm(log10(Area.avg)~EH*as.character(Country.Size), Nation_CV))

plot.3<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgAsy),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.00,1.35))+
  #ylim(-0.93,0.15)+
  #geom_smooth(aes(x= EH,y=lgPhi),method = "lm",formula = y ~ I(x), size = 2.0,color = "black", fill = NA,)+
  geom_smooth(aes(x= EH,y=lgAsy, color = Country.Size.f , fill = Country.Size.f),alpha = 0.2, method = "lm",formula = y ~ I(x),size = 1.5)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22))

plot.4<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgAsy',
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
                      margin.params = list(fill = "Country.Size.f", color = "Black", size = 0.2),
                      margin.plot.size = 0.6,
                      xlab = "Effective diversity",
                      ylab = "log10(crop asynchrony)",
                      legend = c("NA"),
                      ggp = plot.3)

ggsave("Figure2-2.PNG",width = 3350, height = 4300,units = c("px"),device = png, dpi = 600)

plot.5<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgAvgS),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(0.2,2.9))+
  #ylim(0.2,2.5)+
  #geom_smooth(aes(x= EH,y=lgAvgS),method = "lm",formula = y ~ I(x), size = 2.0,color = "black", fill = NA,)+
  geom_smooth(aes(x= EH,y=lgAvgS, color = as.factor(Country.Size) , fill = as.factor(Country.Size)),alpha = 0.2, method = "lm",formula = y ~ I(x),size = 1.5)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26))

plot.6<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgAvgS',
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
                      margin.params = list(fill = "Country.Size.f", color = "Black", size = 0.2),
                      margin.plot.size = 0.6,
                      xlab = "Effective diversity",
                      ylab = "log10(Average crop stability)",
                      legend = c("NA"),
                      ggp = plot.5)

ggsave("Figure2-3.PNG",width = 3350, height = 4300,units = c("px"),device = png, dpi = 600)

plot.7<- ggplot(Nation_CV_log)+geom_point(aes(x= EH, y=lgArea.avg),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN,limits=c(2.8,9.2))+
  #ylim(2.8,8.2)+
  #geom_smooth(aes(x= EH,y=lgArea.avg),method = "lm",formula = y ~ I(x), size = 2.0,color = "black", fill = NA,)+
  geom_smooth(aes(x= EH,y=lgArea.avg, color = as.factor(Country.Size) , fill = as.factor(Country.Size)),alpha = 0.2, method = "lm",formula = y ~ I(x),size = 1.5)+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, size=26),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26))

plot.8<-ggscatterhist(Nation_CV_log, x= 'EH', y= 'lgArea.avg',
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
                      margin.params = list(fill = "Country.Size.f", color = "Black", size = 0.2, alpha = 0.1),
                      margin.plot.size = 0.6,
                      xlab = "Effective diversity",
                      ylab = "log10(Average crop area)",
                      legend = c("NA"),
                      ggp = plot.7)
ggsave("Figure2-4.PNG",width = 3350, height = 4300, units = c("px"),device = png, dpi = 600)
ggarrange(plot.1, plot.3)


model1<- lmer(scale(log10(Yield.S)) ~ scale(EH)+(1|Country)+(0+scale(EH)|Country), data = Nation_CV)
SRC<-data.frame(row.names(ranef(model1)$Country),ranef(model1)$Country)%>%
  rowwise()%>%
  mutate(Intercpt = X.Intercept. + summary(model1)$coefficients[1,1],
         EH_slope = scale.EH. + summary(model1)$coefficients[2,1])
colnames(SRC)[1]<- "Country"
SRC<- left_join(SRC, Landarea2000, by ="Country")
SRC<-left_join(SRC,HDI, by = "Country")
avg.area<- Nation_CV %>% group_by(Country)%>% summarise(avg.area = mean(Area.avg))
SRC<- left_join(SRC, avg.area, by ="Country")
SRC2<- country_name[country_name$Areaname_FAO %in% unique(Nation_CV$Country),]%>% left_join(SRC[,c(1,5,6,7,8)], by =c("Areaname_FAO"="Country"))
world.polygon@data<- left_join(world.polygon@data, SRC2[,c(1,3,6)], by= c("Area"="Areaname_worldmap"))
world.ID2<- world.polygon@data
world.ID2$id<- 0:(nrow(world.ID2)-1)
world.ID2$id<- as.character(world.ID2$id)
world2.df<-fortify(world.polygon)
world2.df<- left_join(world2.df,world.ID2[,c(2,3,4)], by = "id")
world2.df$HDI<-as.numeric(world2.df$HDI)
sum(Nation_CV[Nation_CV$Decade == 2000,]$Cropland)/1505263.42

windows()
ggplot(world2.df)+
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
windows()
ggsave("global.png",device = "png",dpi =600)


ggplot(SRC2[!duplicated(SRC2$Areaname_FAO),], aes(x= log10(Landarea),y=EH_slope, fill= EH_slope, size = Landarea))+geom_point(shape = 21, alpha = 0.8)+
  geom_hline(aes(yintercept= 0), size =2,color = "#858585", linetype = 2)+
  theme_bw()+
  scale_fill_gradientn(values = c (0,0.34,0.5,0.65,0.75,1.0),colors =rev(c("#A31E21","#D96241","#E1872799","#7C878EFF","#357EBDFF")))+
  geom_smooth(aes(),method = "lm",formula = y ~ I(x), linewidth = 2.5,color = "black",fill= NA)+
  scale_size_continuous(range=c(1,18))+
  xlab("Log10(Landarea)")+
  ylab("The Stabilizing effect of diversity")+
  xlim(2.5,6.3)+
  theme_classic()+theme(axis.title.y=element_text(hjust=0.5, vjust=2,size=30),
                        axis.title.x=element_text(hjust=0.5, vjust=2,size=30),
                        axis.text.x=element_text(size=26),
                        axis.text.y=element_text(size=26),
                        legend.key.size = unit(1.2, 'cm'),
                        axis.line.x=element_line(linetype=1,color="black",linewidth=1),
                        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
                        axis.ticks.length = unit(0.2, 'cm'),
                        legend.position="none")
SRC2$HDI<-as.numeric(SRC2)
anova(lm(EH_slope~log10(Landarea)*HDI,SRC2))

ggsave("Landarea~stabilizeseize.png", device = "png",dpi =600) 
anova(lm(EH_slope~log10(Landarea),SRC2[!duplicated(SRC2$Areaname_FAO),]))
Nation_CV$asy<-1/Nation_CV$phi
test_SEM<- psem(
  #lm(EH~ log10(Area.h), Nation_CV),
  lm(log10(pre.cv)~ log10(Area.h), Nation_CV),
  lm(log10(asy) ~ EH+log10(tmp.cv), Nation_CV),
  lm(log10(Avg.crop.S) ~ log10(Area.h)+log10(pre.cv)+log10(tmp.cv), Nation_CV),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy), Nation_CV),
  log10(Avg.crop.S) %~~% log10(asy),log10(tmp.cv) %~~% log10(pre.cv), data = as.data.frame(Nation_CV)
)
summary(test_SEM)

totalsizeplot<- data.frame(Item= c("Diversity","Area"), Size = c(0.209617,0.166371))
plotblank<- ggplot()+theme_classic()+theme_classic()+theme(axis.line.x=element_blank(),
                                                           axis.line.y=element_blank())
plot1.2<-ggplot(totalsizeplot, aes(x= Item, y = Size))+geom_bar(fill = "#BABABA",color = "black", stat = "identity", position = "dodge", width = 0.6)+
  scale_x_discrete(limits = c("Diversity","Area"))+
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
windows()
plot1.1/plotblank/plot1.2+plot_layout(heights = c(1.2, 0.1,0.7))

ggsave("totalsize.png",device = png, dpi = 300)

test_SEM<- psem(
  lm(log10(Area.avg) ~ EH, Nation_CV[Nation_CV$Country.Size == 4,]),
  lm(log10(asy) ~ EH, Nation_CV[Nation_CV$Country.Size == 4,]),
  lm(log10(Avg.crop.S) ~ log10(Area.avg), Nation_CV[Nation_CV$Country.Size == 4,]),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy),Nation_CV[Nation_CV$Country.Size == 4,]),
  log10(Avg.crop.S) %~~% log10(asy), data = as.data.frame(Nation_CV[Nation_CV$Country.Size == 4,])
)
summary(test_SEM)

test_SEM<- psem(
  lm(log10(Area.avg) ~ EH, Nation_CV[Nation_CV$Country.Size == 3,]),
  lm(log10(asy) ~ EH+log10(Area.avg), Nation_CV[Nation_CV$Country.Size == 3,]),
  lm(log10(Avg.crop.S) ~ EH, Nation_CV[Nation_CV$Country.Size == 3,]),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy)+log10(Area.avg),Nation_CV[Nation_CV$Country.Size == 3,]),
  log10(Avg.crop.S) %~~% log10(asy), data = as.data.frame(Nation_CV[Nation_CV$Country.Size == 3,])
)
summary(test_SEM)

test_SEM<- psem(
  #lm(log10(Area.avg) ~ EH, Nation_CV[Nation_CV$Country.Size == 2,]),
  lm(log10(asy) ~ EH, Nation_CV[Nation_CV$Country.Size == 2,]),
  lm(log10(Avg.crop.S) ~ log10(Area.avg), Nation_CV[Nation_CV$Country.Size == 2,]),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy),Nation_CV[Nation_CV$Country.Size == 2,]),
  log10(Avg.crop.S) %~~% log10(asy), data = as.data.frame(Nation_CV[Nation_CV$Country.Size == 2,])
)
summary(test_SEM)

test_SEM<- psem(
  #lm(log10(Area.avg) ~ EH, Nation_CV[Nation_CV$Country.Size == 1,]),
  lm(log10(asy) ~ EH, Nation_CV[Nation_CV$Country.Size == 1,]),
  lm(log10(Avg.crop.S) ~ log10(Area.avg)+EH, Nation_CV[Nation_CV$Country.Size == 1,]),
  lm(log10(Yield.S) ~ log10(Avg.crop.S) + log10(asy),Nation_CV[Nation_CV$Country.Size == 1,]),
  log10(Avg.crop.S) %~~% log10(asy), data = as.data.frame(Nation_CV[Nation_CV$Country.Size == 1,])
)

summary(test_SEM)


anova(lm(log10(Area.h)~Decade, Nation_CV[Nation_CV$Income_Group == "LM",]))
rsquared(lm(lm(log10(Area.h)~Decade, Nation_CV[Nation_CV$Income_Group == "LM",])))





summary(lm(log10(phi)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Avg.crop.S)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Yield.S)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(phi)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(Avg.crop.S)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(Yield.S)~EH*Country.Size+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Yield.S)~EH+log10(Landarea)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))

anova(lm(log10(asy)~log10(Area.h), Nation_CV))

summary(lm(log10(asy)~EH+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Avg.crop.S)~EH+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Yield.S)~EH+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))

summary(lm(log10(asy)~EH+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Avg.crop.S)~EH+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Yield.S)~EH+log10(Area.h)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))



anova(lm(log10(phi)~EH*Country.Size+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(Avg.crop.S)~EH*Country.Size+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(Yield.S)~EH*Country.Size+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
summary(lm(log10(Yield.S)~EH+log10(Landarea)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
anova(lm(log10(Yield.S)~EH*Country.Size+log10(Cropland)+log10(pre.cv)+log10(tmp.cv)+log10(Irrigation)+log10(N.use)+Decade, Nation_CV))
str(Nation_CV)
anova(lm(log10(Yield.S)~EH*log10(Landarea), Nation_CV))
anova(lm(log10(asy)~EH*log10(Landarea), Nation_CV))
anova(lm(log10(Avg.crop.S)~EH*log10(Landarea), Nation_CV))
anova(lm(log10(Area.avg)~EH*log10(Landarea), Nation_CV))


plot.2.1<-ggplot(Nation_CV)+ geom_point(aes(x= log10(Area.h), y=Richness),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,125)+
  labs(x = "log10(Total harvested area)", y = "Richness")+
  geom_smooth(aes(x= log10(Area.h), y=Richness),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

plot.2.2<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=EH),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,35)+
  labs(x = "log10(Total harvested area)", y = "Effective diversity")+
  geom_smooth(aes(x= log10(Area.h), y=EH),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.2.3<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(asy)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,1.2)+
  labs(x = "log10(Total harvested area)", y = "log10(Crop asynchrony)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(asy)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.2.4<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(pre.cv)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(-4,1)+
  labs(x = "log10(Total harvested area)", y = "log10(Pricipitation variation)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(pre.cv)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=25),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.2.5<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(tmp.cv)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(-3,0.5)+
  labs(x = "log10(Total harvested area)", y = "log10(Temperature variation)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(tmp.cv)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=25),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

plot.2.6<-ggplot(Nation_CV)+geom_point(aes(x= log10(Area.h), y=log10(Avg.crop.S)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,2.8)+
  labs(x = "log10(Total harvested area)", y = "log10(Average crop stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(Avg.crop.S)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=25),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=25),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 
windows()
plot.2.1+plot.2.2+plot.2.3+plot.2.4+plot.2.5+plot.2.6
ggsave("areafactors.png", width = 4596, height = 3059, units = c("px"),device = "png",dpi =300)

anova(lm(log10(Avg.crop.S)~ log10(Area.h), Nation_CV))

fit1<-aov(EH~as.factor(Country.Size), Nation_CV)
summary(fit1)
TukeyHSD(fit1)

#Major crops
Major_crops<- Crop_global_T%>%
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
  summarise(Area.h = mean(Area),
            mean.yield = mean(Yield),
            sd.yield = sd(detrend(Yield)))%>%
  mutate(YieldS = mean.yield/sd.yield)%>%
  mutate(lgYieldS = log10(YieldS))

boxplot.stats(Major_crops[Major_crops$Item == "Rice, paddy",]$lgYieldS)$out
Major_crops_filter<- Major_crops%>%
  group_by(Item)%>%
  filter(!lgYieldS %in% boxplot.stats(lgYieldS)$out)
length(Major_crops[Major_crops$Item == "Rice, paddy",]$YieldS)
length(Major_crops_filter[Major_crops_filter$Item == "Rice, paddy",]$YieldS)
 
plot.3.1<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Wheat",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

plot.3.2<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Rice, paddy",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.3.3<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Maize",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.3.4<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Soybeans",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 

plot.3.5<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Barley",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0))

plot.3.6<-ggplot(Major_crops_filter[Major_crops_filter$Item == "Sorghum",])+geom_point(aes(x= log10(Area.h), y=log10(YieldS)),shape = 21, color = "#BABABA", fill = "#E3E3E3", size = 4,alpha = 0.6)+
  theme_classic()+
  ylim(0,3.5)+
  labs(x = "log10(Harvested area)", y = "log10(Yield stability)")+
  geom_smooth(aes(x= log10(Area.h), y=log10(YieldS)),method = "lm",formula = y ~ I(x), linewidth = 2.0,color = "black", )+
  theme(axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0)) 
windows()
plot.3.1+plot.3.2+plot.3.3+plot.3.4+plot.3.5+plot.3.6
ggsave("majorcrops.png", device = "png",dpi =300)

anova(lm(log10(YieldS)~log10(Area.h), Major_crops_filter[Major_crops_filter$Item == "Wheat",]))
summary(lm(log10(YieldS)~log10(Area.h), Major_crops_filter[Major_crops_filter$Item == "Wheat",]))

# beta coefficient within group
betacoef.yieldS.m <-summary(lm(log10(Yield.S)~as.factor(Country.Size)+EH:as.factor(Country.Size),Nation_CV))
betacoef.yieldS <- cbind(group = c("Super-larger & Larger", "Middle", "Mid-small", "Small"), data.frame(betacoef.yieldS.m$coefficients[5:8,1:2]))
betacoef.asy.m <-summary(lm(log10(asy)~as.factor(Country.Size)+EH:as.factor(Country.Size),Nation_CV))
betacoef.asy <- cbind(group = c("Super-larger & Larger", "Middle", "Mid-small", "Small"), data.frame(betacoef.asy.m$coefficients[5:8,1:2]))
betacoef.Avg.crop.S.m <-summary(lm(log10(Avg.crop.S)~as.factor(Country.Size)+EH:as.factor(Country.Size),Nation_CV))
betacoef.Avg.crop.S <- cbind(group = c("Super-larger & Larger", "Middle", "Mid-small", "Small"), data.frame(betacoef.Avg.crop.S.m$coefficients[5:8,1:2]))
betacoef.Area.avg.m <- summary(lm(log10(Area.avg)~as.factor(Country.Size)+EH:as.factor(Country.Size),Nation_CV))
betacoef.Area.avg <- cbind(group = c("Super-larger & Larger", "Middle", "Mid-small", "Small"), data.frame(betacoef.Area.avg.m$coefficients[5:8,1:2]))


plot.4.1<- ggplot(betacoef.yieldS,aes(x = group, y = Estimate, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Estimate+Std..Error,ymin=Estimate-Std..Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-larger & Larger", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(-0.005,0.065)+
  ylab("Beta coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_blank(),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")
plot.4.2<- ggplot(betacoef.asy,aes(x = group, y = Estimate, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Estimate+Std..Error,ymin=Estimate-Std..Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-larger & Larger", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(0.00,0.035)+
  ylab("Beta coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_blank(),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")
plot.4.3<- ggplot(betacoef.Avg.crop.S,aes(x = group, y = Estimate, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Estimate+Std..Error,ymin=Estimate-Std..Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-larger & Larger", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(-0.02,0.04)+
  ylab("Beta coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(angle =20, hjust=0.7, vjust=0.8, size=21),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")
plot.4.4<- ggplot(betacoef.Area.avg,aes(x = group, y = Estimate, fill = group))+geom_bar(stat="identity",position="dodge", width = 0.8)+
  geom_errorbar(aes(ymax=Estimate+Std..Error,ymin=Estimate-Std..Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(limits = c("Super-larger & Larger", "Middle","Mid-small","Small"))+
  scale_fill_manual(values = c("#357EBDFF","#E1872799","#067A8F","#BA3822"))+
  ylim(-0.075,0.02)+
  ylab("Beta coefficient")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(angle =20, hjust=0.7, vjust=0.8, size=21),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.2, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position = "none")  

plot.4.1+plot.4.2+plot.4.3+plot.4.4
ggsave("Betacoef.png",device = png, dpi =600)
anova(lm(log10(EH)~Country.Size, Nation_CV))


