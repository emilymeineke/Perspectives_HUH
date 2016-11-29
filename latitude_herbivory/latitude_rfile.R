library(data.table)
library(ggplot2)
library(lme4)
library(nlme)
library(plyr)

centroids <- read.csv("county_centroids.csv", header=TRUE)
lat_herbiv <- read.csv("Herbivinherbaria_data.csv", header=TRUE)

centroids_NE <- subset(centroids, state=="Maine"|state=="New Hampshire"|state=="Vermont"|state=="Massachusetts"|
                         state=="Rhode Island"|state=="Connecticut")

#This is correct, but do need to figure out where the 20 specimens or so went when joined. Are these just 
#NA's? 
merged <- merge(lat_herbiv, centroids_NE, by=c("county","state"))

DECA <- subset(merged, plant_genus_species =="desmodium_canadense")
VAAN <- subset(merged, plant_genus_species =="vaccinium_angustifolium")
QUBI <- subset(merged, plant_genus_species =="quercus_bicolor")
CAOV <- subset(merged, plant_genus_species =="carya_ovata")

ggplot(merged, aes(x=decimalLatitude, y=leafremoved, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=leafremoved)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=leafremoved)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=leafremoved)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=leafremoved)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=skel, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=skel)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=skel)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=skel)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=skel)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=stippling, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=stippling)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=stippling)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=stippling)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=stippling)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=serpentine.mine, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=serpentine.mine)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=serpentine.mine)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=serpentine.mine)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=serpentine.mine)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=blotch.mine, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=blotch.mine)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=blotch.mine)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=blotch.mine)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=blotch.mine)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=sooty.mould, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=sooty.mould)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=sooty.mould)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=sooty.mould)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=sooty.mould)) + geom_point()

ggplot(merged, aes(x=decimalLatitude, y=unknown.lesions, colour=plant_genus_species)) + geom_point()
ggplot(DECA, aes(x=decimalLatitude, y=unknown.lesions)) + geom_point()
ggplot(VAAN, aes(x=decimalLatitude, y=unknown.lesions)) + geom_point()
ggplot(QUBI, aes(x=decimalLatitude, y=unknown.lesions)) + geom_point()
ggplot(CAOV, aes(x=decimalLatitude, y=unknown.lesions)) + geom_point()



#test of herbivory with latitude
#create binary data
merged$binary_noherbiv <- ifelse(merged$totalherbiv>0, 1, 0)
merged$binary_chew<- ifelse(merged$leafremoved>0, 1, 0)
merged$binary_skel <- ifelse(merged$skel>0, 1, 0)
merged$binary_stipp <- ifelse(merged$stippling>0, 1, 0)
merged$binary_SM <- ifelse(merged$sooty.mould>0, 1, 0)
merged$binary_lgalls <- ifelse(merged$leaf.galls>0, 1, 0)
merged$binary_blotchmines <- ifelse(merged$blotch.mine>0, 1, 0)
merged$binary_serpmines <- ifelse(merged$serpentine.mine>0, 1, 0)

#create percent with herbivory column
merged_noNAchew <- subset(merged, leafremoved != "NA")
merged_noNAchew$totalboxes <- 5
merged_noNAchew$chew_prop <- merged_noNAchew$leafremoved/merged_noNAchew$totalboxes

model <- glmer(chew_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAchew, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

chew_glm <- glm(chew_prop~decimalLatitude+plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
                 na.action=na.omit, data=merged_noNAchew)
summary(chew_glm)

#Same model with skeletonization
merged_noNAskel <- subset(merged, skel != "NA")
merged_noNAskel$totalboxes <- 5
merged_noNAskel$skel_prop <- merged_noNAskel$skel/merged_noNAskel$totalboxes

model <- glmer(skel_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAskel, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

skel_glm <- glm(skel_prop~decimalLatitude+plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
                na.action=na.omit, data=merged_noNAskel)
summary(skel_glm)

#Same model with stippling
merged_noNAstippling <- subset(merged, stippling != "NA")
merged_noNAstippling$totalboxes <- 5
merged_noNAstippling$stippling_prop <- merged_noNAstippling$stippling/merged_noNAstippling$totalboxes

model <- glmer(stippling_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAstippling, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

stipp_glm <- glm(stippling_prop~decimalLatitude*plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
                na.action=na.omit, data=merged_noNAstippling)
summary(stipp_glm)

#Same model with sooty mould
merged_noNAsooty.mould <- subset(merged, sooty.mould != "NA")
merged_noNAsooty.mould$totalboxes <- 5
merged_noNAsooty.mould$sooty.mould_prop <- merged_noNAsooty.mould$sooty.mould/merged_noNAsooty.mould$totalboxes

model <- glmer(sooty.mould_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAsooty.mould, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

SM_glm <- glm(sooty.mould_prop~decimalLatitude+plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
                 na.action=na.omit, data=merged_noNAsooty.mould)
summary(SM_glm)

#Same model with blotch mine
merged_noNAblotch.mine <- subset(merged, blotch.mine != "NA")
merged_noNAblotch.mine$totalboxes <- 5
merged_noNAblotch.mine$blotch.mine_prop <- merged_noNAblotch.mine$blotch.mine/merged_noNAblotch.mine$totalboxes

model <- glmer(blotch.mine_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAblotch.mine, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

BM_glm <- glm(blotch.mine_prop~decimalLatitude*plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
              na.action=na.omit, data=merged_noNAblotch.mine)
summary(BM_glm)

#Same model with serpentine mine
merged_noNAserpentine.mine <- subset(merged, serpentine.mine != "NA")
merged_noNAserpentine.mine$totalboxes <- 5
merged_noNAserpentine.mine$serpentine.mine_prop <- merged_noNAserpentine.mine$serpentine.mine/merged_noNAserpentine.mine$totalboxes

model <- glmer(serpentine.mine_prop~decimalLatitude+month+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=merged_noNAserpentine.mine, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

SMine_glm <- glm(serpentine.mine_prop~decimalLatitude*plant_genus_species+month+year, weights=totalboxes, family = binomial(logit), 
              na.action=na.omit, data=merged_noNAserpentine.mine)
summary(SMine_glm)

#Output average herbivory data for maps
#Overall averages
average_chew <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state"), summarize, mean_lrem = mean(leafremoved, na.rm=T))
average_skel <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state"), summarize, mean_skel = mean(skel, na.rm=T))
average_stipp <- ddply(merged, c("county","decimalLatitude", "decimalLongitude",  "state"), summarize, mean_stipp = mean(stippling, na.rm=T))
average_bmines <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state"), summarize, mean_bmine = mean(blotch.mine, na.rm=T))
average_serpmines <- ddply(merged, c("county","decimalLatitude","decimalLongitude", "state"), summarize, mean_smine = mean(serpentine.mine, na.rm=T))
average_SM <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state"), summarize, mean_SM = mean(sooty.mould, na.rm=T))

df <- join_all(list(average_chew, average_skel, average_stipp, average_bmines, 
                    average_serpmines, average_SM), by = c("county","state"), type = 'full')

write.csv(merged, file="output_countycentroids_herbivory.csv")
write.csv(df, file="output_countycentroids_herbivory_average.csv")

#By species
average_chew_species <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state", "plant_genus_species"), summarize, mean_lrem = mean(leafremoved, na.rm=T))
average_skel_species <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state", "plant_genus_species"), summarize, mean_skel = mean(skel, na.rm=T))
average_stipp_species <- ddply(merged, c("county","decimalLatitude", "decimalLongitude",  "state", "plant_genus_species"), summarize, mean_stipp = mean(stippling, na.rm=T))
average_bmines_species <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state", "plant_genus_species"), summarize, mean_bmine = mean(blotch.mine, na.rm=T))
average_serpmines_species <- ddply(merged, c("county","decimalLatitude","decimalLongitude", "state", "plant_genus_species"), summarize, mean_smine = mean(serpentine.mine, na.rm=T))
average_SM_species <- ddply(merged, c("county","decimalLatitude", "decimalLongitude", "state", "plant_genus_species"), summarize, mean_SM = mean(sooty.mould, na.rm=T))

#Separate dataframes by species
average_chew_DECA <- subset(average_chew_species, plant_genus_species == "desmodium_canadense")
average_skel_DECA <- subset(average_skel_species, plant_genus_species == "desmodium_canadense")
average_stipp_DECA <- subset(average_stipp_species, plant_genus_species == "desmodium_canadense")
average_bmines_DECA <- subset(average_bmines_species, plant_genus_species == "desmodium_canadense")
average_serpmines_DECA <- subset(average_serpmines_species, plant_genus_species == "desmodium_canadense")
average_SM_DECA <- subset(average_SM_species, plant_genus_species == "desmodium_canadense")

df_DECA <- join_all(list(average_chew_DECA, average_skel_DECA, average_stipp_DECA, average_bmines_DECA, 
                    average_serpmines_DECA, average_SM_DECA), by = c("county","state"), type = 'full')

average_chew_VAAN <- subset(average_chew_species, plant_genus_species == "vaccinium_angustifolium")
average_skel_VAAN <- subset(average_skel_species, plant_genus_species == "vaccinium_angustifolium")
average_stipp_VAAN <- subset(average_stipp_species, plant_genus_species == "vaccinium_angustifolium")
average_bmines_VAAN <- subset(average_bmines_species, plant_genus_species == "vaccinium_angustifolium")
average_serpmines_VAAN <- subset(average_serpmines_species, plant_genus_species == "vaccinium_angustifolium")
average_SM_VAAN <- subset(average_SM_species, plant_genus_species == "vaccinium_angustifolium")

df_VAAN <- join_all(list(average_chew_VAAN, average_skel_VAAN, average_stipp_VAAN, average_bmines_VAAN, 
                         average_serpmines_VAAN, average_SM_VAAN), by = c("county","state"), type = 'full')

average_chew_QUBI <- subset(average_chew_species, plant_genus_species == "quercus_bicolor")
average_skel_QUBI <- subset(average_skel_species, plant_genus_species == "quercus_bicolor")
average_stipp_QUBI <- subset(average_stipp_species, plant_genus_species == "quercus_bicolor")
average_bmines_QUBI <- subset(average_bmines_species, plant_genus_species == "quercus_bicolor")
average_serpmines_QUBI <- subset(average_serpmines_species, plant_genus_species == "quercus_bicolor")
average_SM_QUBI <- subset(average_SM_species, plant_genus_species == "quercus_bicolor")

df_QUBI <- join_all(list(average_chew_QUBI, average_skel_QUBI, average_stipp_QUBI, average_bmines_QUBI, 
                         average_serpmines_QUBI, average_SM_QUBI), by = c("county","state"), type = 'full')
df_QUBI <- subset(df_QUBI, mean_lrem !="NaN")

average_chew_CAOV <- subset(average_chew_species, plant_genus_species == "carya_ovata")
average_skel_CAOV <- subset(average_skel_species, plant_genus_species == "carya_ovata")
average_stipp_CAOV <- subset(average_stipp_species, plant_genus_species == "carya_ovata")
average_bmines_CAOV <- subset(average_bmines_species, plant_genus_species == "carya_ovata")
average_serpmines_CAOV <- subset(average_serpmines_species, plant_genus_species == "carya_ovata")
average_SM_CAOV <- subset(average_SM_species, plant_genus_species == "carya_ovata")

df_CAOV <- join_all(list(average_chew_CAOV, average_skel_CAOV, average_stipp_CAOV, average_bmines_CAOV, 
                         average_serpmines_CAOV, average_SM_CAOV), by = c("county","state"), type = 'full')

#Export dataframes by species
write.csv(df_DECA, file="output_herbiv_avgs_DECA.csv")
write.csv(df_VAAN, file="output_herbiv_avgs_VAAN.csv")
write.csv(df_QUBI, file="output_herbiv_avgs_QUBI.csv")
write.csv(df_CAOV, file="output_herbiv_avgs_CAOV.csv")

#Take a look at averages
average_chew_DECA <- subset(average_chew_species, plant_genus_species == "desmodium_canadense")
average_chew_VAAN <-  subset(average_chew_species, plant_genus_species == "vaccinium_angustifolium")
average_chew_QUBI <-  subset(average_chew_species, plant_genus_species == "quercus_bicolor")
average_chew_CAOV <-  subset(average_chew_species, plant_genus_species == "carya_ovata")

plot(average_chew_DECA$decimalLatitude, average_chew_DECA$mean_lrem)
plot(average_chew_VAAN$decimalLatitude, average_chew_VAAN$mean_lrem)
plot(average_chew_QUBI$decimalLatitude, average_chew_QUBI$mean_lrem)
plot(average_chew_CAOV$decimalLatitude, average_chew_CAOV$mean_lrem)




