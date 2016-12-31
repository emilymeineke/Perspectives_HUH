library(ggplot2)
library(reshape2)
library(plyr)
library(AICcmodavg)
library(vegan)
library(RColorBrewer)
library(nlme)
library(lme4)
library(Rcpp)
library(aod)

RA_dframe <- read.csv("rosid_asterid_data.csv")
names(RA_dframe)
attach(RA_dframe)
RA_dframe$date <- as.Date(RA_dframe$date)

#split genus in species in case I need them separated
RA_dframe$plant_genus_species_split <- RA_dframe$plant_genus_species
bind <- data.frame(do.call('rbind', strsplit(as.character(RA_dframe$plant_genus_species_split),'_',fixed=TRUE)))
RA_dframe <- cbind(RA_dframe, bind)

RA_dframe$leafmines_tot <- RA_dframe$serpentine.mine+RA_dframe$blotch.mine
RA_dframe$chewing_tot <- RA_dframe$ch_leafremoved+RA_dframe$skel

#melt into individual rows
RA_dframe_resh <- melt(RA_dframe, id.vars=c("plant_genus_species", "taxon", "MF", "abscission_num",
                                            "herbarium", "date", "month", "month_cat", "day", "year", "doy", 
                                            "state", "phenology", "county", "plant_genus_species_split", "X1", "X2"))
names(RA_dframe_resh)

#Exploratory plots
#cells with each type of damage
bplot <- ggplot(RA_dframe_resh, aes(x=variable, y=value, colour=variable)) 
bplot <- bplot + geom_boxplot() + geom_point() + theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ylab("cells with damage") + geom_jitter() + theme(legend.position="none")
bplot

#cells with each type of damage by taxon
bplot1a <- ggplot(RA_dframe_resh, aes(x=variable, y=value, colour=taxon)) 
bplot1a <- bplot1a + geom_boxplot() + geom_point() + theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ylab("cells with damage") + geom_jitter()
bplot1a

#cells with each type of damage by state
bplot1b <- ggplot(RA_dframe_resh, aes(x=variable, y=value, colour=state)) 
bplot1b <- bplot1b + geom_boxplot() + geom_point() + theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ylab("cells with damage") + geom_jitter()
bplot1b


#focus on chewing, because it's the most prevalent damage type
#cells with chewing damage by taxon & species
bplot2 <- ggplot(RA_dframe, aes(x=plant_genus_species, y=ch_leafremoved, colour=taxon)) 
bplot2 <- bplot2 + geom_boxplot() + geom_jitter() + theme(axis.text.x = element_text(angle=60, hjust=1)) + ylab("cells with chewing damage")
bplot2

#cells with chewing damage over time
bplot3 <- ggplot(RA_dframe, aes(x=year, y=ch_leafremoved)) 
bplot3 <- bplot3 + geom_point() + ylab("cells with chewing damage") + geom_jitter()
bplot3

#cells with chewing damage by taxon
bplot3 <- ggplot(RA_dframe, aes(x=taxon, y=ch_leafremoved, colour=taxon)) 
bplot3 <- bplot3 + geom_boxplot() + ylab("cells with chewing damage")
bplot3

#cells with chewing damage by state
#ch_leafremoved
bplot3 <- ggplot(RA_dframe, aes(x=state, y=ch_leafremoved, colour=state)) 
bplot3 <- bplot3 + geom_boxplot() + ylab("cells with chewing damage")
bplot3

#change in chewing damage over time and by taxon
facet1 <- ggplot(RA_dframe, aes(x=year, y=ch_leafremoved, colour=taxon)) + geom_point()
facet1 <- facet1 + facet_grid(state ~ ., margins=TRUE)
facet1

#change in chewing damage over growing season
RA_dframe_rosids <- subset(RA_dframe, taxon == "rosid")
RA_dframe_asterids <- subset(RA_dframe, taxon == "asterid")

facet_a <- ggplot(RA_dframe_asterids, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
facet_a <- facet_a + facet_grid(plant_genus_species ~ ., margins=TRUE) + theme(legend.position= "none")
facet_a

facet_r <- ggplot(RA_dframe_rosids, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
facet_r <- facet_r + facet_grid(plant_genus_species ~ ., margins=TRUE) + theme(legend.position= "none")
facet_r

detach(RA_dframe)


#Making paper figures

#Variation among species
#cells with chewing damage by taxon & species
#order dataframe
x <- subset(RA_dframe, ch_leafremoved != "NA")

italic.text <- element_text(face = "italic", color = "black")
bplot2 <- ggplot(x, aes(x=reorder(plant_genus_species,ch_leafremoved, mean), y=ch_leafremoved)) 
bplot2 <- bplot2 + geom_boxplot() + ylab("Grid cells with herbivory") +
  xlab("") + scale_x_discrete("", labels = c("baptisia_tinctoria"="Baptisia tinctoria",    "clethra_alnifolia"="Clethra alnifolia",     "cornus_racemosa"="Cornus racemosa",       "epigaea_repens"="Epigaea repens",
                                                                              "galium_triflorum"="Galium triflorum",      "gaylussacia_baccata"="Gaylussacia baccata",  "kalmia_angustifolia"="Kalmia angustifolia",   "lechea_intermedia"="Lechea intermedia",
                                                                              "lespedeza_capitata"="Lespedeza capitata",    "lespedeza_hirta"="Lespedeza hirta",       "ludwigia_palustris"="Ludwigia palustris",    "lycopus_americanus"="Lycopus americanus",   
                                                                              "lysimachia_terrestris"="Lysimachia terrestris", "mentha_arvensis"="Mentha arvensis",       "polygala_sanguinea"="Polygala sanguinea",    "triadenum_virginicum"="Triadenum virginicum",
                                                                              "vaccinium_macrocarpon"="Vaccinium macrocarpon", "viola_blanda"="Viola blanda", "viola_cucullata"="Viola cucullata",       "vitis_labrusca"="Vitis labrusca")) +
  theme(axis.text.x = italic.text) + theme(legend.position= "none") +
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                     axis.title.y = element_text(size=12), 
                   panel.background = element_blank(), 
                   panel.grid.major = element_blank(),  #remove major-grid labels
                   panel.grid.minor = element_blank()) 
bplot2
#ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3a_var_btwn_spp.jpeg", dpi=300)
#ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3a_var_btwn_spp.tiff", dpi=300)



#Figure 3b: Zero inflation
#What are the most prevalent 5 types of herbivory? 
RA_dframe_resh$binary <- ifelse(RA_dframe_resh$value>0, 1, 0)
topfive <- ddply(RA_dframe_resh, ~variable, summarize, mean_specdamaged = mean(binary, na.rm=T))
topfive <- topfive[order(-topfive$mean_specdamaged),]
#topfive = chewing_tot, stippling, sooty.mould, leafmines_tot, leaf.galls

topfive_RA_dframe_resh <- subset(RA_dframe_resh, variable=="ch_leafremoved"|variable=="skel"|variable=="stippling"|variable=="sooty.mould"|variable=="leafmines_tot"|variable=="leaf.galls")

#Freqency poly
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
Fig2 <- ggplot(topfive_RA_dframe_resh, aes(value, colour=variable)) 
Fig2 <- Fig2 +  geom_freqpoly(binwidth = 1, size=1.3) +xlim(0,5) +theme_bw() +xlab("Grid cells with herbivory") + ylab("Specimens") + scale_colour_manual(values=cbPalette, 
                                                                                                                                                          name="Damage Type",
                                                                                                                                                          breaks=c("ch_leafremoved", "skel", "stippling", "leaf.galls", "sooty.mould", "leafmines_tot"),
                                                                                                                                                          labels=c("Chewing (leaf area removed)", "Skeletonization", "Stippling", "Leaf Galls", "Sooty Mould", "Leaf mines"))
Fig2

#Histogram
topfive_RA_dframe_resh <- subset(topfive_RA_dframe_resh, variable != "sooty.mould")
cbPalette <- c("ch_leafremoved"="#E69F00", "skel"="#56B4E9", "stippling"= "#0072B2", "leaf.galls" = "#F0E442", "leafmines_tot" = "#000000")
hist_cut <- ggplot(topfive_RA_dframe_resh, aes(x=value, fill=variable)) +xlab("Grid cells with herbivory") + ylab("Specimens")
hist_cut <- hist_cut + geom_bar(position="dodge") + scale_fill_manual(values=cbPalette, 
                                                                        name="Damage Type",
                                                                        breaks=c("ch_leafremoved", "skel", "stippling", "leaf.galls", "leafmines_tot"),
                                                                        labels=c("chewing (leaf area removed)", "skeletonization", "stippling", "leaf galls", "leaf mines")) +
  theme_bw() + theme(axis.title.x = element_text(size=12), 
                     axis.title.y = element_text(size=12), 
                     panel.background = element_blank(), 
                     panel.grid.major = element_blank(),  #remove major-grid labels
                     panel.grid.minor = element_blank(),
                     legend.title = element_text(size=12),
                     legend.key = element_blank()) 
hist_cut
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3c_zeroinflation.jpeg", dpi=300)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3c_zeroinflation.jpeg.tiff", dpi=300)



#Progression of herbivory annually

#Abundance of herbivory
#Continuous, garbage
Fig3_cont <- ggplot(topfive_RA_dframe_resh, aes(x=doy, y=value, colour=plant_genus_species)) 
Fig3_cont <- Fig3_cont +  geom_point() +ylim(0,5) +theme_bw() +xlab("day of year") + ylab("cells with herbivore damage") 
Fig3_cont

#Get incidence of chewing herbivory for all species
#Plot how chewing herbivory changes over season for all species
levels(RA_dframe$plant_genus_species)

BATI <- subset(RA_dframe, plant_genus_species == "baptisia_tinctoria")
CLAL <- subset(RA_dframe, plant_genus_species == "clethra_alnifolia")
CORA <- subset(RA_dframe, plant_genus_species == "cornus_racemosa")
EPRE <- subset(RA_dframe, plant_genus_species == "epigaea_repens")
GATR <- subset(RA_dframe, plant_genus_species == "galium_triflorum")
GABA <- subset(RA_dframe, plant_genus_species == "gaylussacia_baccata")
KAAN <- subset(RA_dframe, plant_genus_species == "kalmia_angustifolia")
LEIN <- subset(RA_dframe, plant_genus_species == "lechea_intermedia")
LECA <- subset(RA_dframe, plant_genus_species == "lespedeza_capitata")
LEHI <- subset(RA_dframe, plant_genus_species == "lespedeza_hirta")
LUPA <- subset(RA_dframe, plant_genus_species == "ludwigia_palustris")
LYAM <- subset(RA_dframe, plant_genus_species == "lycopus_americanus")
LYTE <- subset(RA_dframe, plant_genus_species == "lysimachia_terrestris")
MEAR <- subset(RA_dframe, plant_genus_species == "mentha_arvensis")
POSA <- subset(RA_dframe, plant_genus_species == "polygala_sanguinea")
TRVI <- subset(RA_dframe, plant_genus_species == "triadenum_virginicum")
VAMA <- subset(RA_dframe, plant_genus_species == "vaccinium_macrocarpon")
VIBL <- subset(RA_dframe, plant_genus_species == "viola_blanda")
VICU <- subset(RA_dframe, plant_genus_species == "viola_cucullata")
VILA <- subset(RA_dframe, plant_genus_species == "vitis_labrusca")
  
#Plots of chewing abundance by doy by species
plo <- ggplot(BATI, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(CLAL, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(CORA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(EPRE, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(GATR, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(GABA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(KAAN, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LEIN, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LECA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LEHI, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LUPA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LYAM, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(LYTE, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(MEAR, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(POSA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(TRVI, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(VAMA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(VIBL, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(VICU, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo

plo <- ggplot(VILA, aes(x=doy, y=ch_leafremoved, colour=plant_genus_species)) + geom_point() + geom_abline()
plo


#Identify species with most chewing damage
mean_damage_abund <- ddply(RA_dframe, ~plant_genus_species, summarize, mean_chewing = mean(ch_leafremoved, na.rm=T))
#Sort and choose top 5, see if this is a good method for displaying data
#Do this for other types of damage, too?
attach(mean_damage_abund)
mean_damage_abund_top <- mean_damage_abund[order(mean_chewing),]

#Cat
#Format data
RA_dframe$totalherbiv <- RA_dframe$chewing_tot+RA_dframe$stippling+RA_dframe$leafmines_tot+RA_dframe$sooty.mould+RA_dframe$leaf.galls
RA_dframe$binary_noherbiv <- ifelse(RA_dframe$totalherbiv>0, 1, 0)
RA_dframe$binary_chew<- ifelse(RA_dframe$ch_leafremoved>0, 1, 0)
RA_dframe$binary_skel <- ifelse(RA_dframe$skel>0, 1, 0)
RA_dframe$binary_stipp <- ifelse(RA_dframe$stippling>0, 1, 0)
RA_dframe$binary_lmines <- ifelse(RA_dframe$leafmines_tot>0, 1, 0)
RA_dframe$binary_SM <- ifelse(RA_dframe$sooty.mould>0, 1, 0)
RA_dframe$binary_lgalls <- ifelse(RA_dframe$leaf.galls>0, 1, 0)
RA_dframe$binary_blotchmines <- ifelse(RA_dframe$blotch.mine>0, 1, 0)
RA_dframe$binary_serpmines <- ifelse(RA_dframe$serpentine.mine>0, 1, 0)

RA_dframe_resh2 <- melt(RA_dframe, id.vars=c("plant_genus_species", "taxon", "MF", "abscission_num",
                                            "herbarium", "date", "month", "month_cat", "day", "year", "doy", 
                                            "state", "phenology", "county", "X1", "X2"))

RA_dframe_resh2_fig2 <- subset(RA_dframe_resh2, variable== "binary_noherbiv"|variable=="binary_chew"|variable=="binary_stipp"|variable=="binary_lmines"|
                                 variable=="binary_SM"|variable=="binary_lgalls"|variable=="binary_skel")

mar <- subset(RA_dframe_resh2_fig2, month_cat=="March")
apr <- subset(RA_dframe_resh2_fig2, month_cat=="April")
may <- subset(RA_dframe_resh2_fig2, month_cat=="May")
jun <- subset(RA_dframe_resh2_fig2, month_cat=="June")
jul <- subset(RA_dframe_resh2_fig2, month_cat=="July")
aug <- subset(RA_dframe_resh2_fig2, month_cat=="August")
sept <- subset(RA_dframe_resh2_fig2, month_cat=="September")
oct <- subset(RA_dframe_resh2_fig2, month_cat=="October")

#Plot
cat <- ggplot(RA_dframe_resh2_fig2, aes(value)) 
cat <- cat + geom_bar(aes(fill = variable), position = "fill")
cat

RA_dframe_resh2_fig2$value <- as.numeric(RA_dframe_resh2_fig2$value)
x <- nrow(RA_dframe_resh2_fig2)
RA_dframe_fig2_noNA <- subset(RA_dframe_resh2_fig2, value != "NA")
RA_dframe_fig2_noNA$prop <- RA_dframe_fig2_noNA$value/4908
RA_dframe_fig2_noNA <- subset(RA_dframe_fig2_noNA, month_cat != "March" & month_cat != "December" & variable != "binary_SM")

#Progression of herbivory throughout the season, potential figure
cbPalette <- c("binary_noherbiv"= "#009E73", "binary_chew"="#E69F00", "binary_skel"="#56B4E9", "binary_stipp"= "#0072B2", "binary_lgalls" = "#F0E442", "binary_lmines" = "#000000")
cat <- ggplot(RA_dframe_fig2_noNA, aes(x=reorder(month_cat,doy), y=prop))+ xlab("") + ylab("Proportion of specimens")
cat <- cat + geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values=cbPalette, 
                                                                                                    name="Damage Type",
                                                                                                    breaks=c("binary_chew", "binary_skel", "binary_stipp", "binary_lgalls", "binary_lmines", "binary_noherbiv"),
                                                                                                    labels=c("chewing (leaf area removed)", "skeletonization", "stippling", "leaf galls", "leaf mines", "no herbivory")) + 
  theme_bw() + theme(axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank()) +
  theme(legend.key = element_blank())
cat
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3d_seasonal_progression_herbiv.jpeg", dpi=300)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3d_seasonal_progression_herbiv.tiff", dpi=300)

#x=reorder(plant_genus_species,ch_leafremoved, mean)

#Overall test for effect of year, doy
RA_dframe_noNAchew <- subset(RA_dframe, ch_leafremoved != "NA")
RA_dframe_noNAchew$totalboxes <- 5
RA_dframe_noNAchew$chew_prop <- RA_dframe_noNAchew$ch_leafremoved/RA_dframe_noNAchew$totalboxes

model <- glmer(chew_prop~log(doy+1)+(1|plant_genus_species), weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=RA_dframe_noNAchew, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

model2 <- glm(chew_prop~plant_genus_species, weights=totalboxes, family = binomial(logit), 
                na.action=na.omit, data=RA_dframe_noNAchew)
summary(model2)
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 1:19)

binary_chew <- list()
binary_chew[[1]] <- glm(chew_prop~doy*plant_genus_species+year, weights=totalboxes, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe_noNAchew)
binary_chew[[2]] <- glm(chew_prop~doy*plant_genus_species, weights=totalboxes, family = binomial(logit), 
               na.action=na.omit, data=RA_dframe_noNAchew)
binary_chew[[3]] <- glm(chew_prop~doy, weights=totalboxes, family = binomial(logit), 
              na.action=na.omit, data=RA_dframe_noNAchew)
binary_chew[[4]] <- glm(chew_prop~plant_genus_species, weights=totalboxes, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe_noNAchew)
binary_chew[[5]] <- glm(chew_prop~1, weights=totalboxes, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe_noNAchew)

Modnames <- c("year_doy_species", "species_doy", "doy", "species", "null")

(aict <- aictab(cand.set = binary_chew, modnames=Modnames, sort=TRUE))
anova(binary_chew[[2]], binary_chew[[5]], test="LRT")

summary(binary_chew[[2]])

#Separate GLM models for each spp? to see how many are significant? 



#Figure 4: NMDS
#Just Viola and Lespedeza for paper figure
#Construct matrix 
RA_dframe_sub <- RA_dframe
attach(RA_dframe_sub)
RA_dframe_sub$Total <- ch_leafremoved+skel+stippling+blotch.mine+serpentine.mine+leafminer.oviholes+leaf.galls+leaf.roller
RA_dframe_sub <- subset(RA_dframe_sub, Total != "NA" & Total != 0)
RA_dframe_sub_justfour <- subset(RA_dframe_sub, plant_genus_species == "viola_blanda"|plant_genus_species == "viola_cucullata"|
                                   plant_genus_species == "lespedeza_hirta"|plant_genus_species == "lespedeza_capitata")
RA_dframe_sub_justfour$plant_genus_species <- factor(RA_dframe_sub_justfour$plant_genus_species)

#Get rid of outlier with 4 leaf galls
RA_dframe_sub_justfour <- subset(RA_dframe_sub_justfour, leaf.galls<4)

mat <- RA_dframe_sub_justfour[,15:25]
matID <- 1:nrow(mat)
mat <- cbind(mat, matID)
RA_dframe_sub_justfour <- cbind(matID, RA_dframe_sub_justfour)

mat <- mat[c("ch_leafremoved", "skel",    "stippling", "blotch.mine", "serpentine.mine", 
               "leafminer.oviholes", "leaf.galls")]
mat <- as.matrix(mat)


#Make map
map <- RA_dframe_sub_justfour[c("matID", "plant_genus_species", "taxon", "MF", "abscission_num", "herbarium",
                   "date", "month", "month_cat", "day", "year", "doy", "state", "phenology", "county" )]


#nmds
otu.bray <- vegdist(mat,method="bray")
braycurtis.mds <- metaMDS(otu.bray, k=2)

#Hypothesis testing
fad<-adonis(otu.bray~plant_genus_species, data=map, permutations=999)
fad
fmod<-with(map,betadisper(otu.bray,plant_genus_species))
anova(fmod)
TukeyHSD(fmod)

#base plot
with(map, levels(plant_genus_species))
stressplot(braycurtis.mds)
colvec <- c("black","darkgrey","red","orange")
plot(braycurtis.mds)
with(map, points(braycurtis.mds, display="sites", col=colvec[plant_genus_species], pch=21, bg=colvec[plant_genus_species]))
with(map, legend("topright",legend=levels(plant_genus_species),bty="n", col=colvec, pch=21, pt.bg=colvec))

#plot with hulls
braycurtis.mds_ord <- braycurtis.mds$points
braycurtis.mds_ord <- braycurtis.mds_ord[1:101,]
joined_ord_map <- merge(braycurtis.mds_ord, map, by= "row.names")
head(joined_ord_map)
rownames(joined_ord_map) <- joined_ord_map[,1]
joined_ord_map <- joined_ord_map[,-1]
names(joined_ord_map)

#Create data for convex hulls
data.scores <- as.data.frame(scores(braycurtis.mds_ord))
data.scores$site <- rownames(data.scores)
data.scores$grp <- joined_ord_map$plant_genus_species
grp.vc <- data.scores[data.scores$grp == "viola_cucullata", ][chull(data.scores[data.scores$grp == 
                                                                     "viola_cucullata", c("MDS1", "MDS2")]), ]  # hull values for grp A
grp.vb <- data.scores[data.scores$grp == "viola_blanda", ][chull(data.scores[data.scores$grp == 
                                                                      "viola_blanda", c("MDS1", "MDS2")]), ]  # hull values for grp B
grp.lh <- data.scores[data.scores$grp == "lespedeza_hirta", ][chull(data.scores[data.scores$grp == 
                                                                                  "lespedeza_hirta", c("MDS1", "MDS2")]), ]  # hull values for grp A
grp.lc <- data.scores[data.scores$grp == "lespedeza_capitata", ][chull(data.scores[data.scores$grp == 
                                                                                  "lespedeza_capitata", c("MDS1", "MDS2")]), ]  # hull values for grp A

hull.data <- rbind(grp.vc, grp.vb, grp.lh, grp.lc)  #combine grp.a and grp.b
hull.data

#Species vectors
vectors_species <- envfit(braycurtis.mds, map, perm=999, na.rm=TRUE)
#spp.scrs <- as.data.frame(scores(vectors_species, display = "vectors"))
#spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))

A <- as.list(vectors_species$vectors)
#creating the dataframe
pvals <-as.data.frame(A$pvals)
arrows <-as.data.frame(A$arrows*sqrt(A$r))
C <-cbind(arrows, pvals)
#subset
Cred_sp<-subset(C, pvals<0.05)
Cred_sp <- cbind(Cred_sp, Species = rownames(Cred_sp))
colvec <- c("black","darkgrey","red","orange")

ggplot() + scale_colour_manual(values=c("lespedeza_capitata" = "black", "lespedeza_hirta" = "darkgrey", "viola_blanda" = "red", "viola_cucullata"="orange"),
                    breaks=c("lespedeza_capitata", "lespedeza_hirta", "viola_blanda", "viola_cucullata"),
                    labels=c("Lespedeza capitata", "Lespedeza hirta", "Viola blanda", "Viola cucullata")) +
  geom_point(data=data.scores,aes(x=MDS1,y=MDS2, colour=grp),size=3) + # add the point markers
 # scale_colour_manual(values=c("lespedeza_capitata" = "black", "lespedeza_hirta" = "darkgrey", "viola_blanda" = "red", "viola_cucullata"="orange"), name="") +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.text = element_text(face = "italic")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size =10)) 
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3b_NMDS.jpeg", dpi=300)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3b_NMDS.tiff", dpi=300)



#All species
#Construct matrix 
RA_dframe_sub <- RA_dframe
attach(RA_dframe_sub)
RA_dframe_sub$Total <- ch_leafremoved+skel+stippling+blotch.mine+serpentine.mine+leafminer.oviholes+leaf.galls+leaf.roller
RA_dframe_sub <- subset(RA_dframe_sub, Total != "NA" & Total != 0)

#Get rid of outlier with 4 leaf galls
RA_dframe_sub <- subset(RA_dframe_sub, leaf.galls<4)

mat <- RA_dframe_sub[,15:25]
matID <- 1:nrow(mat)
mat <- cbind(mat, matID)
RA_dframe_sub <- cbind(matID, RA_dframe_sub)

mat <- mat[c("ch_leafremoved", "skel",    "stippling", "blotch.mine", "serpentine.mine", 
             "leafminer.oviholes", "leaf.galls")]
mat <- as.matrix(mat)


#Make map
map <- RA_dframe_sub[c("matID", "plant_genus_species", "taxon", "MF", "abscission_num", "herbarium",
                                "date", "month", "month_cat", "day", "year", "doy", "state", "phenology", "county" )]


#nmds
otu.bray <- vegdist(mat,method="bray")
braycurtis.mds <- metaMDS(otu.bray, k=2)

#base plot
with(map, levels(plant_genus_species))
stressplot(braycurtis.mds)
colvec <- c("black","lawngreen","navy","grey", "yellow", "darkblue","green","purple","orange",
           "red1", "darkgreen","skyblue", "darkturquoise", "brown","darkslategrey", "springgreen1","cadetblue4", 
           "chocolate4", "palegoldenrod", "violetred2")
plot(braycurtis.mds)
with(map, points(braycurtis.mds, display="sites", col=colvec[plant_genus_species], pch=21, bg=colvec[plant_genus_species]))
with(map, legend("topright",legend=levels(plant_genus_species),bty="n", col=colvec, pch=21, pt.bg=colvec))

#Hypothesis testing
fotu.bray <- vegdist(mat,method="bray")
fad<-adonis(fotu.bray~plant_genus_species, data=map, permutations=999)
fad
fmod<-with(map,betadisper(fotu.bray,plant_genus_species))
anova(fmod)
TukeyHSD(fmod)

#create joined map and nmds score dataframe
braycurtis.mds_ord <- braycurtis.mds$points
dim(braycurtis.mds_ord)
braycurtis.mds_ord <- braycurtis.mds_ord[1:527,]
joined_ord_map <- merge(braycurtis.mds_ord, map, by= "row.names")
rownames(joined_ord_map) <- joined_ord_map[,1]
joined_ord_map <- joined_ord_map[,-1]
names(joined_ord_map)

#Supplementary figure with all species 
n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
cbPalette = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

ggplot() + geom_point(data=joined_ord_map,aes(x=MDS1,y=MDS2, colour=plant_genus_species),size=3) +
  scale_colour_manual(values=cbPalette, 
                      name="",
                      breaks=c("baptisia_tinctoria", "clethra_alnifolia", "cornus_racemosa", "epigaea_repens",
                               "galium_triflorum", "gaylussacia_baccata", "kalmia_angustifolia", "lechea_intermedia",
                               "lespedeza_capitata", "lespedeza_hirta", "ludwigia_palustris", "lycopus_americanus",   
                               "lysimachia_terrestris", "mentha_arvensis", "polygala_sanguinea", "triadenum_virginicum",
                               "vaccinium_macrocarpon", "viola_blanda", "viola_cucullata", "vitis_labrusca"),
                      labels=c("Baptisia tinctoria", "Clethra alnifolia", "Cornus racemosa", "Epigaea repens", 
                               "Galium triflorum", "Gaylussacia baccata","Kalmia angustifolia", "Lechea intermedia", 
                               "Lespedeza capitata", "Lespedeza hirta",  "Ludwigia palustris", "Lycopus americanus",
                               "Lysimachia terrestris", "Mentha arvensis", "Polygala sanguinea", "Triadenum virginicum",
                               "Vaccinium macrocarpon", "Viola blanda", "Viola cucullata", "Vitis labrusca")) +
                        coord_equal() + theme_bw() + theme(axis.text.x = element_blank(),  # remove x-axis text
                                                           axis.text.y = element_blank(), # remove y-axis text
                                                           axis.ticks = element_blank(),  # remove axis ticks
                                                           axis.title.x = element_text(size=12), # remove x-axis labels
                                                           axis.title.y = element_text(size=12), # remove y-axis labels
                                                           panel.background = element_blank(), 
                                                           panel.grid.major = element_blank(),  #remove major-grid labels
                                                           panel.grid.minor = element_blank(),  #remove minor-grid labels
                                                           plot.background = element_blank(),
                                                           legend.text = element_text(face = "italic"),
                                                           legend.title = element_blank(),
                                                           legend.text = element_text(size =10))
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Supp_NMDS_allspecies.jpeg", dpi=300)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Supp_NMDS_allspecies.tiff", dpi=300)



#Export data for phylogenetic tree
braycurtis.mds_ord <- braycurtis.mds$points
dim(braycurtis.mds_ord)
braycurtis.mds_ord <- braycurtis.mds_ord[1:527,]
joined_ord_map <- merge(braycurtis.mds_ord, map, by= "row.names")
head(joined_ord_map)
rownames(joined_ord_map) <- joined_ord_map[,1]
joined_ord_map <- joined_ord_map[,-1]
names(joined_ord_map)
mean_ordscore <- ddply(joined_ord_map, ~plant_genus_species, summarize, mean_mds = mean(MDS1, na.rm=T))
write.csv(mean_ordscore, file="meanordscore.csv")


#Calculate total damage types per specimen
RA_dframe$total_damtypes <- RA_dframe$binary_chew + RA_dframe$binary_skel + binary_stipp + binary_lmines + binary_SM + binary_lgalls + binary_blotchmines + binary_serpmines 
RA_dframe$damdiv_prop <- RA_dframe$total_damtypes/8
RA_dframe$Total_types <- 8
mean_damage <- ddply(RA_dframe, ~plant_genus_species, summarize, mean_damtypes = mean(total_damtypes, na.rm=T))
mean_damage_abv1 <- subset(mean_damage, mean_damtypes >1)
write.csv(mean_damage, file="meandamagetypesperspecies.csv")

#RA_dframe_sub1 <- subset(RA_dframe, plant_genus_species == "epigaea_repens"|plant_genus_species=="lespedeza_hirta"|plant_genus_species=="lespedeza_capitata")
#ggplot(RA_dframe_sub1, aes(y=damdiv_prop, x=doy, colour=plant_genus_species)) + geom_point()

#Logistic regression with plant_genus_species as a fixed effect
binary_damdiv <- list()
binary_damdiv[[1]] <- glm(damdiv_prop~doy*plant_genus_species+year, weights=Total_types, family = binomial(logit), 
                          na.action=na.omit, data=RA_dframe)
binary_damdiv[[2]] <- glm(damdiv_prop~doy*plant_genus_species, weights=Total_types, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe)
binary_damdiv[[3]] <- glm(damdiv_prop~doy, weights=Total_types, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe)
binary_damdiv[[4]] <- glm(damdiv_prop~plant_genus_species, weights=Total_types, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe)
binary_damdiv[[5]] <- glm(damdiv_prop~1, weights=Total_types, family = binomial(logit), 
                        na.action=na.omit, data=RA_dframe)

Modnames <- c("year_doy_species", "species_doy", "doy", "species", "null")

(aict <- aictab(cand.set = binary_damdiv, modnames=Modnames, sort=TRUE))
anova(binary_damdiv[[2]], binary_damdiv[[4]], test="LRT")

summary(binary_damdiv[[2]])

#Linear models don't meet assumptions
plot(RA_dframe$year, RA_dframe$total_damtypes)
damtypes <- lm(total_damtypes~year*plant_genus_species+doy, na.action=na.omit, data=RA_dframe)
summary(damtypes)
plot(damtypes)

linear <- lme(total_damtypes~doy+year, data=RA_dframe, random=~1|plant_genus_species, na.action=na.omit)
summary(linear)
plot(linear)

#Final full model
plot(RA_dframe$plant_genus_species, log(RA_dframe$doy+1))
plot(RA_dframe$doy, RA_dframe$damdiv_prop)

model <- glmer(damdiv_prop~log(doy+1)+(1|plant_genus_species), weights=Total_types, family = binomial(logit), 
               na.action=na.omit, data=RA_dframe, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)
anova(model)

#Models for indiv. species
#What species have the most diverse damage? Can we just plot the progression of those throughout the year? 
 

#Comparison w Turcotte paper
#Get averages for comparing w Turcotte paper
RA_dframe_JulthruAug <- subset(RA_dframe, month_cat== "July"|month_cat=="August"|month_cat=="September")
mean_dam_July_Aug_Sept <- ddply(RA_dframe_JulthruAug, ~plant_genus_species, summarize, mean_chewing = mean(ch_leafremoved, na.rm=T))

#Input data
herb_corr <- read.csv("Herb_correlation.csv", header=TRUE)
names(herb_corr)

lm_hcrr <- lm(Herb_JulAugSept~Annherbiv_Turcotte, data=herb_corr)
summary(lm_hcrr)


#Fig 3d
turc <- ggplot(herb_corr, aes(x=Annherbiv_Turcotte, y=Herb_JulAugSept)) + geom_point(size=3) + geom_smooth(method="lm", se=FALSE, colour="black") +
  theme_bw() + xlab("Annual herbivory rate (%)") + ylab("Grid cells with chewing herbivory") + theme(axis.title.x = element_text(size=12), # remove x-axis labels
                     axis.title.y = element_text(size=14),
                     axis.title.x = element_text(size=14),
                     axis.text.x = element_text(size=12),
                     axis.text.y = element_text(size=12),
                     panel.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     legend.text = element_text(face = "italic"),
                     legend.key = element_blank()) 
turc
#+ xlim(1,9) + ylim(0,2.5)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3d_turcotte.jpeg", height=6, dpi=300)
ggsave(file="/Users/emilymeineke/Documents/Perspectives_HUH/rosid_asterid_herbivory/Herbivory_POCfigs/Fig3d_turcotte.tiff", height=6, dpi=300)


