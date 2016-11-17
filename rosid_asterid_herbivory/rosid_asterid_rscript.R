library(ggplot2)
library(reshape2)
library(plyr)

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
                                            "state", "phenology", "county"))
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

#Figure 1: Variation among species
#cells with chewing damage by taxon & species
#order dataframe
x <- subset(RA_dframe, ch_leafremoved != "NA")

italic.text <- element_text(face = "italic", color = "black")
bplot2 <- ggplot(x, aes(x=reorder(plant_genus_species,ch_leafremoved, mean), y=ch_leafremoved)) 
bplot2 <- bplot2 + geom_boxplot() + theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1)) + ylab("Grid cells with herbivory") +
  xlab("") + theme(legend.position= "none") + scale_x_discrete("", labels = c("baptisia_tinctoria"="Baptisia tinctoria",    "clethra_alnifolia"="Clethra alnifolia",     "cornus_racemosa"="Cornus racemosa",       "epigaea_repens"="Epigaea repens",
                                                                              "galium_triflorum"="Galium triflorum",      "gaylussacia_baccata"="Gaylussacia baccata",  "kalmia_angustifolia"="Kalmia angustifolia",   "lechea_intermedia"="Lechea intermedia",
                                                                              "lespedeza_capitata"="Lespedeza capitata",    "lespedeza_hirta"="Lespedeza hirta",       "ludwigia_palustris"="Ludwigia palustris",    "lycopus_americanus"="Lycopus americanus",   
                                                                              "lysimachia_terrestris"="Lysimachia terrestris", "mentha_arvensis"="Mentha arvensis",       "polygala_sanguinea"="Polygala sanguinea",    "triadenum_virginicum"="Triadenum virginicum",
                                                                              "vaccinium_macrocarpon"="Vaccinium macrocarpon", "viola_blanda"="Viola blanda", "viola_cucullata"="Viola cucullata",       "vitis_labrusca"="Vitis labrusca")) +
  theme(axis.text.x = italic.text)
bplot2




#Figure 2: Zero inflation
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
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
hist_cut <- ggplot(topfive_RA_dframe_resh, aes(x=value, fill=variable)) +xlab("Grid cells with herbivory") + ylab("Specimens")
hist_cut <- hist_cut + geom_bar(position="dodge") + theme_bw() + scale_fill_manual(values=cbPalette, 
                                                                        name="Damage Type",
                                                                        breaks=c("ch_leafremoved", "skel", "stippling", "leaf.galls", "sooty.mould", "leafmines_tot"),
                                                                        labels=c("Chewing (leaf area removed)", "Skeletonization", "Stippling", "Leaf Galls", "Sooty Mould", "Leaf mines"))
hist_cut



#Figure 3: Progression of herbivory annually

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
  
#Plots of chewing by doy by speciesc
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
RA_dframe_fig2_noNA <- subset(RA_dframe_fig2_noNA, month_cat != "March" & month_cat != "December")


cat <- ggplot(RA_dframe_fig2_noNA, aes(x=reorder(month_cat,doy), y=prop)) + xlab("") + ylab("Proportion of specimens")
cat <- cat + geom_bar(aes(fill = variable), position = "fill", stat="identity")
cat


#Maybe make the plot above for species with the highest incidence of herbivory? 


x=reorder(plant_genus_species,ch_leafremoved, mean)

#Test for effect of year, doy
model <- lme(ch_leafremoved~doy, random = ~1|plant_genus_species, na.action=na.omit, data=RA_dframe)
summary(model)
