library(ggplot2)
library(reshape2)
library(plyr)

RA_dframe <- read.csv("rosid_asterid_data.csv")
names(RA_dframe)
attach(RA_dframe)

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

#Figure 1: Zero inflation
#What are the most prevalent 5 types of herbivory? 
RA_dframe_resh$binary <- ifelse(RA_dframe_resh$value>0, 1, 0)
topfive <- ddply(RA_dframe_resh, ~variable, summarize, mean_specdamaged = mean(binary, na.rm=T))
topfive <- topfive[order(-topfive$mean_specdamaged),]
#topfive = chewing_tot, stippling, sooty.mould, leafmines_tot, leaf.galls

topfive_RA_dframe_resh <- subset(RA_dframe_resh, variable=="chewing_tot"|variable=="stippling"|variable=="sooty.mould"|variable=="leafmines_tot"|
                                   variable=="leaf.galls")

Fig1 <- ggplot(topfive_RA_dframe_resh, aes(value, colour=variable)) 
Fig1 <- Fig1 +  geom_freqpoly(binwidth = 1) +xlim(0,5) +theme_bw() +xlab("cells with herbivore damage") + ylab("specimens")
Fig1



#Figure 2: Progression of herbivory annually


#Continuous
Fig2_cont <- ggplot(topfive_RA_dframe_resh, aes(x=doy, y=value, colour=plant_genus_species)) 
Fig2_cont <- Fig2_cont +  geom_point() +ylim(0,5) +theme_bw() +xlab("day of year") + ylab("cells with herbivore damage") 
Fig2_cont

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
RA_dframe$binary_chew<- ifelse(RA_dframe$chewing_tot>0, 1, 0)
RA_dframe$binary_stipp <- ifelse(RA_dframe$stippling>0, 1, 0)
RA_dframe$binary_lmines <- ifelse(RA_dframe$leafmines_tot>0, 1, 0)
RA_dframe$binary_SM <- ifelse(RA_dframe$sooty.mould>0, 1, 0)
RA_dframe$binary_lgalls <- ifelse(RA_dframe$leaf.galls>0, 1, 0)

RA_dframe_resh2 <- melt(RA_dframe, id.vars=c("plant_genus_species", "taxon", "MF", "abscission_num",
                                            "herbarium", "date", "month", "month_cat", "day", "year", "doy", 
                                            "state", "phenology", "county"))

RA_dframe_resh2_fig2 <- subset(RA_dframe_resh2, variable== "binary_noherbiv"|variable=="binary_chew"|variable=="binary_stipp"|variable=="binary_lmines"|
                                 variable=="binary_SM"|variable=="binary_lgalls")

mar <- subset(RA_dframe_resh2_fig2, month_cat=="March")
apr <- subset(RA_dframe_resh2_fig2, month_cat=="April")
may <- subset(RA_dframe_resh2_fig2, month_cat=="May")
jun <- subset(RA_dframe_resh2_fig2, month_cat=="June")
jul <- subset(RA_dframe_resh2_fig2, month_cat=="July")
aug <- subset(RA_dframe_resh2_fig2, month_cat=="August")
sept <- subset(RA_dframe_resh2_fig2, month_cat=="September")
oct <- subset(RA_dframe_resh2_fig2, month_cat=="October")

#Work on this
prop <- function(x)  { for (i in 1:length(x)) {
  x$prop = x$value/nrow(x)
}
}

mar$prop <- mar$value/nrow(mar)
apr$prop <- apr$value/nrow(apr)
may$prop <- may$value/nrow(may)
jun$prop <- jun$value/nrow(jun)
jul$prop <- jul$value/nrow(jul)
aug$prop <- aug$value/nrow(aug)
sept$prop <- sept$value/nrow(sept)
oct$prop <- oct$value/nrow(oct)

props_summary <- rbind(mar, apr, may, jun, jul, aug, sept, oct)

#props_summary <- ddply(RA_dframe_resh2_fig2, c("month_cat", "variable"), summarize, sum_wtype = sum(value, na.rm=T))

props_summary <- subset(props_summary, month<10 & month>3)
Fig2_cat <- ggplot(props_summary, aes(x=month_cat, y=prop, fill=variable)) + geom_bar(stat="identity") +theme_bw() + ylab("cells with herbivore damage") 
Fig2_cat



#Test for effect of year, doy
model <- lme(ch_leafremoved~doy+year+state, random = ~1|plant_genus_species, na.action=na.omit, data=RA_dframe)
summary(model)
