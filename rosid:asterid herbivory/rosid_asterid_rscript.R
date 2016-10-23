library(ggplot2)
library(reshape2)

RA_dframe <- read.csv("rosid_asterid_data.csv")
names(RA_dframe)
attach(RA_dframe)

#melt into individual rows
RA_dframe_resh <- melt(RA_dframe, id.vars=c("plant_genus_species", "taxon", "MF", "abscission_num",
                                            "herbarium", "date", "month", "day", "year", "doy", 
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

#cells with chewing damage by taxon
bplot3 <- ggplot(RA_dframe, aes(x=taxon, y=ch_leafremoved, colour=taxon)) 
bplot3 <- bplot3 + geom_boxplot() + ylab("cells with chewing damage")
bplot3

#cells with chewing damage by state
#ch_leafremoved
bplot3 <- ggplot(RA_dframe, aes(x=state, y=ch_leafremoved, colour=state)) 
bplot3 <- bplot3 + geom_boxplot() + ylab("cells with chewing damage")
bplot3

#change in chewing damage over time and by species
facet1 <- ggplot(RA_dframe, aes(x=year, y=ch_leafremoved, colour=plant_genus_species)) + geom_point()
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


#Convert all data to binary, plots and do analyses



#Test for effect of year, doy
#model <- lme(sooty.mould~doy+year+state, random = ~1|plant_genus_species, na.action=na.omit, data=RA_dframe)
#summary(model)
