library(ape)
library(picante)

#set to correct directory
#setwd("C:/Jonathan/Dropbox/Herbivory in herbaria/Data")

#I just scraped this from your GitHub repository
data<-read.csv("rosidasterid_JD.csv")

species<-as.vector(unique(data$plant_genus_species))




#read in zanne phylo
zanne<- read.tree("Vascular_Plants_rooted.dated.tre")
#convert tip names to all lower case for string matching
zanne$tip.label<-tolower(zanne$tip.label)

#rename tips to match dataframe (switching species within genera)
zanne$tip.label[zanne$tip.label == "lycopus_uniflorus"]<- "lycopus_americanus"
zanne$tip.label[zanne$tip.label == "lysimachia_quadrifolia"]<- "lysimachia_terrestris"
zanne$tip.label[zanne$tip.label == "triadenum_fraseri"]<- "triadenum_virginicum"
# substituted for another genus within Cistaceae
#http://www.rjb.csic.es/jardinbotanico/ficheros/documentos/pdf/pubinv/PVG/GuzmanVargas_009ODECistaceae.pdf
zanne$tip.label[zanne$tip.label == "helianthemum_syriacum"]<- "lechea_intermedia"
# approximate placement of missing viola
#https://treebase.org/treebase-web/search/study/trees.html?id=1629
zanne$tip.label[zanne$tip.label == "viola_tricolor"]<- "viola_cucullata"


#subset tree to species
tree<-drop.tip(zanne, zanne$tip.label[!zanne$tip.label %in% species])



plot(tree)

#which species did we lose?
species[! species %in% tree$tip.label] 

#[1] "lycopus_americanus"    "lysimachia_terrestris" "lechea_intermedia"     "triadenum_virginicum" 
#[5] "viola_cucullata"  

#########################################
#check we need these species, then check for typos in names, 
#then look to see if we can use a phylogenetic 'placeholder'
#i.e. another species in the same genus (or family) that IS
#included in the Zanne et al. tree. For example, we could use
#Lycopus_uniflorus as a placeholder for Lycopus_americanus because
#this is the only species we sample within this genus we can use
#any species within the genus.

#For example:
#zanne$tip.label[zanne$tip.label == "lycopus_uniflorus"]<- "lycopus_americanus"

#The Viola will be more problematic - but viola_cucullata appears to be a synonym
#of Viola obliqua ...
#########################################


#########################################
#this is where I generate  summary dataframe for herbivory damage
#any damage:
damage <-NULL
for (x in 1:length(data[,1])){
damage[x]<-sum(data[x,c(14:26)], na.rm = T)
}
my.data<-cbind(data, damage)

#summarise herbivory damage by species
sp.herbivory<-aggregate(damage~plant_genus_species, data = my.data, FUN = "mean")
sp.chewing<-aggregate(ch_leafremoved~plant_genus_species, data = my.data, FUN = "mean")
sp.bmines<-aggregate(blotch.mine~plant_genus_species, data = my.data, FUN = "mean")
sp.serpmines<-aggregate(serpentine.mine~plant_genus_species, data = my.data, FUN = "mean")
sp.skel<-aggregate(skel~plant_genus_species, data = my.data, FUN = "mean")
sp.lgalls<-aggregate(leaf.galls~plant_genus_species, data = my.data, FUN = "mean")
sp.SM<-aggregate(sooty.mould~plant_genus_species, data = my.data, FUN = "mean")
row.names(sp.herbivory)<-sp.herbivory[,1]
row.names(sp.chewing)<-sp.chewing[,1]
row.names(sp.bmines)<-sp.bmines[,1]
row.names(sp.serpmines)<-sp.serpmines[,1]
row.names(sp.skel)<-sp.skel[,1]
row.names(sp.lgalls)<-sp.lgalls[,1]
row.names(sp.SM)<-sp.SM[,1]

#Overall herbivory- Jonathan's test
herbivory.dat<-sp.herbivory[,2]
names(herbivory.dat)<-row.names(sp.herbivory)

phy.signal.data<-match.phylo.data(tree, herbivory.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.2219002        0.0746682             0.0972578          0.452     -0.3963395


##suggest no signifiicant phylo signal in herbivory

#and just for fun, let's map herbivory on to the phylogeny:
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Chewing damage
chewing.dat<-sp.chewing[,2]
names(chewing.dat)<-row.names(sp.chewing)

phy.signal.data<-match.phylo.data(tree, chewing.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

# K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
# 1 0.3965634       0.01561714            0.03275709          0.119       -1.14018


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)




#Blotch mines
bmines.dat<-sp.bmines[,2]
names(bmines.dat)<-row.names(sp.bmines)

phy.signal.data<-match.phylo.data(tree, bmines.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#   K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
# 1 0.5119857     0.0005162566           0.001318864          0.268     -0.8899112


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Serpentine mines
serpmines.dat<-sp.serpmines[,2]
names(serpmines.dat)<-row.names(sp.serpmines)

phy.signal.data<-match.phylo.data(tree, serpmines.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 1.07898     1.122842e-06          6.128488e-06         0.2135     -0.8276614


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Skeletonization
skel.dat<-sp.skel[,2]
names(skel.dat)<-row.names(sp.skel)

phy.signal.data<-match.phylo.data(tree, skel.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.2219002        0.0746682             0.0972578          0.452     -0.3963395


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Leaf galls
lgalls.dat<-sp.lgalls[,2]
names(lgalls.dat)<-row.names(sp.lgalls)

phy.signal.data<-match.phylo.data(tree, lgalls.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.06808228     0.0006281368          0.0002252308         0.9965       2.471692


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Sooty mould 
SM.dat<-sp.SM[,2]
names(SM.dat)<-row.names(sp.SM)

phy.signal.data<-match.phylo.data(tree, SM.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#           K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.2014183      0.001167965           0.001213554          0.487    -0.05650842


##

#map
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)


#Damage diversity
sp.div <- read.csv("meandamagetypesperspecies.csv", header=TRUE)
sp.div <- sp.div[,-1]
row.names(sp.div)<-sp.div[,1]
div.dat<-sp.div[,2]
names(div.dat)<-row.names(sp.div)

phy.signal.data<-match.phylo.data(tree, div.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.1454586       0.01213566           0.009018029          0.767      0.6477327


##suggest no signifiicant phylo signal in damage type diversity

#and just for fun, let's map herbivory on to the phylogeny:
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)



#Damage composition
sp.ord <- read.csv("meanordscore.csv", header=TRUE)
head(sp.ord)

sp.ord <- sp.ord[,-1]
row.names(sp.ord)<-sp.ord[,1]
ord.dat<-sp.ord[,2]
names(ord.dat)<-row.names(sp.ord)

phy.signal.data<-match.phylo.data(tree, ord.dat)

#test for phylo signal
library(picante)
phylosignal(phy.signal.data$data, phy.signal.data$phy, reps = 999)

#          K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
#1 0.3667384     0.0001648413          0.0003166893          0.139      -1.060697


##suggest no signifiicant phylo signal

#and just for fun, let's map herbivory on to the phylogeny:
library(phytools)
contMap(phy.signal.data$phy, phy.signal.data$data)
