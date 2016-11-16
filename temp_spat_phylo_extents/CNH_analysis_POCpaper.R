# load occurrences #
ne <- read.csv("NEC_list.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#Data clean up
#get rid of columns I don't need
ne <- ne[,c(2:20, 29:33, 53:60, 83, 84)]

# Filter occurrences #
ne <- ne[ne$month!= 0 & ne$year!= 0 & ne$day!= 0,] # filter specimens without complete colection dates
ne <- ne[ne$year > 1700 & ne$year < 2017,] # filter specimens with incorrect dates
ne <- ne[!is.na(ne$specificEpithet) & !is.na(ne$genus),] #filter specimens without names

# Temporal extent

# Spatial extent
