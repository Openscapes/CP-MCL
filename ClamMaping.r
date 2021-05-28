##Pismo Clam histograms and maps
library(maptools)
library(lubridate)
library(xlsx)
library(tidyverse)

#function to create legend labels
mylabs <- function(a) {
  leg0 <- a[1]
  leg1 <- paste(a[1], "-", a[3], sep = ' ')
  labels <- character(length(a)-3)
  labels <- ''
  for(i in 3:length(a)-1) {
    labels[i-2] <- paste(a[i],"-", a[i+1], sep = ' ') 
  }
  leglabel <- c(leg0, leg1, labels)  
}
#end function

usercomp <- "C:\\Users\\bruttenb\\Google Drive\\Project Files\\Pismo Clam\\Analysis" #Ben's computer

setwd(usercomp)


#import size and transect/section data

####New Import Allclams Pre-Alex and from Alex Thesis (May 2021)####

clamspre <- read.table('SizeAllClams_2014-2017_tomatch_AlexM_query.csv', header = T, sep = ',')
clamscurrent <- read.table('SizeAllClams_Alex_2018_thru2020-02-22.csv', header = T, sep = ',')

allclams <- rbind(clamspre, clamscurrent)
names(allclams) <- tolower(names(allclams))
allclams$datesampled <- as.Date(allclams$datesampled, "%m/%d/%Y")
names(allclams)[names(allclams) == 'datesampled'] <- 'date'


allclams$month <- month(allclams$date) %>% as.character
allclams$year <- year(allclams$date) %>% as.character

allclams <- allclams %>%
  mutate(yrmo = make_date(year, month))



pismo1 <- allclams[grep('ismo Beach', allclams$location),]
pismo2 <- allclams[grep('PB-', allclams$location),]
pismo <- rbind(pismo1, pismo2)

pismosection <- pismo %>% filter(size > 25 & size < 150) %>%
            group_by(transnumber, sectnumber, yrmo) %>%
            summarize(n = n())

pismotrans <- pismosection %>% group_by(transnumber, yrmo) %>%
            summarize(meantransect = mean(sectnumber))

pismodate <- pismotrans %>% group_by(yrmo) %>%
            summarise(mean = mean(meantransect), sd = sd(meantransect), n = n())

pismodate$se <- pismodate$sd/sqrt(pismodate$n)


ggplot(pismodate, aes(x=yrmo, y=mean)) + 
  geom_line() +
  geom_point(colour = "blue", size = 5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.4) +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        text = element_text(size=20)) 
  

boxpl <- pismo %>%
        filter(year>=2015)  %>%
        ggplot(aes(x=year, y=size)) +
      geom_boxplot() + 
  theme_bw() +
    theme(axis.line = element_line(size = 1, colour = "black"),
        text = element_text(size=20))+
  ylim(0, 100)


####End Import AllClams pre-Alex and through 2021 


####original size clams####

clamsize <- read.table('SizeAllClams.csv', header = T, sep = ',')
clamtransects <- read.table('AllClamTransects.csv', header = T, sep = ',')

#cleans up clamsize; lower case names, clean times and dates
names(clamsize) <- tolower(names(clamsize))
clamsize$datesampled <- as.Date(clamsize$datesampled, "%m/%d/%Y")
clamsize$timestart <- strptime(clamsize$timestart, '%m/%d/%Y %H:%M:%S')
clamsize$timeend <- strptime(clamsize$timeend, '%m/%d/%Y %H:%M:%S')
clamsize$transstarttime <- strptime(clamsize$transstarttime, '%m/%d/%Y %H:%M:%S')
clamsize$transendtime <- strptime(clamsize$transendtime, '%m/%d/%Y %H:%M:%S')
names(clamsize)[names(clamsize) == 'datesampled'] <- 'date'

#cleans up clamtransects
names(clamtransects) <- tolower(names(clamtransects))
clamtransects$datesampled <- as.Date(clamtransects$datesampled, "%m/%d/%Y")
clamtransects$timestart <- strptime(clamtransects$timestart, '%m/%d/%Y %H:%M:%S')
clamtransects$timeend <- strptime(clamtransects$timeend, '%m/%d/%Y %H:%M:%S')
clamtransects$transstarttime <- strptime(clamtransects$transstarttime, '%m/%d/%Y %H:%M:%S')
clamtransects$transendtime <- strptime(clamtransects$transendtime, '%m/%d/%Y %H:%M:%S')
names(clamtransects)[names(clamtransects) == 'datesampled'] <- 'date'


clam1 <- clamsize[clamsize$size > 6 & clamsize$size < 150,]
clam1 <- clamsize
#creates newreg variable, equal to region (except RIN for Rincon)
clam1$newreg <- ifelse(clam1$location == 'Rincon', 'RIN', as.character(clam1$region))

######RINCON SUBTIDAL data######
#brute force data entry for rincon subtidal data
rinsubsize <- c(120,120,110,120,115,115,101,115,102,108,103,120,110,120,122,120,123,112,113,111,108,120,121,113,111,118)

rinsubsize2015 <- c(70,100,100,75,100,80,100,100,100,100,100,100,100,100,100,100,55,100,100,100,110,100,100,110,100,110,110,110,110,100,100,100,100,90,100,100,110,100,100,110,100,100,130,100,100,130,110,110,100,100,100,100,100,110,110,110,110,100,100,100,110,100,110,100,110,110,110,110,110,110,110,110,100,110,100,110,130,90,100,110,100,110,100,55,110,100,100,100,100,120,100,110,50,100,100,120,110,50,130,70,95,115,65,106,106,116,95,100,90,100,105,110,90,120,110,120,110,115,103,119,118,102,112,111,119,120,90,100,120,100,120,100,100,98,98,102,94,98,110,108,128,100,110,120,42,103,101,115,100,90,92,100,120,140,100,90,110,48,110,103,102,100)

#creates dummy rincon subtidal data frame
rinsub <- as.data.frame(matrix(NA,length(rinsubsize),ncol(clam1)))
names(rinsub ) <- names(clam1)
rinsub$size <- rinsubsize

rinsub$location <- "subtidal"
rinsub$region <- 'SUB'

#joins clam1 data with rincon subtidal data
clam2 <- rbind(clam1, rinsub)

#plots Rincon data with intertidal and subtidal together
png('clam_size_adult_Rincon_inter-subtidal2015-2016.png', width = 5, height = 6, units = "in", res = 600, bg = "white")

par(mfrow = c(3,1), mar = c(3,3,3,3), oma = c(2,2,2,2))

with(clam1[!is.na(clam1$size) & clam1$size > 49.9 & clam1$location == 'Rincon',], hist(size, breaks = c(seq(40,110,5),114,seq(120,145,5)), col = 'blue', freq = T, main = 'Abundant SoCal Site', xlab = '' ))
abline(v=114, col = 'red')

hist(rinsubsize, breaks = c(seq(40,110,5),114,seq(120,145,5)), col = 'blue', freq = T, main = 'Abundant Site - Subtidal', xlab = '')
abline(v=114, col = 'red')

hist(rinsubsize2015[rinsubsize2015 > 49.9], breaks = c(seq(40,110,5),114,seq(120,145,5)), col = 'blue', freq = T, main = 'Abundant Site - Subtidal 2015', xlab = '')
abline(v=114, col = 'red')


dev.off()
#####end RINCON subtidal####

#####plot all data####
par(mfrow = c(1,1), mar = c(3,3,3,3), oma = c(2,2,2,2))

with(clam1[!is.na(clam1$size),], hist(size,breaks = c(seq(0,110,5),114,seq(120,140,5)), col = 'blue'))
abline(v=114)

#plot adults by region
png('clam_size_adult_2017a.png', width = 5, height = 7.5, units = "in", res = 600, bg = "white")

regions <- as.data.frame(c('NPC', 'SPC','RIN', 'BAJA', 'SUB'))
names(regions) <- c('regcode')
regions$regname <- c('North of Pt. Conception', 'South of Pt. Conception', 'Abundant SoCal Site', 'Baja', 'Abundant Site subtidal')


par(mfrow = c(4,1), mar = c(3,3,3,3), oma = c(2,2,2,2))

for(i in c('NPC', 'SPC','RIN', 'BAJA')) {

tempsize <- clam1[!is.na(clam1$size) & clam1$size > 49.9 & clam1$newreg == i,]

with(tempsize, hist(size, breaks = c(seq(45,110,5),114,seq(120,140,5)), col = 'blue', freq = F, main = regions$regname[regions$regcode == i], xlab = ''))#, ylim = c(0,180)))
abline(v=114)

mtext(with(tempsize, paste('count: ', length(size), sep = '')), cex = 0.8)

}
mtext('size (mm)',outer = T,side = 1)
mtext('frequency', outer = T, side = 2)


dev.off()

#end plot histograms

######plot Pismo histograms over time#####

#include only Pismo Beach sites
pismo <- clam1[grep('ismo', clam1$location),]
#check to makes sure locations are only pismo beach
unique(pismo$location)
#creates moyear variable
pismo$yearmo <- format(as.Date(pismo$date), "%Y-%m")
#creates season (to combine multiple months within a season)
pismoseason <- as.data.frame(sort(unique(pismo$yearmo)))
names(pismoseason) <- "yearmo"
pismoseason$season <- c('2014', '2014', 'Summer 2015', 'Winter 2016', 'Winter 2016','Spring 2016', 'Winter 2017', 'Winter 2017', 'Fall 2017', 'Winter 2018', 'Winter 2018', 'Winter 2018', 'Spring 2018', 'Spring 2018', 'Spring 2018')
#sets min and max values to track mean of juv pulse (excludes smaller and larger clams that arrived at different times)
pismoseason <- pismoseason[!pismoseason$season == '2014',]
pismoseason$min <- c(10, 18,18,20,10, 10 )
pismoseason$max <- c(35,45,45,47,47, 50)

#sets max size for Pismo
pismax <- 90
#merges pismo data with season
pismojuvs <- merge(pismo[pismo$size < pismax,], pismoseason, by = 'yearmo', all = T)

yearmotransnumber <- with(pismojuvs, aggregate(size~yearmo+transnumber, FUN = length))
yearmotrans <- with(yearmotransnumber, aggregate(transnumber~yearmo, FUN = length))
yearmotrans <- table(pismojuvs[yearmotrans$yearmo])

png('Pismo_beach_sizes_by_year2018_nolines.png', width = 5, height = 5, units = "in", res = 600, bg = "white")

par(mfcol = c(4,2), mar = c(2.5,2.5,1,0.5), oma = c(2,2,1,2))

for(j in unique(pismoseason$season)) { #sort(unique(pismo$year))) {

with(pismojuvs[pismojuvs$season == j,], hist(size, breaks = c(seq(0,pismax,5)), ylim = c(0, 360),col = 'blue', main = j, xlab = '', ylab = '', freq = T)) 
#abline(v = mean(pismojuvs$size[pismojuvs$season == j], na.rm = T), lwd = 2, col = 'gray50')

mtext(with(pismojuvs, paste0('mean: ',round(mean(size[season == j], na.rm = T),1))), cex = 0.8, line = -1) #DELETED: #'; count: ', length(size[season == j & size <= pismax])))
 
}
mtext('size (mm)', outer = T, side = 1)
mtext('frequency', outer = T, side = 2)

dev.off()

#Sums clams per transect per site per date over 50 mm
clampertrans <- with(clam1[clam1$size >= 50,], aggregate(size~region+location+date+transid, FUN = 'length'))
names(clampertrans)[names(clampertrans) == 'size'] <- 'count'
clampertrans$sitetransID <- with(clampertrans, paste(location, transid, sep = " "))
clampertrans1 <- clampertrans[,c(1:4,6,5)] 
clampertrans1 <- clampertrans1[order(clampertrans1$date),]

#adds in transects with zeros
sitetranssection <- with(clamtransects, aggregate(sectnumber~region+location+date+transid, FUN = length))
sitetranssection$sitetransID <- with(sitetranssection, paste(location, transid, sep = ' '))

#sitetrans <- with(sitetranssection, aggregate(transid~location+yearmo, FUN = length))
#names(sitetrans)[names(sitetrans) == 'transid'] <- 'counttrans'

#merges clampertrans1 and sitetransection
clampertransall <- merge(sitetranssection, clampertrans1, all = T)
#replaces NA with 0 in count
clampertransall$count[is.na(clampertransall$count)] <- 0
#calc clam per section
clampertransall$clampersect <- with(clampertransall, count/sectnumber)
#Aggregates clams per trans and per sect by site
siteavg <- with(clampertransall, aggregate(count~location, FUN = mean))
names(siteavg)[names(siteavg) == 'count'] <- 'avgpertrans'
siteavg$avgpersect <- with(clampertransall, aggregate(clampersect~location, FUN = mean))[,2]
siteavg$perm2 <- siteavg$avgpersect/(3*0.4)

#sitetranssection$yearmo <- with(sitetranssection, format(as.Date(date), "%Y-%m"))
#loads clamdensity datasheet; used only for GPS
clamdens <- read.table("ClamSiteDensity.csv", header = T, sep = ",")
names(clamdens) <- tolower(names(clamdens))
#merge siteavg with lat/lon from clamdens
clammap <- merge(siteavg, clamdens[c('location', 'lat','lon')], all = T)

clammap <- clammap[order(-clammap$avgpertrans),]


######Plot map of adult clams####
library(maptools)

worldshp <- readShapePoly('C:\\Users\\bruttenb\\Google Drive\\Project Files\\Pismo Clam\\carib5.shp') 

#change usercomp to Marine drive on Bens computer
usercomp <- "S:/bruttenb/Pismo/AlexMDatabase"
setwd(usercomp)

clammap <- read.csv('clammap.csv')

clammap <- clammap[order(-clammap$perm2),]

clamsubset <- clammap[clammap$perm2<2 &clammap$use == 'y',]
#subset to only state parks
#clamsubset <- clammap[clammap$sp18 == 'y',]
#clamdens <- read.table("ClamSiteDensity.csv", header = T, sep = ",")
#names(clamdens) <- tolower(names(clamdens))

cityname <- c("Monterey", "San Luis Obispo", "Los Angeles")
citylat <- c(36.5, 35.2742, 34.05)
citylon <- c(-121.85, -120.5, -118.25)
posvec <- c(4, 4, 4)


#maps
png('clam_map_NSA_CA.png', width = 5, height = 7.5, units = "in", res = 600, bg = "white")

#colors <- c('white', '#CCFFCC','#00CC00', '#FFFF00', '#FDA200', '#FA2E00', '#b70000')
#colors <- c('gray97', 'turquoise1', 'green', 'yellow1', 'orange1', 'red', 'red4', 'purple', 'purple4' )
#brks <- c(0, 0.001, 1, 2, 10 ,25, 50, 250, 300) #for number per transect
#brks <- c(0,0.0001,0.1,0.2,0.5,1,2,5,10)

#colors and breaks for 4 nonzero groups
colors <- c('gray97', 'turquoise1', 'green', 'yellow1', 'orange1', 'red')#, 'red4', 'purple', 'purple4' )
brks <- c(0, 0.0001, 0.1, 0.2, 0.5, 2) #for number per transect


sizes <- seq(1.5, 2 + (length(brks)-1)/5, .3)

resp <- clamsubset$perm2

#Incl Baja
#plot(worldshp, col = 'gray70', xlim = c(-122, -114), ylim = c(28.25,36.95), axes = F)
#CA only
plot(worldshp, col = 'gray70', xlim = c(-122, -117), ylim = c(32,36.95), axes = F)
#SLO/SB county only
#plot(worldshp, col = 'gray70', xlim = c(-122, -118), ylim = c(33,36), axes = F)
points(citylon, citylat, pch = 22, col = 'black', bg = 'gray25', cex = 1.4, lwd = 2, add = T)
text(citylon, citylat, labels=cityname, cex= 0.7, pos = posvec)
with(clamsubset, points(lon, lat, pch = 21, col = 'black', bg = colors[findInterval(resp, brks,all.inside=TRUE)], cex = sizes[findInterval(resp, brks,all.inside=TRUE)]))
legend(x = -122.2, y = 34, legend=mylabs(brks), fill=colors, bty="n",x.intersp = .75, y.intersp = .75, title = expression(paste('Adults m'^'-2')))
#title(main = 'Adult clam abundance')

dev.off()


#plot size
clamsize <- read.table("ClamSizeData.csv", header = T, sep = ",")

png('clam_size.png', width = 7, height = 10, units = "in", res = 600, bg = "white")

par(mfrow=c(3,1), oma = c(1,1,1,1), mar = c(4,4,2,2))

loc <- c('NPC', 'SPC', 'RIN')
loc.name <- c('North of Pt Conception', 'South of Pt Conception', 'Rincon')
location <- as.data.frame(cbind(loc, loc.name))

bins <- c(seq(4.3,max(clamsize$size)+ 10,10))
  # OLD BINS c(0, 20, 40, 60, 80, 100, 114.5, 140)
for(i in loc) {

hist(clamsize$size[clamsize$loc == i], breaks = bins, freq = T, cex.main = 1.95, cex.axis = 1.8, main = location$loc.name[location$loc == i], col = c(rep("gray75",11),c(rep("gray25",5))), xlim = c(0,max(clamsize$size)+10), xlab = "", ylab = "")
mtext('Frequency', side = 2, outer = T, line = -1.25, cex = 1.45)
mtext('Size class (mm)', side = 1, outer = T, line = -1, cex = 1.45)
}

dev.off()


