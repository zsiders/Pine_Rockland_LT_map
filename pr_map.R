###-----------------------------------------------------
#		Pine Rockland Map
###-----------------------------------------------------
setwd("/Volumes/HDD/Users/Zach/Documents/AAA_Git_projects/Pine_Rockland_LT_map/")

list.files()

### read in locality data
loc <- read.csv(paste0(getwd(),"/Locality Data/parks_latlong_PR.csv"))

#packages
library(rgdal)

#Rockland layers
hist <- readOGR(dsn=paste0(getwd(),"/Map Layers/Rocks2/rocks2.shp"),layer='rocks2',stringsAsFactors=FALSE)
curr <- readOGR(dsn=paste0(getwd(),"/Map Layers/Rocklands/Rocklands.shp"),layer='Rocklands',stringsAsFactors=FALSE)
dade <- readOGR(dsn=paste0(getwd(),"/Map Layers/dade_pine_rockland_habitat_final/dade_pine_rockland_habitat_final.shp"),layer='dade_pine_rockland_habitat_final',stringsAsFactors=FALSE)

#other layers
flc <- readOGR(dsn='/Volumes/HDD/Users/Zach/Downloads/statecounties/STATECOUNTIES.shp',layer='STATECOUNTIES') #this reads in the florida counties as a SpatialPolygonsDataFrame

wtb <- readOGR(dsn='/Volumes/HDD/Users/Zach/Downloads/mjwaterbnd/mjwaterbnd.shp',layer='mjwaterbnd',stringsAsFactors=FALSE)
wtb$NAME[is.na(wtb$NAME)] <- ""

###-----------------------------------------------------
#		check coordinate reference systems
###-----------------------------------------------------
slot(hist,"proj4string")
slot(curr,"proj4string")
slot(dade,"proj4string") ### <<<<< BAD
slot(flc,"proj4string") ### <<<<< BAD
slot(wtb,"proj4string") ### <<<<< BAD

###-----------------------------------------------------
#		fixing coordinate reference systems
###-----------------------------------------------------

dade_fx <- spTransform(dade, CRS(proj4string(hist)))
flc_fx <- spTransform(flc, CRS(proj4string(hist)))
wtb_fx <- spTransform(wtb, CRS(proj4string(hist)))

###-----------------------------------------------------
#		projecting the locality data
###-----------------------------------------------------
loc_na <- loc[!is.na(loc$lat) & !is.na(loc$long),]
loc_p <- SpatialPoints(coords=data.frame(x=loc_na$long,y=loc_na$lat) ,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
loc_fx <- spTransform(loc_p,CRS(proj4string(hist)))
###-----------------------------------------------------
#		build out map
###-----------------------------------------------------

source("/Volumes/HDD/Users/Zach/Dropbox/Rcode/col2rgbA.r")

### fonts

pdf(file="PR_draft_1.pdf", width=4, height=5.5)
# quartz("map",4,5.5)
	par(mar=c(1,1,1,1))

	### counties
	#gets a white border base and blue sea
	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" | flc_fx$COUNTYNAME=="MONROE",],xlim=bbox(hist)[1,],ylim=c(bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,1]*1.1,bbox(hist)[2,2]),bg=col2rgbA('cadetblue3',0.3),col='white',border=NA)
	#plots white everglades (to erase borders of counties)
	plot(wtb_fx[wtb_fx$NAME=="THE EVERGLADES",],add=TRUE,col="white",border=NA)
	#plots light green everglades
	plot(wtb_fx[wtb_fx$NAME=="THE EVERGLADES",],add=TRUE,col=col2rgbA("darkseagreen",0.3),border="grey80")
	#overlays county borders
	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" |flc_fx$COUNTYNAME=="MONROE",],add=TRUE, border='grey50')
	text(886640,603000,"BROWARD",cex=0.8,col='grey40')
	text(886640,582000,"MIAMI-DADE",cex=0.8,col='grey40')
	text(788421.1,347726.4,"EVERGLADES",cex=0.8,col="grey40",adj=c(0.5,0.5))

	###pine rocklands
	plot(hist,col='cornsilk2',add=TRUE,border="grey70") #historical
	plot(curr,col='dodgerblue1',add=TRUE,border=NA) #current
	plot(dade_fx,col='dodgerblue1',add=TRUE,border=NA) #current

	#for plotting the star shaped symbols (needs a 10 column matrix)
	scale <- 2500 #sets size
	v <- rep(scale,10)
	v[seq(2,10,by=2)] <- scale*1/2 #sets size of between points
	symbols(x=coordinates(loc_fx)[,1],y=coordinates(loc_fx)[,2],stars=matrix(v,length(coordinates(loc_fx)[,1]),ncol=length(v)),inches=FALSE,add=TRUE,bg='coral1',fg=NA)
	legend(707016,562265.7,legend=c("Historic","Current"),pch=c(22,22),pt.bg=c('cornsilk2','dodgerblue1'),col='grey20',pt.cex=2, ncol=2, bty='n')
	legend(707016,542265.7,legend=c("Sampling Locales"),pch=22,pt.bg=NA,col=NA,pt.cex=2,bty='n')
	scale2 <- 7000 #sets size
	v2 <- rep(scale2,10)
	v2[seq(2,10,by=2)] <- scale2*1/2
	symbols(x=717615.7,y=529287.2,stars=matrix(v2,1,ncol=length(v2)),inches=FALSE,add=TRUE,bg='coral1',fg=NA)
dev.off()


















