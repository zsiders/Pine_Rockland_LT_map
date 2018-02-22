###-----------------------------------------------------
#		Pine Rockland Map
###-----------------------------------------------------
setwd("/Volumes/HDD/Users/Zach/Documents/AAA_Git_projects/Pine_Rockland_LT_map/")

list.files()

### read in locality data
loc <- read.csv("./Locality Data/parks_latlong_PR.csv")

#packages
library(rgdal)
library(rgeos)
library(raster)
library(OpenStreetMap)
library(GISTools)

#source functions
source("AddInsetMapZS.R")
source("map.scaleZS.R")

#Rockland layers
hist <- readOGR(dsn="./Map Layers/Rocks2/rocks2.shp",layer='rocks2',stringsAsFactors=FALSE)
curr <- readOGR(dsn="./Map Layers/Rocklands/Rocklands.shp",layer='Rocklands',stringsAsFactors=FALSE)
dade <- readOGR(dsn="./Map Layers/dade_pine_rockland_habitat_final/dade_pine_rockland_habitat_final.shp",layer='dade_pine_rockland_habitat_final',stringsAsFactors=FALSE)

#other layers
flc <- readOGR(dsn='./Map Layers/statecounties/STATECOUNTIES.shp',layer='STATECOUNTIES') #this reads in the florida counties as a SpatialPolygonsDataFrame

wtb <- readOGR(dsn='./Map Layers/mjwaterbnd/mjwaterbnd.shp',layer='mjwaterbnd',stringsAsFactors=FALSE)
wtb$NAME[is.na(wtb$NAME)] <- ""

nps <- readOGR(dsn = "./Map Layers/nps_boundary/nps_boundary.shp", layer="nps_boundary", stringsAsFactors = FALSE)

state <- readOGR(dsn = "./Map Layers/cb_2016_us_state_500k/cb_2016_us_state_500k.shp", layer="cb_2016_us_state_500k", stringsAsFactors = FALSE)
state_s <- state[state$NAME=="Florida",]

###-----------------------------------------------------
#		check coordinate reference systems
###-----------------------------------------------------
slot(hist,"proj4string")
slot(curr,"proj4string")
slot(dade,"proj4string") ### <<<<< BAD
slot(flc,"proj4string") ### <<<<< BAD
slot(wtb,"proj4string") ### <<<<< BAD
slot(nps,"proj4string") ### <<<<< BAD
slot(state_s,"proj4string") ### <<<<< BAD
###-----------------------------------------------------
#		fixing coordinate reference systems
###-----------------------------------------------------

#spTransform takes a SpatialPolygonsDF and projects it in a different coordinate reference system (CRS) which can be supplied by stripping the projection from another SpatialPolygonsDF using proj4string
dade_fx <- spTransform(dade, CRS(proj4string(hist)))
flc_fx <- spTransform(flc, CRS(proj4string(hist)))
wtb_fx <- spTransform(wtb, CRS(proj4string(hist)))
nps_s <- nps[nps$UNIT_NAME=="Everglades",]
nps_fx <- spTransform(nps_s, CRS(proj4string(hist)))
bis_fx <- spTransform(nps[nps$UNIT_NAME=="Biscayne",], CRS(proj4string(hist)))
nps_c <- gIntersection(nps_fx,flc_fx,byid=FALSE)
enp_pr <- gIntersection(hist, nps_c, byid=FALSE) #makes PR layer in ENP
state_sc <- spTransform(state_s, CRS(proj4string(hist)))

###-----------------------------------------------------
#		projecting the locality data
###-----------------------------------------------------
loc_na <- loc[!is.na(loc$lat) & !is.na(loc$long),] #drops NAs
#takes the coordinate data and makes it a SpatialPoints class -- must know CRS
loc_p <- SpatialPoints(coords=data.frame(x=loc_na$long,y=loc_na$lat) ,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
loc_fx <- spTransform(loc_p,CRS(proj4string(hist)))
###-----------------------------------------------------
#		build out map
###-----------------------------------------------------

##************
col2rgbA <- function(color,transparency)
{
  rgb(t(col2rgb(color))/255,alpha=transparency)
}


#-------
# Pulling in the satellite layer

# supply upper left corner and lower right corner in Lat/Long, can specify which type of map, type="bing" equals satellite imagery
# this needs to be bigger enough to crop but small enough b/c OpenStreetMap rescales the resolution as a function of map extent (coarser the bigger the map extent is)
sat <- openmap(c(24.75,-82), c(26.5,-79.75), type='bing')


satras <- raster(sat) #makes OpenStreetMap class to raster

#subsets the FL county object to just those surrounding Pine Rockland, took all adjacent counties
flc_s <- flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" | flc_fx$COUNTYNAME=="MONROE" | flc_fx$COUNTYNAME=="PALM BEACH" | flc_fx$COUNTYNAME=="HENDRY",]

#project the satellite raster to the same projection as counties
satras_p <- projectRaster(satras, crs=proj4string(flc_s))

satras_c <- crop(satras_p, extent(flc_s)) #crop the satellite raster to the counties
satras_m <- mask(satras_c, flc_s) #mask to only land (deletes ocean)

#crop again to a next extent, this time the plotting extent
satras_mc <- crop(satras_m, extent(c(bbox(hist)[1,],bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,]*c(0.9,1.02))))
#crop the county layers
flc_sc <- crop(flc_s, extent(c(bbox(hist)[1,],bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,]*c(0.9,1.02))))
#crop the ENP layer
nps_sc <- crop(nps_c, extent(c(bbox(hist)[1,],bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,]*c(0.9,1.02))))

### fonts

pdf(file="PR_draft_2.pdf", width=4, height=5.5)
# quartz("map",4,5.5)
	par(mar=c(1,1,1,1))

	### counties
	#gets a white border base and blue sea

	col_land <- 'white'
	col_ocean <- 'cadetblue3'
	col_glades <- 'darkseagreen'
	col_hist <- 'cornsilk2'
	col_curr <- 'dodgerblue1'
	col_samp <- 'coral1'


	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" | flc_fx$COUNTYNAME=="MONROE",],xlim=bbox(hist)[1,],ylim=c(bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,1]*1.1,bbox(hist)[2,2]),bg=col2rgbA(col_ocean,0.3),col=col_land,border=NA)
	plot(nps_c,add=TRUE,col="white",border=NA)
	#plots light green everglades
	plot(nps_c,add=TRUE,col=col2rgbA(col_glades,0.3),border="grey80")
	#overlays county borders
	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" |flc_fx$COUNTYNAME=="MONROE",],add=TRUE, border='grey50')
	text(886640,603000,"BROWARD",cex=0.8,col='grey40')
	text(886640,582000,"MIAMI-DADE",cex=0.8,col='grey40')
	text(737487.7,346137.6,"EVERGLADES",cex=0.8,col="grey40",adj=c(0.5,0.5))

	###pine rocklands
	plot(hist,col=col_hist,add=TRUE,border="grey70") #historical
	plot(curr,col=col_curr,add=TRUE,border=NA) #current
	plot(dade_fx,col=col_curr,add=TRUE,border=NA) #current

	#for plotting the star shaped symbols (needs a 10 column matrix)
	scale <- 2500 #sets size
	v <- rep(scale,10)
	v[seq(2,10,by=2)] <- scale*1/2 #sets size of between points
	symbols(x=coordinates(loc_fx)[,1],y=coordinates(loc_fx)[,2],stars=matrix(v,length(coordinates(loc_fx)[,1]),ncol=length(v)),inches=FALSE,add=TRUE,bg=col_samp,fg=NA)
	legend(707016,562265.7,legend=c("Historic","Current"),pch=c(22,22),pt.bg=c(col_hist,col_curr),col='grey20',pt.cex=2, ncol=2, bty='n')
	legend(707016,542265.7,legend=c("Sampling Locales"),pch=22,pt.bg=NA,col=NA,pt.cex=2,bty='n')
	scale2 <- 6000 #sets size
	v2 <- rep(scale2,10)
	v2[seq(2,10,by=2)] <- scale2*1/2
	symbols(x=717615.7,y=529287.2,stars=matrix(v2,1,ncol=length(v2)),inches=FALSE,add=TRUE,bg=col_samp,fg=NA)
dev.off()





pdf(file="PR_draft_AJB.pdf", width=3.6, height=5.5)
	par(mar=c(1,1,1,1))
	col_hist <- NA
	col_ocean <- 'cadetblue3'
	col_histo <- "chartreuse3"
	col_curr <- 'cyan'
	col_glades <- 'darkseagreen'

	plotRGB(satras_mc, colNA=col_ocean, bgalpha=floor(255*0.3))
	plot(flc_sc[flc_sc$COUNTYNAME=="BROWARD" | flc_sc$COUNTYNAME=="DADE" |flc_sc$COUNTYNAME=="MONROE",],add=TRUE, border='grey50', lwd=3)
	text(886640,603000,"BROWARD",cex=1,col='white', font=2)
	text(886640,582000,"MIAMI-DADE",cex=1,col='white', font=2)
	#plots light green everglades
	plot(nps_sc,add=TRUE,col=col2rgbA(col_glades,0.2),border="grey80")
	text(756487.7,346137.6,"EVERGLADES\nNATIONAL PARK",cex=1,col="white",adj=c(0.5,0.5), font=2)

	###pine rocklands
	plot(hist,col=col_hist,add=TRUE,border=col_histo, lwd=1) #historical
	plot(curr,col=col_curr,add=TRUE,border=NA) #current
	plot(dade,col=col_curr,add=TRUE,border=NA) #current
	plot(enp_pr, col=col_curr, add=TRUE, border=NA)

	
	legend(734824.5,577750.4,legend=c("Historic","Current"), lwd=c(2,NA), col=c(col_histo, NA), pt.bg=c(NA,col_curr), pt.cex=2, pch=c(NA,22), bty='n', text.col= 'white', text.font=2)
dev.off()

pdf(file="PR_draft_AJB_mod2.pdf", width=3.6, height=5.5)
	par(mar=c(1,1,1,1))
	col_hist <- NA
	col_ocean <- 'cadetblue3'
	col_histo <- "chartreuse3"
	col_curr <- 'cyan'
	col_glades <- 'darkseagreen'

	plotRGB(satras_mc, colNA=col_ocean, bgalpha=floor(255*0.3))
	plot(flc_sc[flc_sc$COUNTYNAME=="BROWARD" | flc_sc$COUNTYNAME=="DADE" |flc_sc$COUNTYNAME=="MONROE",],add=TRUE, border='grey50', lwd=3)
	text(886640,603000,"BROWARD",cex=0.8,col='white', font=2)
	text(886640,582000,"MIAMI-DADE",cex=0.8,col='white', font=2)
	#plots light green everglades
	plot(nps_sc,add=TRUE,col=col2rgbA(col_glades,0.2),border="grey80")
	text(756487.7,346137.6,"EVERGLADES\nNATIONAL PARK",cex=0.8,col="white",adj=c(0.5,0.5), font=2)
	text(895250.9,417946.9,"BISCAYNE BAY",cex=0.8,col="black",adj=c(0,0.5), font=2, srt=68)

	###pine rocklands
	plot(hist,col=col_hist,add=TRUE,border=col_histo, lwd=1) #historical
	plot(curr,col=col_curr,add=TRUE,border=NA) #current
	plot(dade,col=col_curr,add=TRUE,border=NA) #current
	plot(enp_pr, col=col_curr, add=TRUE, border=NA)

	legend(720836.6,496055.6,legend=c("Historic","Current"), lwd=c(2,NA), col=c(col_histo, NA), pt.bg=c(NA,col_curr), pt.cex=2, pch=c(NA,22), bty='n', text.col= 'white', text.font=2, cex=0.8)

	# GISTools::map.scale(895530.5, 283839.7, len=63006, units="Miles", ndivs=5, subdiv=2)
	map.scaleZS(895530.5, 283839.7, len=20, units="Kilometers", ndivs=5, subdiv=2, tx.cex=0.6)
	north.arrow(924029.9, 305243, len=5000, lab="N", cex.lab=0.9, col='black')
	AddInsetMapZS(state_sc, col=c('gray80',col2rgbA("red",0.3)), loc="topleft", inset=0.01, width=1, bg=c(col2rgbA("gray99",0.1), col2rgbA("gray95",0.7)))
dev.off()

pdf(file="PR_draft_split.pdf", width=7, height=5.5)
# quartz("map",4,5.5)
	par(mar=c(1,1,1,1),mfrow=c(1,2))

	### counties
	#gets a white border base and blue sea

	col_land <- 'white'
	col_ocean <- 'cadetblue3'
	col_glades <- 'darkseagreen'
	col_hist <- 'cornsilk2'
	col_curr <- 'dodgerblue1'
	col_samp <- 'coral1'


	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" | flc_fx$COUNTYNAME=="MONROE",],xlim=bbox(hist)[1,],ylim=c(bbox(flc_fx[flc_fx$COUNTYNAME=="DADE",])[2,1]*1.1,bbox(hist)[2,2]),bg=col2rgbA(col_ocean,0.3),col=col_land,border=NA)
	#plots white everglades (to erase borders of counties)
	# plot(wtb_fx[wtb_fx$NAME=="THE EVERGLADES",],add=TRUE,col="white",border=NA)
	# #plots light green everglades
	# plot(wtb_fx[wtb_fx$NAME=="THE EVERGLADES",],add=TRUE,col=col2rgbA("darkseagreen",0.3),border="grey80")
	plot(nps_c,add=TRUE,col="white",border=NA)
	#plots light green everglades
	plot(nps_c,add=TRUE,col=col2rgbA(col_glades,0.3),border="grey80")
	#overlays county borders
	plot(flc_fx[flc_fx$COUNTYNAME=="BROWARD" | flc_fx$COUNTYNAME=="DADE" |flc_fx$COUNTYNAME=="MONROE",],add=TRUE, border='grey50')
	text(886640,603000,"BROWARD",cex=0.8,col='grey40')
	text(886640,582000,"MIAMI-DADE",cex=0.8,col='grey40')
	text(742487.7,346137.6,"EVERGLADES",cex=0.8,col="grey40",adj=c(0.5,0.5))

	###pine rocklands
	plot(hist,col=col_hist,add=TRUE,border="grey70") #historical
	plot(curr,col=col_curr,add=TRUE,border=NA) #current
	plot(dade_fx,col=col_curr,add=TRUE,border=NA) #current

	#for plotting the star shaped symbols (needs a 10 column matrix)
	scale <- 2500 #sets size
	v <- rep(scale,10)
	v[seq(2,10,by=2)] <- scale*1/2 #sets size of between points
	symbols(x=coordinates(loc_fx)[,1],y=coordinates(loc_fx)[,2],stars=matrix(v,length(coordinates(loc_fx)[,1]),ncol=length(v)),inches=FALSE,add=TRUE,bg=col_samp,fg=NA)
	legend(707016,562265.7,legend=c("Historic","Current"),pch=c(22,22),pt.bg=c(col_hist,col_curr),col='grey20',pt.cex=2, ncol=2, bty='n')
	legend(707016,542265.7,legend=c("Sampling Locales"),pch=22,pt.bg=NA,col=NA,pt.cex=2,bty='n')
	scale2 <- 6000 #sets size
	v2 <- rep(scale2,10)
	v2[seq(2,10,by=2)] <- scale2*1/2
	symbols(x=717615.7,y=526287.2,stars=matrix(v2,1,ncol=length(v2)),inches=FALSE,add=TRUE,bg=col_samp,fg=NA)

	
	col_hist <- NA
	col_ocean <- 'cadetblue3'
	col_histo <- "chartreuse3"
	col_curr <- 'cyan'

	plotRGB(satras_mc, colNA=col_ocean, bgalpha=floor(255*0.3))
	plot(flc_sc[flc_sc$COUNTYNAME=="BROWARD" | flc_sc$COUNTYNAME=="DADE" |flc_sc$COUNTYNAME=="MONROE",],add=TRUE, border='grey50', lwd=3)
	text(886640,603000,"BROWARD",cex=1,col='white', font=2)
	text(886640,582000,"MIAMI-DADE",cex=1,col='white', font=2)
	#plots light green everglades
	plot(nps_sc,add=TRUE,col=col2rgbA(col_glades,0.2),border="grey80")
	text(756487.7,346137.6,"EVERGLADES\nNATIONAL PARK",cex=1,col="white",adj=c(0.5,0.5), font=2)

	###pine rocklands
	plot(hist,col=col_hist,add=TRUE,border=col_histo, lwd=1) #historical
	plot(curr,col=col_curr,add=TRUE,border=NA) #current
	plot(dade,col=col_curr,add=TRUE,border=NA) #current

	
	legend(734824.5,577750.4,legend=c("Historic","Current"), lwd=c(2,NA), col=c(col_histo, NA), pt.bg=c(NA,col_curr), pt.cex=2, pch=c(NA,22), bty='n', text.col= 'white', text.font=2)

dev.off()
















