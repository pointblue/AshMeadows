# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(XLConnect)
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(plyr)

source("C:/Users/lsalas/git/sparklemotion/lsalas_rcode/AshMeadows/Marshbirds/AshMeadows_AnalysisUtils.R")

basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"
data<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Transect Points"))
names(data)<-c("transect","point","easting","northing")
dft<-data
coordinates(data)<-c("easting","northing")
proj4string(data) <- CRS("+proj=utm +zone=11 +north ellps=NAD27")

veg<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/VegetationPolygons","VegetationPolygons_20090730")
tproj<-projection(veg)
mdf <- spTransform(data, CRS(tproj))

#add 100m buffer
points<-data.frame()
for(rr in 1:nrow(mdf)){
	pnt<-gBuffer(mdf[rr,],width=100)
	suid<-paste(dft[rr,"transect"],dft[rr,"point"],sep="::")
	vdf<-intersect(pnt,veg)
	if(is.null(vdf)){
		adf<-data.frame(alnz="Outside",area=0,count=0,SamplingUnitId=suid)	
	}else{
		vdf$area<-area(vdf)
		tt<-data.frame(alnz=vdf$ALLIANCE_C,area=area(vdf))
		adf<-aggregate(area~alnz, data=tt, FUN=sum)
		acnt<-aggregate(area~alnz, data=tt, FUN=NROW);names(acnt)<-c("alnz","count")
		adf<-merge(adf,acnt,by="alnz")
		adf$SamplingUnitId<-suid
	}
		
	points<-rbind(points,adf)
}

#These do not work!
byArea<-aggregate(area~SamplingUnitId,data=points,FUN=max);byArea<-merge(byArea,points,by=c("area","SamplingUnitId"))
byArea<-byArea[order(byArea$area,decreasing=TRUE),]
byCount<-aggregate(count~SamplingUnitId,data=points,FUN=max);byCount<-merge(byCount,points,by=c("count","SamplingUnitId"))
byCount<-byCount[order(byCount$count,decreasing=TRUE),]

q<-aggregate(area~alnz,data=points,FUN=sum)
q<-q[order(q$area,decreasing=TRUE),]
head(q,10)

#There are 80 points
NROW(unique(subset(points,alnz=="PRPU_SL" | alnz=="PRGL2_SL")$SamplingUnitId))
w<-unique(subset(points,alnz=="PRPU_SL" | alnz=="PRGL2_SL",select=c("alnz","SamplingUnitId")))

bdata<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Point Count Data"))
bdata$SamplingUnitId<-paste(bdata$TransectID,bdata$PointNumber,sep="::")
bveg<-merge(bdata,w,by="SamplingUnitId",all.x=T)
bveg<-subset(bveg,!is.na(alnz))

z<-aggregate(Number~Species,data=bveg,FUN=sum,na.rm=T)
z<-z[order(z$Number, decreasing=TRUE),]

