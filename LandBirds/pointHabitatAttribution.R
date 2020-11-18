# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file is used to attribute the survey locations with geospatial data on habitat type.

## Checking for install of necessary packages
libs<-c("XLConnect","plyr","rgdal","sp","rgeos","raster")
libcheck<-lapply(libs,function(pp){
			if(!pp %in% installed.packages()){
				install.packages(pp,repos="https://cloud.r-project.org/")
				return(paste(pp,"was installed"))
			}else{
				return(paste(pp,"already installed"))
			}
		})

## Load libraries
lapply(libs, require, character.only = TRUE)

## Need to source utility files
source("C:/Users/lsalas/git/sparklemotion/lsalas_rcode/AshMeadows/Marshbirds/AshMeadows_AnalysisUtils.R")

## Load survey point data
basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"
data<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Transect Points"))
names(data)<-c("transect","point","easting","northing")
dft<-data
coordinates(data)<-c("easting","northing")
proj4string(data) <- CRS("+proj=utm +zone=11 +north ellps=NAD27")

## Load vegetation data
veg<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/AHEM_Vegetation_BW_20100308/VegHabitat","Veg_HabitatTypes")
tproj<-projection(veg)
mdf <- spTransform(data, CRS(tproj))

#add 100m buffer
points<-data.frame()
for(rr in 1:nrow(mdf)){
	pnt<-gBuffer(mdf[rr,],width=100)
	suid<-paste(dft[rr,"transect"],dft[rr,"point"],sep="::")
	vdf<-intersect(pnt,veg)
	if(is.null(vdf)){
		adf<-data.frame(hab="Outside",area=0,count=0,SamplingUnitId=suid)	
	}else{
		vdf$area<-area(vdf)
		tt<-data.frame(hab=vdf$HAB_TYPE,area=area(vdf))
		adf<-aggregate(area~hab, data=tt, FUN=sum)
		acnt<-aggregate(area~hab, data=tt, FUN=NROW);names(acnt)<-c("hab","count")
		adf<-merge(adf,acnt,by="hab")
		adf$SamplingUnitId<-suid
	}
	
	points<-rbind(points,adf)
}

## Attribute and save
habpresence<-data.frame()
for(ss in unique(points$SamplingUnitId)){
	w<-subset(points,SamplingUnitId==ss)
	aa<-ifelse("Mesquite Bosque" %in% w$hab,1,0)
	bb<-ifelse("Riparian Woodland" %in% w$hab | "Ash" %in% w$hab,1,0)
	cc<-ifelse("Riparian Shrubland" %in% w$hab,1,0)
	abdf<-data.frame(SamplingUnitId=ss,MesquiteB=aa,Riparian=bb,Shrubland=cc)	
	habpresence<-rbind(habpresence,abdf)
}
sum(habpresence$MesquiteB==1)
sum(habpresence$Riparian==1)
sum(habpresence$Shrubland==1)
sum(habpresence$MesquiteB==1 & habpresence$Riparian==1)
sum(habpresence$MesquiteB==0 & habpresence$Riparian==0)

save(habpresence,file=paste(basepth,"/landbirdPoints_attributed_100.RData",sep=""))


#############################################
## DO NOT USE
#These do not work!
byArea<-aggregate(area~SamplingUnitId,data=points,FUN=max);byArea<-merge(byArea,points,by=c("area","SamplingUnitId"))

