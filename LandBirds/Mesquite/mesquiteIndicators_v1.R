# TODO: Add comment
# 
# Author: lsalas
###############################################################################


##Processing GBBO data for visualizations
library(XLConnect)
library(plyr)
library(unmarked)
library(ggplot2)

basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"
data<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Point Count Data"))
#habitat data
load(file=paste(basepth,"/landbirdPoints_attributed_100.RData",sep=""))

if(inherits(data,"try-error"))stop("Could not read the data. Please check the path, name of file and name of sheet")
data$Species<-gsub("Blue-Gray Gnatcatcher","Blue-gray Gnatcatcher",data$Species)
data$TransectID<-toupper(data$TransectID)
data$suid<-paste(data$TransectID,data$PointNumber,sep="::")
data$year<-as.integer(format(data$SurveyDate,"%Y"))

effort<-unique(data[,c("suid","TransectID","PointNumber","SurveyDate","year")])

generateVisit<-function(df){
	vdata<-data.frame()
	for(yy in unique(df$year)){
		ydf<-subset(df,year==yy)
		for(ss in unique(ydf$suid)){
			sdf<-subset(ydf,suid==ss)
			sdf<-sdf[order(sdf$SurveyDate),]
			sdf$visit<-c(1:(nrow(sdf)))
			vdata<-rbind(vdata,sdf)
		}
	}
	return(vdata)
}

effort<-generateVisit(df=effort)

#131 locations, 540 point-years, 450 with 1 visit, 90 with 2 visits
data<-merge(data,effort, by=c("suid","TransectID","PointNumber","SurveyDate","year"),all.x=T)

#Shuf suggests dropping YBCH and YWAR
indicators<-c("Bell's Vireo","Blue-gray Gnatcatcher","Phainopepla","Yellow Warbler","Lucy's Warbler","Verdin",
		"Blue Grosbeak","Yellow-breasted Chat","Ash-throated Flycatcher","Crissal Thrasher")

#need to sum within visit, then get the max
vdata<-aggregate(as.formula("Number~Species+suid+year+visit"),data=data,FUN=sum,na.rm=T)
obs<-aggregate(as.formula("Number~Species+suid+year"),data=vdata,FUN=max,na.rm=T)


obsdata<-ldply(.data=indicators,.fun=function(x,obs,effort){
			obsi<-subset(obs, Species==x);
			oed<-merge(effort,obsi,by=c("suid","year"),all.x=T);
			oed$Number<-ifelse(is.na(oed$Number),0,oed$Number);
			oed$Species<-ifelse(is.na(oed$Species),x,oed$Species);
			return(oed)
		},obs=obs,effort=effort)

#select only points with Mesquite habitat
obsdata<-merge(obsdata,habpresence,by.x="suid",by.y="SamplingUnitId",all.x=T)
obsdata<-subset(obsdata,MesquiteB==1)

#calculate annual abundance for each species - corrected by effort, and inter-annual variance
obsyrdata<-aggregate(Number~Species+year,obsdata,sum)
obsyrup<-unique(obsdata[,c("suid","year")])
obsyrvp<-aggregate(suid~year,obsyrup,NROW)
yeardata<-merge(obsyrdata,obsyrvp,by="year")
yeardata$rate<-yeardata$Number/yeardata$suid
vardata<-ldply(.data=indicators,.fun=function(x,yeardata){
			spd<-subset(yeardata,Species==x);
			vsp<-sd(spd$rate);
			spd$annStDev<-vsp;
			spd$yrmin<-spd$rate-spd$annStDev;
			spd$yrmin<-ifelse(spd$yrmin<0,0,spd$yrmin);
			spd$yrmax<-spd$rate+spd$annStDev;
			return(spd)
		},yeardata=yeardata)

p<-ggplot(data=vardata,aes(x=year,y=rate))+ geom_point(size=1.5) +
		geom_errorbar(aes(ymin=yrmin,ymax=yrmax)) +
		facet_wrap(~Species,ncol=3,scales="free")
