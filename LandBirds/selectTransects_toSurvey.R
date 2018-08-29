# TODO: Add comment
# 
# Author: lsalas
###############################################################################

 
points<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/GBBOpts_habitatAttrib.csv",stringsAsFactors=F)
points$transect<-substr(points$SamplingUnitId,1,regexpr("::",points$SamplingUnitId,fixed=T)-1)
points$point<-substr(points$SamplingUnitId,regexpr("::",points$SamplingUnitId,fixed=T)+2,nchar(points$SamplingUnitId))
points$FWSAshRiparian<-ifelse(points$FWSAsh==1 | points$FWSRiparian==1,1,0)

mesqB<-aggregate(MesquiteB~transect,points,sum)
ashRip<-aggregate(AshRiparian~transect,points,sum)
resto<-aggregate(Resto~transect,points,sum)
mesqFWS<-aggregate(FWSMesq~transect,points,sum)
ashRipFWS<-aggregate(FWSAshRiparian~transect,points,sum)

res<-merge(mesqB,mesqFWS,by="transect")
res<-merge(res,ashRip,by="transect")
res<-merge(res,ashRipFWS,by="transect")
res<-merge(res,resto,by="transect")

#transecs to select:
trans<-c("LR-75349","MQC-50885","MR-CRYSTALRESERVOIR","MR-POINTOFROCKS")

library(XLConnect)
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

#subset by year and species of interest
effort<-subset(effort,year==2015 & TransectID %in% trans)
data<-merge(data,effort, by=c("suid","TransectID","PointNumber","SurveyDate","year"),all.x=T)
obs<-aggregate(as.formula("Number~Species+suid+TransectID+visit+year"),data=data,FUN=sum,na.rm=T)
obs<-subset(obs,year==2015 & TransectID %in% trans)

#use only indicator species
indsp<-c("Brown-crested Flycatcher","Lucy's Warbler","Yellow-breasted Chat","Bell's Vireo",
		"Blue-gray Gnatcatcher","Yellow Warbler","Brown-headed Cowbird","Ash-throated Flycatcher",
		"Bewick's Wren","Gambel's Quail","Verdin","Crissal Thrasher")
obs<-subset(obs, Species %in% indsp)
woods<-merge(effort,obs,by=c("suid","TransectID","year","visit"),all.x=T)
NROW(unique(woods$suid))
aggregate(Number~visit,woods,sum)
aggregate(Number~visit+suid,woods,sum)


