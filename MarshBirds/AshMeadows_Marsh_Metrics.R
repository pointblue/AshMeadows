# TODO: Add comment
# 
# Author: lsalas
###############################################################################


####################################################################################################################################
####################################################################################################################################
## READ THIS

# This file is provided for documentation purposes. The objective of the file is to generate the data needed for the 
# Ash Meadows indicator indices (plots), in the code file makePlots.R
# Every time a new dataset is pulled from the AKN (for now using the Downloader tool: https://data.pointblue.org/apps/downloader/)
# this code file must be run, judiciously inspecting the models and altering the code accordingly to choose the better models
# and generating the file marshIndicators_Data.RData 

####################################################################################################################################
####################################################################################################################################

## Checking for install of necessary packages
libs<-c("ggplot2","unmarked","fitdistrplus","timeDate","maptools","sp")
libcheck<-lapply(libs,function(pp){
			if(!pp %in% installed.packages()){
				install.packages(pp,repos="https://cloud.r-project.org/")
				return(paste(pp,"was installed"))
			}else{
				return(paste(pp,"already installed"))
			}
		})

## Load librariesf
lapply(libs, require, character.only = TRUE)

## MUST download the file AshMeadows_AnalysisUtils.R from the GitHub repository, then point to it in the next line
source("C:/Users/lsalas/git/sparklemotion/AshMeadows/Marshbirds/AshMeadows_AnalysisUtils.R")

basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"

##############################################################################################
## Goodness of fit calculation functions

fitstats<-function(mdl) {
	observed <- getY(mdl@data)
	expected <- fitted(mdl)
	resids <- residuals(mdl)
	sse <- sum(resids^2, na.rm=T)
	chisq <- sum((observed - expected)^2 / expected,na.rm=T)
	freeTuke <- sum((sqrt(observed) - sqrt(expected))^2,na.rm=T)
	out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
	return(out)
}

Nhat<-function(mdl,kval=50) {
	sum(bup(ranef(mdl, K=kval)),na.rm=T)
}


#################################################################################################

## Read the data downloaded from the Downloader tool
df<-read.csv("c:/users/lsalas/downloads/ashmeadows_all.csv", stringsAsFactors=FALSE)

#load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/Data/AshMeadows_marshbird_indicatorData.RData") #this is the same as the csv.

## Find best-fitting models.
flds<-c("ProjectCode","StudyArea","Transect","Point","SamplingUnitId","DecimalLatitude","DecimalLongitude","Visit",
		"ProtocolCode","YearCollected","MonthCollected","DayCollected","JulianDay","Time","ScientificName",
		"CommonName","BirdCd","DistanceFromObserver","SamplingEventStartTm","ObservationCount","NoObservations","FocalSpeciesInd",          
		"SamplingEventDataStatusCd","TimeBinId")

amdf<-df[,flds]

#create some statistics for each species
num.events<-aggregate(as.formula("ProtocolCode~ProjectCode+StudyArea+Transect+Point+SamplingUnitId+YearCollected"),
		data=unique(amdf[,c("ProjectCode","ProtocolCode","StudyArea","Transect","Point","SamplingUnitId","YearCollected","JulianDay","SamplingEventStartTm")]),FUN=NROW)
names(num.events)<-c("ProjectCode","StudyArea","Transect","Point","SamplingUnitId","YearCollected","numVisits")
num.visits<-aggregate(as.formula("Visit~ProjectCode+StudyArea+Transect+Point+SamplingUnitId+YearCollected"),data=amdf,FUN=max)
nrow(num.events)==nrow(num.visits)
ash<-merge(amdf,num.events,all.x=T)

# Visualize number of birds detected per species
spdata<-aggregate(as.formula("ObservationCount~ProjectCode+StudyArea+Transect+Point+SamplingUnitId+YearCollected+JulianDay+BirdCd+SamplingEventStartTm"),data=ash,FUN=max)
spplot1<-aggregate(as.formula("ObservationCount~ProjectCode+StudyArea+Transect+Point+SamplingUnitId+YearCollected+BirdCd"),data=spdata,FUN=mean)
spplot2<-aggregate(as.formula("ObservationCount~ProjectCode+YearCollected+BirdCd"),data=spplot1,FUN=sum)

p<-ggplot(data=spplot2,aes(x=YearCollected,y=ObservationCount)) + geom_point(aes(color=BirdCd)) + 
		geom_line(aes(color=BirdCd)) + scale_x_continuous(breaks=c(2007,2009,2011,2013,2015,2017)) +
		labs(x="",y="Mean # birds detected")

#minimal data filters and edits...
ash<-subset(ash,!is.na(ScientificName))
ash$BirdCd<-ifelse(ash$BirdCd=="CLRA","RIRA",as.character(ash$BirdCd))
ash$SurveyDate<-as.Date(paste(ash$YearCollected,ash$MonthCollected,ash$DayCollected,sep="-"))

#re-generate the visits count for the effort table
eff<-unique(ash[,c("SamplingUnitId","StudyArea","Point","YearCollected","JulianDay","SurveyDate","SamplingEventStartTm")])
eff$key<-paste(eff$StudyArea,eff$Point,eff$YearCollected,sep="::")

kdf<-data.frame()
for(kk in unique(eff$key)){
	tdf<-subset(eff,key==kk)
	tdf<-tdf[order(tdf$JulianDay,tdf$SamplingEventStartTm),]
	tdf$est.visit<-c(1:nrow(tdf))
	tdf<-tdf[,c("key","JulianDay","SamplingEventStartTm","est.visit")]
	kdf<-rbind(kdf,tdf)
}
eff<-merge(eff,kdf,by=c("key","JulianDay","SamplingEventStartTm"),all.x=T)
eff$Y_COORD<-36.425;eff$X_COORD<--116.333333

#add point attributions
load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/marshPoints_attributed_200.RData")
eff<-merge(eff,pointsAtt,by="SamplingUnitId",all.x=T)

#use 200m apart points as in 2013
pts<-unique(subset(eff,YearCollected==2013)$SamplingUnitId)
eff<-subset(eff,SamplingUnitId %in% pts)
ash<-subset(ash,SamplingUnitId %in% pts)

#table of areas surveyed and detections
df<-unique(eff[,c("SamplingUnitId","YearCollected","Area_emgMarsh")])
dfa<-aggregate(Area_emgMarsh~YearCollected,df,sum)
dfa$Area_emgMarsh<-round(dfa$Area_emgMarsh/10000,1)
dfp<-aggregate(SamplingUnitId~YearCollected,df,NROW);names(dfp)<-c("YearCollected","NumPoints")
dfa<-merge(dfa,dfp,by="YearCollected")

### Bar plot of detections by species
plotdf<-getNumDetectionsbyYear(df=ash[,c("SamplingUnitId","YearCollected","JulianDay","SamplingEventStartTm","BirdCd","ObservationCount")],
		eff=eff[,c("SamplingUnitId","YearCollected","JulianDay","SamplingEventStartTm")],fct="sum")


sq<-ggplot(data=subset(plotdf,BirdCd != "COGA" & YearCollected > 2007),aes(x=YearCollected,y=ObservationCount,group=BirdCd)) + 
		geom_histogram(stat="identity",position="stack",aes(fill=BirdCd)) + 
		scale_x_continuous(breaks=c(2008,2010,2012,2014,2016)) +
		labs(x="Year",y="Total detections",fill="Species")

png(filename=paste(basepth,"/plots/NumDetctions_bySpeciesYear.png",sep=""),width=860,height=600,res=200)
print(sq)
dev.off()

###############################################################################################################

## Prepare the data for abundance modeling for RIRA
datalist<-prepUDFdata(df=ash,eff,occ=FALSE)

splst<-datalist[["RIRA"]]
ydf<-splst$ydf
covsdf<-splst$covsdf
ocovs<-splst$ocovs
udfr<-unmarkedFramePCount(y=ydf,siteCovs=covsdf,obsCovs=ocovs)

## Competing models
mdl1r<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)"),data=udfr,K=200,mixture="P")
mdl1ar<-pcount(formula=as.formula("~JulianDay+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfr,K=200,mixture="NB")
mdl2r<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+StudyArea"),data=udfr,K=200,mixture="NB")	#TOP
mdl2ar<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)*StudyArea"),data=udfr,K=200,mixture="NB")
mdl3r<-pcount(formula=as.formula("~1 ~as.factor(YearCollected)"),data=udfr,K=200,mixture="NB")
mdl4r<-pcount(formula=as.formula("~JulianDay+diffRS+as.factor(YearCollected) ~as.factor(YearCollected)"),data=udfr,K=200,mixture="NB")
mdl5r<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)+StudyArea"),data=udfr,K=200,mixture="NB")
mdl5ar<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)+StudyArea"),data=udfr,K=200,mixture="NB")
mdl6r<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)"),data=udfr,K=200,mixture="NB")
mdl7r<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)"),data=udfr,K=200,mixture="NB") 
mdl7ra<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+log(Area_alkMeadow)"),data=udfr,K=200,mixture="P") 

#top model
predr<-predict(mdl7r,type="state")
rira<-cbind(covsdf,predr)
rira$Species<-"RIRA"

mdl<-mdl7r
observed <- getY(mdl@data)
expected <- fitted(mdl)
resids <- residuals(mdl)

## Visualize obs vs expected
pdf<-data.frame(obs=apply(observed,1,mean,na.rm=T),pred=apply(expected,1,mean,na.rm=T),resid=apply(resids,1,mean,na.rm=T),Species="Yuma RIRA")
plotsdf<-pdf
p<-ggplot(data=pdf,aes(obs,pred)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(x="Observed density",y="Expected density")
jpeg(filename=paste(basepth,"/plots/Marsh_RIRAgof.jpg",sep=""),width=230,height=200,quality=100)
print(p)
dev.off()

mdl<-mdl7r
gofrira<-parboot(mdl, fitstats, nsim=25, report=1)

#not use
set.seed(345)
pb.N <- parboot(mdl, Nhat, nsim=25, report=5)
colSums(confint(ranef(mdl, K=100)))
print(pb.N)


################################################################################################################


## Prepare the data for abundance modeling for VIRA
splst<-datalist[["VIRA"]]
ydf<-splst$ydf
covsdf<-splst$covsdf
ocovs<-splst$ocovs
udfv<-unmarkedFramePCount(y=ydf,siteCovs=covsdf,obsCovs=ocovs)

mdl1v<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl1av<-pcount(formula=as.formula("~JulianDay+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl1bv<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+sqrt(Area_emgMarsh)"),data=udfv,K=200,mixture="NB")
mdl2v<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+StudyArea"),data=udfv,K=200,mixture="NB")	
mdl2av<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)*StudyArea"),data=udfv,K=200,mixture="NB")
mdl3v<-pcount(formula=as.formula("~1 ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl4v<-pcount(formula=as.formula("~JulianDay+diffRS+as.factor(YearCollected) ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB") #Top, but not a good idea?
mdl5v<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)+StudyArea"),data=udfv,K=200,mixture="NB")
mdl5av<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)+StudyArea"),data=udfv,K=200,mixture="NB")
mdl6v<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl6av<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2)+diffRS ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl6bv<-pcount(formula=as.formula("~JulianDay+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl6cv<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2)+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")
mdl7v<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)"),data=udfv,K=200,mixture="NB")

predv<-predict(mdl1bv,type="state")
vira<-cbind(covsdf,predv)
vira$Species<-"VIRA"

mdl<-mdl1bv
observed <- getY(mdl@data)
expected <- fitted(mdl)
resids <- residuals(mdl)

pdf<-data.frame(obs=apply(observed,1,mean,na.rm=T),pred=apply(expected,1,mean,na.rm=T),resid=apply(resids,1,mean,na.rm=T),Species="VIRA")
plotsdf<-rbind(plotsdf,pdf)
p<-ggplot(data=pdf,aes(obs,pred)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(x="Observed density",y="Expected density")
jpeg(filename=paste(basepth,"/plots/Marsh_VIRAgof.jpg",sep=""),width=230,height=200,quality=100)
print(p)
dev.off()

mdl<-mdl1bv
gofvira<-parboot(mdl, fitstats, nsim=25, report=1)


#############


## Prepare the data for abundance modeling for SORA
splst<-datalist[["SORA"]]
ydf<-splst$ydf
covsdf<-splst$covsdf
ocovs<-splst$ocovs
udfs<-unmarkedFramePCount(y=ydf,siteCovs=covsdf,obsCovs=ocovs)

mdl1s<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl1as<-pcount(formula=as.formula("~JulianDay+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
#Can't do: mdl2s<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+StudyArea"),data=udfs,K=20,mixture="NB")	#TOP
#Can't do: mdl2as<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)*StudyArea"),data=udfs,K=20,mixture="NB")
mdl3s<-pcount(formula=as.formula("~1 ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl4s<-pcount(formula=as.formula("~JulianDay+diffRS+as.factor(YearCollected) ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
#Can't do: mdl5s<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)+StudyArea"),data=udfs,K=20,mixture="NB")
#Can't do: mdl5as<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)+StudyArea"),data=udfs,K=20,mixture="NB")
mdl6s<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2) ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl6as<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2)+diffRS ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl6bs<-pcount(formula=as.formula("~JulianDay+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl6cs<-pcount(formula=as.formula("~JulianDay+I(JulianDay^2)+diffRS+I(diffRS^2) ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
mdl7s<-pcount(formula=as.formula("~JulianDay ~as.factor(YearCollected)"),data=udfs,K=20,mixture="NB")
#Can't do: mdl2sa<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+StudyArea+sqrt(Area_emgMarsh)+AlkMeadow"),data=udfs,K=20,mixture="NB")	#TOP
mdl7as<-pcount(formula=as.formula("~JulianDay+diffRS ~as.factor(YearCollected)+AlkMeadow+sqrt(Area_emgMarsh)"),data=udfs,K=20,mixture="NB")

preds<-predict(mdl7as,type="state")
sora<-cbind(covsdf,preds)
sora$Species<-"SORA"

mdl<-mdl7as
observed <- getY(mdl@data)
expected <- fitted(mdl)
resids <- residuals(mdl)

pdf<-data.frame(obs=apply(observed,1,mean,na.rm=T),pred=apply(expected,1,mean,na.rm=T),resid=apply(resids,1,mean,na.rm=T),Species="SORA")
plotsdf<-rbind(plotsdf,pdf)
p<-ggplot(data=pdf,aes(obs,pred)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(x="Observed density",y="Expected density")
jpeg(filename=paste(basepth,"/plots/Marsh_SORAgof.jpg",sep=""),width=230,height=200,quality=100)
print(p)
dev.off()

mdl<-mdl7as
gofsora<-parboot(mdl, fitstats, nsim=25, report=1)

## PLot overall GOF
p<-ggplot(data=plotsdf,aes(obs,pred)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(x="Observed density",y="Expected density") + facet_wrap(~Species,ncol=3,scales="free")
jpeg(filename=paste(basepth,"/plots/Marsh_RailGOF.jpg",sep=""),width=630,height=220,quality=100)
print(p)
dev.off()

## Save the predicted values
save(ash,eff,plotdf,rira,vira,sora,plotsdf,file=paste(basepth,"/marshIndicators_Data.RData",sep=""))

