# TODO: Add comment
# 
# Author: lsalas
###############################################################################

####################################################################################################################################
####################################################################################################################################
## READ THIS

# This file is provided for documentation purposes. The objective of the file is to generate the data needed for the 
# Ash Meadows indicator indices (plots) for landbirds. All the code is contained in this file, including code to generate 
# the indicator plots. Every time a new dataset is obtained from the Great Basin Bird Observatory dataset,
# this code file must be run, judiciously inspecting the models and altering the code accordingly to choose the better models
# and generating the output plots. 

## Attention: the package XLConnect requires that the JRE (java runtime engine) be installed and in the path)

####################################################################################################################################
####################################################################################################################################

## Checking for install of necessary packages
libs<-c("XLConnect","plyr","ggplot2","unmarked","fitdistrplus","timeDate","maptools","sp")
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

###############
## Functions we need...

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

getJday<-function(df){
#calculate jdayVE
	df$vern.equi<-as.Date(timeDate(JPVernalEquinox(df$year),zone="MST"))
	df$jday.ve<-as.numeric(as.Date(df$SurveyDate)-df$vern.equi)
	return(df)
}

# This function prepares the data for fitting hierarchical imperfect detection models
makeUDF<-function(df){
	dfl<-df[,c("suid","TransectID","year","visit","Number")]
	dfw<-reshape(dfl,idvar=c("suid","TransectID","year"),timevar="visit",direction="wide")
	names(dfw)<-gsub("Number","Visit",names(dfw))
	
	jdl<-df[,c("suid","TransectID","year","visit","jday.ve")]
	jdw<-reshape(jdl,idvar=c("suid","TransectID","year"),timevar="visit",direction="wide")
	names(jdw)<-gsub("jday.ve","JDay",names(jdw))
	
	udm<-merge(dfw,jdw,by=c("suid","TransectID","year"))
	ydat<-udm[,grep("Visit",names(udm))]
	scov<-udm[,c("suid","TransectID","year")]
	ocov<-list(JDay=udm[,grep("JDay",names(udm))])
	udf<-unmarkedFramePCount(y=ydat,siteCov=scov,obsCovs=ocov)
	return(udf)
}

#############################################################################################
basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"
data<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Point Count Data"))
#habitat data
load(file=paste(basepth,"/landbirdPoints_attributed_100.RData",sep=""))

if(inherits(data,"try-error"))stop("Could not read the data. Please check the path, name of file and name of sheet")
data$Species<-gsub("Blue-Gray Gnatcatcher","Blue-gray Gnatcatcher",data$Species)
data$TransectID<-toupper(data$TransectID)
data$suid<-paste(data$TransectID,data$PointNumber,sep="::")
data$year<-as.integer(format(data$SurveyDate,"%Y"))
data<-subset(data,year==2015 & TransectID %in% c("LR-75349","MQC-50885","MR-CRYSTALRESERVOIR","MR-POINTOFROCKS"))

effort<-unique(data[,c("suid","TransectID","PointNumber","SurveyDate","year")])
effort<-generateVisit(df=effort)
effort<-getJday(effort)

#131 locations, 540 point-years, 450 with 1 visit, 90 with 2 visits
habpresence$ripWood<-apply(habpresence[,2:4],1,sum)
habpresence<-subset(habpresence,ripWood>0)
data<-subset(data,suid %in% habpresence$SamplingUnitId)
effort<-subset(effort,suid %in% habpresence$SamplingUnitId)

###### DENSITIES
#Cycle through each species' data
#Shuf suggests dropping YBCH and YWAR
species<-c("Brown-crested Flycatcher","Lucy's Warbler","Bell's Vireo","Blue-gray Gnatcatcher",
		"Ash-throated Flycatcher","Bewick's Wren","Gambel's Quail","Verdin","Crissal Thrasher")


#only 2015 has 2 visits
udfdata<-list();zeroTransectList<-list()
for(ss in species){
	obs<-subset(data,Species==ss)
	spd<-aggregate(as.formula("Number~TransectID+suid+year+SurveyDate"),data=obs,FUN=sum,na.rm=T)
	mrg<-merge(effort,spd[,c("suid","TransectID","year","SurveyDate","Number")],by=c("suid","TransectID","year","SurveyDate"),all.x=T)
	mrg$Number<-ifelse(is.na(mrg$Number),0,mrg$Number)
	zt<-aggregate(Number~TransectID,mrg,sum)
	ztv<-subset(zt,Number==0);nztv<-subset(zt,Number>0)
	mrg<-subset(mrg,TransectID %in% nztv$TransectID)
	if(nrow(mrg)>0){
		udfdata[[ss]]<-makeUDF(mrg)
	}else{
		udfdata[[ss]]<-NA
	}
	if(nrow(ztv)>0){
		ztd<-unique(data[,c("TransectID","suid","year")]);ztd<-subset(ztd,TransectID %in% ztv$TransectID)
		ztd$Estimate<-0;ztd$Lower<-0;ztd$Upper<-0;ztd$Species<-ss
		zeroTransectList[[ss]]<-ztd
	}else{
		zeroTransectList[[ss]]<-NA
	}
	
}

#Fit the abundance models
mfml<-as.formula("~JDay ~TransectID")
estimates<-data.frame()
for(ss in species){
	udfd<-udfdata[[ss]]
	if(class(udfd)=="unmarkedFramePCount"){
		yd<-getY(udfd);kv<-max(yd)*4
		est<-try(pcount(mfml,data=udfd,K=kv,mixture="NB",engine="C",se=TRUE),silent=T)
		if(!inherits(est,"try-error")){
			estdf<-siteCovs(udfd);estdf<-estdf[,c("TransectID","suid","year")]
			re<-ranef(est)
			estdf$Estimate<-bup(re, stat="mean")
			pred.re<-as.data.frame(confint(re, level=0.95)); names(pred.re)<-c("Lower","Upper")
			estdf$Lower<-pred.re$Lower;estdf$Upper<-pred.re$Upper
			estdf$Species<-ss
			#add ztdf
			zdf<-zeroTransectList[[ss]]
			if(!is.na(zdf)){
				estdf<-rbind(estdf,zdf)
			}
			estimates<-rbind(estimates,estdf)
		}
	}
}


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

gofrira<-parboot(object=est, fitstats, nsim=25, report=1)


## Plot the average per transect:
p<-ggplot(estimates,aes(x=suid,y=Estimate)) + geom_point(aes(color=TransectID)) + geom_errorbar(aes(ymin=Lower,ymax=Upper,color=TransectID)) +
		theme(axis.text.x=element_text(size=8)) + theme(axis.text.y=element_text(size=6)) +
	coord_flip() + facet_wrap(~Species,ncol=3) + labs(x="",y="Birds per point",color="Transect")

png(filename=paste(basepth,"/plots/AshWood_abundances.png",sep=""),width=1800,height=1800,res=200)
print(p)
dev.off()

transest<-aggregate(Estimate~TransectID+year+Species,estimates,mean)
transsd<-aggregate(Estimate~TransectID+year+Species,estimates,sd);names(transsd)<-c("TransectID","year","Species","StDev")
transest<-merge(transest,transsd,by=c("TransectID","year","Species"))
transest$Upper<-transest$Estimate+(1.96*transest$StDev)
transest$Lower<-transest$Estimate-(1.96*transest$StDev);transest$Lower<-ifelse(transest$Lower<0,0,transest$Lower)

p<-ggplot(data=transest,aes(x=TransectID,y=Estimate)) + geom_point() + geom_errorbar(aes(ymin=Lower,ymax=Upper),width=0.5) + theme_bw() +
		coord_flip() + facet_wrap(~Species,ncol=3) + labs(x="",y="Birds per point",color="Species") + theme(legend.position="none")

png(filename=paste(basepth,"/plots/AshWood_transectAbundances.png",sep=""),width=1500,height=1300,res=200)
print(p)
dev.off()

#We have only one year to draw the quality bins, so...
tmv<-aggregate(Estimate~TransectID+Species,estimates,mean);names(tmv)<-c("TransectID","Species","TransectMean") #mean for each species and transect
tmvs<-aggregate(TransectMean~TransectID,tmv,sum);names(tmvs)<-c("TransectID","TransectMean") #add up the species means by transect
tmvss<-aggregate(Estimate~TransectID,estimates,mean);names(tmvss)<-c("TransectID","TransectMean") #add up species overall (for plot below)
estimates<-merge(estimates,tmvss,by=c("TransectID"))
est2<-aggregate(Estimate~Species+year+TransectID+TransectMean,estimates,mean)
p<-ggplot(est2,aes(x=year,y=Estimate)) + 
		geom_point(aes(color=TransectID),position=position_dodge(width=0.5)) + 
		geom_point(aes(y=TransectMean,color=TransectID),shape=17,size=3,position=position_dodge(width=0.5)) +
		#geom_boxplot(fill=NA,size=0.6, width=0.3) + 
		theme_bw() + theme(axis.text.x=element_blank()) +
		scale_x_continuous(breaks=2015) + labs(x="Transects",y="Birds per point",color="Transect")

png(filename=paste(basepth,"/plots/AshWood_transectPerformance.png",sep=""),width=500,height=380,res=100)
print(p)
dev.off()

#let's fit a gamma to the data and then take quantiles...
fg<-try(fitdist(est2$Estimate, distr="gamma",method="mme"),silent=TRUE)
plot(fg)
#Now let's play with the bins
quants<-c(0.25,0.5,0.75)
vlvlg<-quantile(fg, probs = quants); vlvg<-as.numeric(vlvlg[[1]])
mv<-mean(estimates$Estimate)
p<-ggplot(est2,aes(x=year,y=Estimate)) + 
		geom_rect(xmin=2014.5,xmax=2015.5,ymin=-0.5,ymax=vlvg[1],fill="#F7D1D2") + 
		geom_rect(xmin=2014.5,xmax=2015.5,ymin=vlvg[1],ymax=vlvg[2],fill="#FEF1C6") +
		geom_rect(xmin=2014.5,xmax=2015.5,ymin=vlvg[2],ymax=vlvg[3],fill="#E2EFD9") +
		geom_rect(xmin=2014.5,xmax=2015.5,ymin=vlvg[3],ymax=30,fill="#9CC97D") +
		geom_point(aes(color=TransectID),position=position_dodge(width=0.5)) + 
		geom_point(aes(y=TransectMean,color=TransectID),shape=17,size=3,position=position_dodge(width=0.5)) +
		#geom_boxplot(fill=NA,size=0.6, width=0.3) + theme_bw() +
		geom_point(y=mv,shape=17,size=3) +
		#geom_hline(yintercept=mv,color="blue",size=1) +
		theme_bw() + theme(axis.text.x=element_blank()) +
		scale_x_continuous(breaks=2015) + labs(x="Transects",y="Density Index (birds/point)")

png(filename=paste(basepth,"/plots/AshWood_abundanceIndex.png",sep=""),width=1000,height=800,res=200)
print(p)
dev.off()

#write the table
tmvss[5,]<-c("Overall",mv)
names(tmvss)<-c("Location","MeanEstimate")
tmvss$MeanEstimate<-round(as.numeric(tmvss$MeanEstimate),2)
write.csv(tmvss,file=paste(basepth,"/plots/AshWood_abundanceIndex.csv",sep=""))

#####################################################################################
### Diversity index...
divind<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows/Summary Indicators/Ash_Riparian/DiversityIndicators.csv",stringsAsFactors=F)

#create table with max number of detections for the indicators:
divdat<-aggregate(Number~Species+year,subset(data,Species %in% divind$Species),sum,na.rm=T)
divdat<-subset(divdat,Number>0)

#there is only one year, otherwise loop on year
divdat$pv<-divdat$Number/sum(divdat$Number)
divdat$Shannon<-(-1)*divdat$pv*log(divdat$pv)
ShannonVal<-round(exp(sum(divdat$Shannon)),2)
nsp<-NROW(unique(divdat$Species)); phm<-1/nsp

#Let's calculate by transect and plot...
divdatt<-aggregate(Number~Species+year+TransectID,subset(data,Species %in% divind$Species),sum,na.rm=T)
divdatt<-subset(divdatt,Number>0)
transectAb<-aggregate(Number~TransectID+year,divdatt,sum);names(transectAb)<-c("TransectID","year","TransTotal")
divdatt<-merge(divdatt,transectAb,by=c("TransectID","year"))
divdatt$pv<-divdatt$Number/divdatt$TransTotal
divdatt$Shannon<-(-1)*divdatt$pv*log(divdatt$pv)
transShannon<-aggregate(Shannon~TransectID+year,divdatt,FUN=function(x){round(exp(sum(x)),2)})
nspt<-aggregate(Species~TransectID+year,divdatt,NROW); names(nspt)<-c("TransectID","year","NumSpecies")
#nspt$phm<-1/nspt$NumSpecies
#nspt$Hmax<-round(exp((-1)*nspt$NumSpecies*nspt$phm*log(nspt$phm)),2)
transShannon<-merge(transShannon,nspt,by=c("TransectID","year"))
transShannon<-transShannon[,c("TransectID","year","NumSpecies","Shannon")]
transShannon[5,]<-c("Overall",2015,nsp,ShannonVal); names(transShannon)<-c("Location","year","NumSpecies","Diversity")
transShannon$PercentMaxDiversity<-round(as.numeric(transShannon$Diversity)*100/nrow(divind),0)

write.csv(transShannon,file=paste(basepth,"/plots/AshWood_diversityIndex.csv",sep=""))

p<-ggplot(transShannon,aes(x=Location,y=PercentMaxDiversity)) + 
		geom_rect(xmin=0,xmax=5.8,ymin=-5,ymax=25,fill="#F7D1D2") + 
		geom_rect(xmin=0,xmax=5.8,ymin=25,ymax=50,fill="#FEF1C6") +
		geom_rect(xmin=0,xmax=5.8,ymin=50,ymax=75,fill="#E2EFD9") +
		geom_rect(xmin=0,xmax=5.8,ymin=75,ymax=100,fill="#9CC97D") +
		geom_point(shape=17,size=3) +
		scale_y_continuous(limits=c(-5,100),breaks=seq(0,100,20)) +
		theme_bw() + 
		theme(axis.text.x=element_text(size=8,angle=45,hjust=1)) +
		labs(x="",y="Diversity Index (% of max. diversity)")

png(filename=paste(basepth,"/plots/AshWood_diversityIndex.png",sep=""),width=600,height=900,res=200)
print(p)
dev.off()


###########################################################################
## Overall Index:  ATTENTION! using the sum of abundances of all species by transect
health<-mean(tmvs$TransectMean)*ShannonVal/nrow(divind)	#nrow(divind) is the max number of species monotored for the diversity index
maxHealth<-max(as.numeric(tmvs$TransectMean))*1.2
healthIndex<-health/maxHealth

##Let's try this:
diversityVal<-subset(transShannon,Location=="Overall")$PercentMaxDiversity
abundanceVal<-subset(tmvss,Location=="Overall")$MeanEstimate
maxAbVal<-abundanceVal*1.2 #this is approx. 2.5, make it 3 birds per point?

### How does it look like?
yv<-seq(0,100,by=0.5)	#diversity
xv<-seq(0,3,by=0.02)	#abundance
df<-data.frame()
for(yy in yv){
	for(xx in xv){
		iv<-yy*xx
		tdf<-data.frame(x=xx,y=yy,index=iv)
		df<-rbind(df,tdf)
	}
}
summary(df$index)
fg<-try(fitdist(df$index, distr="gamma",method="mme"),silent=TRUE)
plot(fg)
quants<-c(0.30,0.50,0.75)
vlvlg<-quantile(fg, probs = quants); vlvg<-as.numeric(vlvlg[[1]])

df$color<-ifelse(df$index<vlvg[1],"Poor",ifelse(df$index<vlvg[2],"Fair",ifelse(df$index<vlvg[3],"Good","Very good")))
p<-ggplot(df,aes(x=x,y=y)) + geom_tile(aes(fill=color)) +
		scale_fill_manual(values=c("#FEF1C6","#E2EFD9","#F7D1D2","#9CC97D")) +
		geom_point(x=abundanceVal,y=diversityVal,shape=17,size=4) +
		labs(x="Density Index",y="Diversity Index") + theme_bw() +
		theme(legend.position="none")

png(filename=paste(basepth,"/plots/AshWood_overallHealthIndex.png",sep=""),width=600,height=600,res=200)
print(p)
dev.off()

