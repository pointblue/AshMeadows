# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("RODBC","ggplot2","unmarked","fitdistrplus")
lapply(libs, require, character.only = TRUE)


source("C:/Users/lsalas/git/sparklemotion/AshMeadows/Marshbirds/AshMeadows_AnalysisUtils.R")
basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"


load(file=paste(basepth,"/marshIndicators_Data.RData",sep=""))

##################
sq<-ggplot(data=subset(plotdf,BirdCd != "COGA" & YearCollected > 2007),aes(x=YearCollected,y=ObservationCount,group=BirdCd)) + 
		geom_histogram(stat="identity",position="stack",aes(fill=BirdCd)) + 
		scale_x_continuous(breaks=c(2008,2010,2012,2014,2016)) +
		labs(x="Year",y="Total detections",fill="Species")

png(filename=paste(basepth,"/plots/NumDetctions_bySpeciesYear.png",sep=""),width=860,height=600,res=200)
print(sq)
dev.off()

#######################

p<-ggplot(data=plotsdf,aes(obs,pred)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(x="Observed density",y="Expected density") + facet_wrap(~Species,ncol=3,scales="free")
jpeg(filename=paste(basepth,"/plots/Marsh_RailGOF.jpg",sep=""),width=630,height=220,quality=100)
print(p)
dev.off()

#########################

geomean<-function(x){exp(mean(log(x)))}
riram<-aggregate(Predicted~YearCollected+Species,rira,geomean)
viram<-aggregate(Predicted~YearCollected+Species,vira,geomean)
soram<-aggregate(Predicted~YearCollected+Species,sora,geomean)

df1<-rbind(riram,viram);df1<-rbind(df1,soram)
#fit distribution...
fg<-try(fitdist(df1$Predicted*1.2, distr="gamma",method="mme"),silent=TRUE)
plot(fg)
#Now let's play with the bins
quants<-c(0.25,0.50,0.75)
vlvlg<-quantile(fg, probs = quants); vlvg<-as.numeric(vlvlg[[1]])

yrng<-sort(unique(df1$YearCollected)); runmeans<-data.frame()
for(yy in 3:NROW(yrng)){
	curmn<-mean(subset(df1,YearCollected %in% c(yrng[yy-2],yrng[yy-1],yrng[yy]))$Predicted)
	tdf<-data.frame(YearCollected=yrng[yy],IndexVal=curmn)
	runmeans<-rbind(runmeans,tdf)
}
df1<-merge(df1,runmeans,by="YearCollected",all.x=T)
#maxD<-max(df1$IndexVal,na.rm=T)*1.10
#vlvg<-c(maxD*0.20,maxD*0.45,maxD*0.75)

mv<-runmeans[nrow(runmeans),]
p1<-ggplot(df1,aes(x=YearCollected,y=Predicted)) + 
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=-5,ymax=vlvg[1],fill="red") + 
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[1],ymax=vlvg[2],fill="#FFFF00") +
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[2],ymax=vlvg[3],fill="#99CC00") +
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[3],ymax=max(df1$Predicted)*1.1,fill="#009900") +
		geom_hline(yintercept=0,color="black",size=0.5) +
		geom_hline(yintercept=25,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=50,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=75,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=100,color="black",size=0.5,linetype="dotted") +
		geom_point(aes(shape=Species),size=2) + 
		scale_x_continuous(breaks=c(2007,2009,2011,2013,2015,2017)) + labs(x="",y="")

png(filename=paste(basepth,"/plots/marshAbundance_bySpeciesYear.png",sep=""),width=750,height=700,res=200)
print(p1)
dev.off()

p2<-ggplot(df1,aes(x=YearCollected,y=IndexVal)) + 
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=-2.5,ymax=vlvg[1],fill="red") + 
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[1],ymax=vlvg[2],fill="#FFFF00") +
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[2],ymax=vlvg[3],fill="#99CC00") +
		geom_rect(xmin=2006.5,xmax=2017.5,ymin=vlvg[3],ymax=65,fill="#009900") +
		geom_hline(yintercept=10,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=20,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=30,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=40,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=50,color="black",size=0.5,linetype="dotted") +
		geom_point(shape=17,size=2) +
		geom_point(x=mv$YearCollected,y=mv$IndexVal,shape=17,size=2) +
		scale_x_continuous(limits=c(2009,2017),breaks=c(2009,2011,2013,2015,2017)) + 
		scale_y_continuous(limits=c(0,60),breaks=c(0,10,20,30,40,50)) +
		labs(x="",y="Density Index (birds/point)")

png(filename=paste(basepth,"/plots/Marsh_abundanceIndex_v2.png",sep=""),width=550,height=700,res=200)
print(p2)
dev.off()

#write the table
runmeans$Status<-ifelse(runmeans$IndexVal<vlvg[1],"Poor",
		ifelse(runmeans$IndexVal<vlvg[2],"Fair",
				ifelse(runmeans$IndexVal<vlvg[3],"Good","Very Good")))
write.csv(runmeans,file=paste(basepth,"/plots/Marsh_abundanceIndex.csv",sep=""))


#######################
# Diversity index
div<-ash[,c("BirdCd","YearCollected","StudyArea","Point","Visit","ObservationCount")]
div<-subset(div,BirdCd != "PBGR")
div<-aggregate(ObservationCount~BirdCd+YearCollected+StudyArea+Point+Visit,div,sum)
div<-aggregate(ObservationCount~BirdCd+YearCollected+StudyArea+Point,div,max)
div<-aggregate(ObservationCount~BirdCd+YearCollected,div,mean)
yrdiv<-data.frame()
for(yy in unique(div$YearCollected)){
	tdf<-subset(div,YearCollected==yy)
	tn<-sum(tdf$ObservationCount)
	tdf$prob<-tdf$ObservationCount/tn
	obsdiv<-exp(-1*as.numeric(tdf$prob%*%log(tdf$prob)))
	ydf<-data.frame(year=yy,Hobs=round(obsdiv,2),Hperc=round(obsdiv*100/6,2))
	yrdiv<-rbind(yrdiv,ydf)
}

yrng<-sort(unique(yrdiv$year)); divmeans<-data.frame()
for(yy in 3:NROW(yrng)){
	curmn<-mean(subset(yrdiv,year %in% c(yrng[yy-2],yrng[yy-1],yrng[yy]))$Hperc)
	tdf<-data.frame(year=yrng[yy],IndexVal=curmn)
	divmeans<-rbind(divmeans,tdf)
}

pd<-ggplot(divmeans,aes(x=year,y=IndexVal)) + 
		geom_rect(xmin=2007.5,xmax=2017.5,ymin=-7,ymax=25,fill="red") + 
		geom_rect(xmin=2007.5,xmax=2017.5,ymin=25,ymax=50,fill="#FFFF00") +
		geom_rect(xmin=2007.5,xmax=2017.5,ymin=50,ymax=75,fill="#99CC00") +
		geom_rect(xmin=2007.5,xmax=2017.5,ymin=75,ymax=105,fill="#009900") +
		geom_hline(yintercept=0,color="black",size=0.5) +
		geom_hline(yintercept=20,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=40,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=60,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=80,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=100,color="black",size=0.5,linetype="dotted") +
		geom_point(shape=17,size=2,color="black") +
		scale_x_continuous(breaks=c(seq(2009,2017,by=2))) +
		scale_y_continuous(limits=c(-2,100),breaks=seq(0,100,20)) +
		theme_bw() + labs(x="",y="Diversity Index (% of max. diversity)")

png(filename=paste(basepth,"/plots/Marsh_diversityIndex_v2.png",sep=""),width=550,height=600,res=200)
print(pd)
dev.off()


##############################################
# As heat map - overall index

##Let's try this:
#divmeans$pdiv<-divmeans$IndexVal/100
maxD<-max(df1$IndexVal,na.rm=T)*1.2
densmeans<-unique(df1[,c("YearCollected","IndexVal")]);densmeans$pdens<-densmeans$IndexVal/maxD
densmeans<-na.omit(densmeans)

#health<-divmeans[,c("year","pdiv")];health$pdens<-densmeans$IndexVal
health<-divmeans[,c("year","IndexVal")];health$pdens<-densmeans$IndexVal
nh<-nrow(health)

### How does it look like?
yv<-seq(0,100,by=0.5)
xv<-seq(0,maxD,by=0.5)
df<-data.frame()
for(yy in yv){	#x is density, y is diversity
	for(xx in xv){
		tdf<-data.frame(x=xx,y=yy)
		df<-rbind(df,tdf)
	}
}
#df$geomean<-apply(df[,c(1:2)],1,geomean)
df$geomean<-sqrt(df$x*df$y)
#fg<-try(fitdist(df$geomean, distr="gamma",method="mme"),silent=TRUE)
#plot(fg)
#quants<-c(0.30,0.55,0.8)
#vlvlg<-quantile(fg, probs = quants); vlvg<-as.numeric(vlvlg[[1]])

#df$color<-ifelse(df$geomean<vlvg[1],"Poor",ifelse(df$geomean<vlvg[2],"Fair",ifelse(df$geomean<vlvg[3],"Good","Very good")))
maxH<-max(df$geomean); hlvg<-c(maxH*0.25,maxH*0.5,maxH*0.75)
df$color<-ifelse(df$geomean<hlvg[1],"Poor",ifelse(df$geomean<hlvg[2],"Fair",ifelse(df$geomean<hlvg[3],"Good","Very good")))
p<-ggplot(df,aes(x=x,y=y)) + geom_tile(aes(fill=color)) +
		scale_fill_manual(values=c("#FFFF00","#99CC00","red","#009900")) +
		labs(x="Density Index",y="Diversity Index") + theme_bw() +
		theme(legend.position="none") 
for(rr in 1:nrow(health)){	#replace pdiv with IndexVal
	p<-p+geom_point(x=health[rr,"pdens"],y=health[rr,"IndexVal"],size=2) 
}
p <- p + geom_text(x=health[1,"pdens"],y=health[1,"IndexVal"],label=health[1,"year"],vjust = -0.6,size=2)
p <- p + geom_text(x=health[2,"pdens"],y=health[2,"IndexVal"],label=health[2,"year"],vjust = 1.6,size=2)
p <- p + geom_text(x=health[3,"pdens"],y=health[3,"IndexVal"],label=health[3,"year"],vjust = 1.6, hjust=1,size=2)
p <- p + geom_text(x=health[4,"pdens"],y=health[4,"IndexVal"],label=health[4,"year"],vjust = 1.6, hjust=0,size=2)
p <- p + geom_text(x=health[5,"pdens"],y=health[5,"IndexVal"],label=health[5,"year"],vjust = -0.6, hjust=1,size=2)
p <- p + geom_text(x=health[6,"pdens"],y=health[6,"IndexVal"],label=health[6,"year"],vjust = -0.6,size=2)
p <- p + geom_text(x=health[7,"pdens"],y=health[7,"IndexVal"],label=health[7,"year"],vjust = -0.6,size=2)
p <- p + geom_text(x=health[8,"pdens"],y=health[8,"IndexVal"],label=health[8,"year"],vjust = -0.6,size=2)

png(filename=paste(basepth,"/plots/Marsh_overallHealthIndex_v1A.png",sep=""),width=750,height=700,res=200)
print(p)
dev.off()



#############################
#Overall health index

##Let's try this:
divmeans$pdiv<-divmeans$IndexVal/100
maxabund<-max(df1$IndexVal,na.rm=T)
densmeans<-unique(df1[,c("YearCollected","IndexVal")]); #densmeans$pdens<-densmeans$IndexVal/maxabund
densmeans<-na.omit(densmeans)

health<-divmeans[,c("year","pdiv")];health$pdens<-densmeans$pdens
health$overall<-sqrt(health$pdiv*health$pdens)*100

p<-ggplot(health,aes(x=year,y=overall)) + 
		geom_rect(xmin=2008.5,xmax=2017.5,ymin=-7,ymax=30,fill="red") + 
		geom_rect(xmin=2008.5,xmax=2017.5,ymin=30,ymax=55,fill="#FFFF00") +
		geom_rect(xmin=2008.5,xmax=2017.5,ymin=55,ymax=80,fill="#99CC00") +
		geom_rect(xmin=2008.5,xmax=2017.5,ymin=80,ymax=105,fill="#009900") +
		geom_hline(yintercept=0,color="black",size=0.5) +
		geom_hline(yintercept=20,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=40,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=60,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=80,color="black",size=0.5,linetype="dotted") +
		geom_hline(yintercept=100,color="black",size=0.5,linetype="dotted") +
		geom_point(shape=17,size=2,color="black") +
		scale_x_continuous(breaks=c(seq(2009,2017,by=2))) +
		scale_y_continuous(limits=c(-2,100),breaks=seq(0,100,20)) +
		theme_bw() + labs(x="",y="Index of Marsh Health")

png(filename=paste(basepth,"/plots/Marsh_overallHealthIndex_v2.png",sep=""),width=550,height=700,res=200)
print(p)
dev.off()

