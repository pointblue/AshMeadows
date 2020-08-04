# TODO: Add comment
# 
# Author: lsalas
###############################################################################


##Processing GBBO data for visualizations
## This is Ash Meadows Point Count data curated by GBBO
## This code file generates a series of graphs that describe the survey effort (e.g., number of transects run each year)
## and the observations (e.g., number of species detected, top species detected, etc.)

# Libraries needed
library(XLConnect)
library(plyr)
library(unmarked)
library(ggplot2)

## This is the base path to where the files are located in Leo's computer.
## Edit this path to point to the folder in your computer
basepth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/IandMR8/AshMeadows"

## Reading the excel file
data<-try(readWorksheetFromFile(paste(basepth,"Ash Meadows NWR NBC data through 2016.xlsx",sep="/"),sheet="Point Count Data"))
# Reading the habitat data
load(file=paste(basepth,"/landbirdPoints_attributed_100.RData",sep=""))

## Plots
# p1 = Number of points surveyed per year
# p2 = Number of transects surveyed per year
# p3 = Average number of points per transect per year
# p4 = Number of species detected per year
# p5 = Average number of species detected per transect per year
# plottop10 = Plot the top 10 species (numbers) across all years
# plottop10y = Plot of the top 10 (numbers) species by year
# p20r = Top 20 species across all years, reporting the ratio in Ash riparian vs elsewhere
# p20c = Top 20 species raw counts

# to see any one of these, just type: p1 or p4 or...

##########
## Run the code below to see the plots described above
if(inherits(data,"try-error"))stop("Could not read the data. Please check the path, name of file and name of sheet")
data$Species<-gsub("Blue-Gray Gnatcatcher","Blue-gray Gnatcatcher",data$Species)

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
obs<-aggregate(as.formula("Number~Species+suid+year"),data=data,FUN=sum,na.rm=T)

#number of points and number of transects surveyed by year
#total number of species detected by year, by transect and year
#total counts by species by year (top 10), by species and year
#total number of 0 detections by species per point by year??

plot1<-ldply(.data=unique(effort$year),.fun=function(x,effort){
			q<-subset(effort,year==x);
			np<-NROW(unique(q$suid));
			tdf<-data.frame(year=x,npoints=np);
			return(tdf)
		},effort=effort)
p1<-ggplot(data=plot1, aes(x=year,y=npoints)) + geom_bar(stat="identity") +
		labs(title="Number of points surveyed per year", x="", y="# points") +
		scale_x_continuous(breaks=c(2003,2006,2009,2012,2015))

plot2<-ldply(.data=unique(effort$year),.fun=function(x,effort){
			q<-subset(effort,year==x);
			np<-NROW(unique(q$TransectID));
			tdf<-data.frame(year=x,ntrans=np);
			return(tdf)
		},effort=effort)
p2<-ggplot(data=plot2, aes(x=year,y=ntrans)) + geom_bar(stat="identity") +
		labs(title="Number of transects surveyed per year", x="", y="# transects") +
		scale_x_continuous(breaks=c(2003,2006,2009,2012,2015))

plot3<-ldply(.data=unique(effort$year),.fun=function(x,effort){
			q<-subset(effort,year==x);
			np<-sapply(X=unique(q$TransectID),FUN=function(x,q){
						w<-subset(q,TransectID==x);
						npt<-NROW(unique(w$suid));
						return(npt)
					},q=q);
			avgp<-mean(np)
			tdf<-data.frame(year=x,avgPointsTrans=avgp);
			return(tdf)
		},effort=effort)
p3<-ggplot(data=plot3, aes(x=year,y=avgPointsTrans)) + geom_bar(stat="identity") +
		labs(title="Avg. points/transect/year", x="", y="Avg. points") +
		scale_x_continuous(breaks=c(2003,2006,2009,2012,2015))


#do this for the plots below, but in reality should be done for each species separately..
obsdata<-merge(effort,obs,by=c("suid","year"),all.x=T)
obsdata$Number<-ifelse(is.na(obsdata$Number),0,obsdata$Number)

plot4<-ldply(.data=unique(obsdata$year),.fun=function(x,obsdata){
			q<-subset(obsdata,year==x);
			ns<-NROW(unique(q$Species));ns<-subset(ns,!is.na(ns));
			tdf<-data.frame(year=x,nspecies=ns);
			return(tdf)
		},obsdata=obsdata)
p4<-ggplot(data=plot4, aes(x=year,y=nspecies)) + geom_bar(stat="identity") +
		labs(title="Number of species detected per year", x="", y="# species") +
		scale_x_continuous(breaks=c(2003,2006,2009,2012,2015))

plot5<-ldply(.data=unique(obsdata$year),.fun=function(x,obsdata){
			q<-subset(obsdata,year==x);
			np<-sapply(X=unique(q$TransectID),FUN=function(x,q){
						w<-subset(q,TransectID==x);
						npt<-NROW(unique(w$Species));
						return(npt)
					},q=q);
			avgp<-mean(np)
			tdf<-data.frame(year=x,avgSpeciesTrans=avgp);
			return(tdf)
		},obsdata=obsdata)
p5<-ggplot(data=plot5, aes(x=year,y=avgSpeciesTrans)) + geom_bar(stat="identity") +
		labs(title="Avg. species/transect/year", x="", y="Avg. species") +
		scale_x_continuous(breaks=c(2003,2006,2009,2012,2015))

top10g<-aggregate(as.formula("Number~Species"),data=obsdata,FUN=sum, na.rm=T)
top10g<-top10g[order(top10g$Number,decreasing=TRUE),]
top10g<-top10g[1:10,]
plottop10<-ggplot(data=top10g,aes(x=Species,y=Number)) + geom_bar(stat="identity") +
		coord_flip() + labs(title="Top 10 species across all years", x="Species", y="Total count")

top10gy<-aggregate(as.formula("Number~Species+year"),data=obsdata,FUN=sum, na.rm=T)
top10gy<-subset(top10gy,Species %in% top10g$Species)
plottop10y<-ggplot(data=top10gy,aes(x=year,y=Number,group=Species)) + geom_bar(stat="identity",position="stack",aes(fill=Species),color="black") +
		coord_flip() + labs(title="Top 10 species by year", x="year", y="Species count")

obs10gy<-aggregate(as.formula("Number~Species+year"),data=obs,FUN=sum, na.rm=T)
top10gy<-subset(top10gy,Species %in% top10g$Species)

## Calculate the odds of being within vs outside mesquite points
## First take the max across visits
obsmax<-aggregate(Number~suid+year+TransectID+PointNumber+Species,data=obsdata,FUN=max,na.rm=T)
obsmax$SamplingUnitId<-toupper(obsmax$suid)
obsmax<-merge(obsmax,habpresence,by="SamplingUnitId",all.x=T)

#for each species estimate the ratio of detections in Mesquite vs elsewhere
#this is to select species. Later on we calculate the more proper metric from a logistic of proportion by year and accounting for the number sampled in each habtype
dat<-ldply(.data=unique(obsmax$Species), .fun=function(x,obsmax){
			sm1<-sum(subset(obsmax,Species==x & MesquiteB==1)$Number)/1686;	#1686 is the total number of point-years surveyed in Mesquite
			sm0<-sum(subset(obsmax,Species==x & MesquiteB==0)$Number)/2044; #2044 is the total number of point-years surveyed elsewhere
			ratio<-ifelse(sm0==0,sm1,round(sm1/sm0,3))
			res<-data.frame(Species=x,ratio=ratio,InMesq=round(sm1*1688),OutMesquite=round(sm0*2044));
			return(res)
		},obsmax=obsmax)


rawcounts<-aggregate(as.formula("Number~Species"),data=obs,FUN=sum, na.rm=T)
names(rawcounts)<-c("Species","RawCount")

dat<-merge(dat,rawcounts,by="Species")

#filter for those species with enough detections...
ne<-aggregate(SamplingUnitId~Species,obsmax,NROW);names(ne)<-c("Species","NumPointYrs")
dat<-merge(dat,ne,by="Species",all.x=T)

dat10<-subset(dat,NumPointYrs>9)
top20r<-dat10[order(dat10$ratio,decreasing=TRUE),]
top20rs<-top20r[1:20,]
p20r<-ggplot(data=top20rs,aes(x=Species,y=ratio)) + geom_bar(stat="identity") +
		coord_flip() + labs(title="Top 20 species across all years", x="Species", y="Number in Mesquite per 1 elsewhere")

p20c<-ggplot(data=top20rs,aes(x=Species,y=Count)) + geom_bar(stat="identity") +
		coord_flip() + labs(title="Top 20 species across all years", x="Species", y="Number detected")


##############################
## Calculate the odds of being within vs outside ash and riparian woodland points

#for each species estimate the ratio of detections in Mesquite vs elsewhere
#this is to select species. Later on we calculate the more proper metric from a logistic of proportion by year and accounting for the number sampled in each habtype
dat<-ldply(.data=unique(obsmax$Species), .fun=function(x,obsmax){
			sm1<-sum(subset(obsmax,Species==x & Riparian==1)$Number)/1535;	#1535 is the total number of point-years surveyed in Ash & riparian
			sm0<-sum(subset(obsmax,Species==x & Riparian==0)$Number)/2195; #2195 is the total number of point-years surveyed elsewhere
			ratio<-ifelse(sm0==0,sm1,round(sm1/sm0,3))
			res<-data.frame(Species=x,ratio=ratio,InRiparian=round(sm1*1535),OutRiparian=round(sm0*2195));
			return(res)
		},obsmax=obsmax)


rawcounts<-aggregate(as.formula("Number~Species"),data=obs,FUN=sum, na.rm=T)
names(rawcounts)<-c("Species","RawCount")

dat<-merge(dat,rawcounts,by="Species")

#filter for those species with enough detections...
ne<-aggregate(SamplingUnitId~Species,obsmax,NROW);names(ne)<-c("Species","NumPointYrs")
dat<-merge(dat,ne,by="Species",all.x=T)

dat10<-subset(dat,NumPointYrs>9)
top20r<-dat10[order(dat10$ratio,decreasing=TRUE),]
top20rs<-top20r[1:20,]
p20r<-ggplot(data=top20rs,aes(x=Species,y=ratio)) + geom_bar(stat="identity") +
		coord_flip() + labs(title="Top 20 species across all years", x="Species", y="Number in AshRiparian per 1 elsewhere")

p20c<-ggplot(data=top20rs,aes(x=Species,y=RawCount)) + geom_bar(stat="identity") +
		coord_flip() + labs(title="Top 20 species across all years", x="Species", y="Number detected")

