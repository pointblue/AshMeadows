# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file has utility functions to analyze Ash Meadows secretive marshbird data

## This function retrieves the hour and minute of time of sunrise and sunset for a given date, time, coordinates and time zone
# df is a data.frame that contains a time variable (by default SamplingEventStartTm), longlat variables (default X_COORD,Y_COORD), a date variable (default SurveyDate), and location variables (default StudyArea, Point)
#     If the data were downloaded from the AKN using the downloader tool, the default values will work.
# timezone is a valid (i.e., POSIXct) timezone. By default, the code uses the timezone registered with the operating system. If you are running this in a time zone other than GMT-7 (Mountain time), then use timezone="MST"
# loc.fields, dat.field, tm.field, and pos.fields indicate the location, date, time and position of the survey point. Leave as-is if downloading from the AKN using bulk downloader.
get.sunrise.set<-function(df,timezone=Sys.timezone(),
		loc.fields=c("StudyArea","Point"),dat.field="SurveyDate",tm.field="SamplingEventStartTm",
		pos.fields=c("X_COORD","Y_COORD"),num.days=1){ 
	
	require(timeDate)
	require(maptools) 
	require(sp)
	
	#this needs to be long lat# 
	df[,tm.field]<-as.character(df[,tm.field])
	long.lat.dd<-as.data.frame(unique(df[,c(loc.fields,dat.field,tm.field,pos.fields)]))
	#these are UTMs so convert to decimal degrees 
	sp::coordinates(long.lat.dd)<-pos.fields
	proj4string(long.lat.dd) <- CRS("+proj=longlat +ellps=WGS84") 
	sunrised<-numeric()
	sunsetd<-numeric()
	for(ii in 1:(nrow(long.lat.dd))){
		rowdf<-as.data.frame(long.lat.dd[ii,dat.field])
		day <- as.POSIXct(rowdf[1,dat.field], tz=timezone)
		sequence <- seq(from=day, length.out=num.days , by="days") 
		rise <- sunriset(matrix(coordinates(long.lat.dd)[ii,],nrow=1),sequence, direction="sunrise", POSIXct=TRUE) 
		setd <- sunriset(matrix(coordinates(long.lat.dd)[ii,],nrow=1),sequence, direction="sunset", POSIXct=TRUE)
		sunrised<-c(sunrised,format(rise$time,"%H%M"))
		sunsetd<-c(sunsetd,format(setd$time,"%H%M"))
	}
	
	long.lat.dd$sunrise<-sunrised
	long.lat.dd$sunset<-sunsetd
	dff<-merge(df,long.lat.dd,by=c(loc.fields,dat.field,tm.field,pos.fields),all.x=TRUE)
	#we want to keep c(names(df),"sunrise","sunset","diff.rise.set"
	dff$start_time.c<-ifelse(nchar(dff[,tm.field])<4,paste("0",dff[,tm.field],sep=""),dff[,tm.field])
	dff$start_time.min<-(as.integer(substr(dff$start_time.c,1,2))*60)+(as.integer(substr(dff$start_time.c,4,5)))
	dff$sunrise.min<-(as.integer(substr(dff$sunrise,1,2))*60)+(as.integer(substr(dff$sunrise,3,4)))
	dff$sunset.min<-(as.integer(substr(dff$sunset,1,2))*60)+(as.integer(substr(dff$sunset,3,4)))
	dff$diff.rise.set<-ifelse(dff[,tm.field]<1200,
			dff$start_time.min-dff$sunrise.min,
			dff$start_time.min-dff$sunset.min)
	dff<-dff[,c(names(df),"sunrise","sunset","diff.rise.set")]
	return(dff) 
} 

## This function prepares the survey data for analysis with hierarchical imperfect detection models
# df is the data.frame of observations
# eff is the survey effort table
# occ is a boolean indicating if the data should be prepared for occupancy models (default) or abundance models
prepUDFdata<-function(df,eff,occ=T){
	#create the wide version per species of StudyArea-Transect-point-year-visit+ObservationCount+jday+SamplingEventStartTm
	#but first need the effort table...
	#HERE
	
	spp<-as.character(unique(df$BirdCd))
	spp<-subset(spp,spp != "BLRA")
	mrgfields<-c("StudyArea","Point","YearCollected","JulianDay","SurveyDate","SamplingEventStartTm")
	udflist<-list()
	for(ss in spp){
		spdf<-subset(df,BirdCd==ss)
		spdf<-aggregate(as.formula(paste("ObservationCount~",paste(mrgfields,collapse="+"),sep="")),data=spdf,FUN=sum)
		if(occ==TRUE){spdf$ObservationCount<-ifelse(spdf$ObservationCount>0,1,0)}
		#need to add the no-detection events
		spdf<-merge(eff,spdf,by=mrgfields,all.x=TRUE)
		spdf$BirdCd<-ss
		spdf$ObservationCount<-ifelse(is.na(spdf$ObservationCount),0,spdf$ObservationCount)
		if(ss=="SORA"){spdf<-subset(spdf,YearCollected<2016)}
		
		#calculate diffRS
		spdata<-get.sunrise.set(spdf)
		aggdata<-unique(spdata[,c("ObservationCount","JulianDay","diff.rise.set","StudyArea","Point","YearCollected","est.visit","EmMarsh","AlkMeadow","Area_emgMarsh","Area_alkMeadow")])
		widedf<-reshape(aggdata,idvar=c("StudyArea","Point","YearCollected","EmMarsh","AlkMeadow","Area_emgMarsh","Area_alkMeadow"),timevar="est.visit",direction="wide")
		odfind<-as.numeric(grep("ObservationCount",names(widedf)))
		covfields<-c(1:7)
		obsdf<-widedf[,c(covfields,odfind)]
		names(obsdf)<-gsub("ObservationCount","Visit",names(obsdf))
		jdyind<-as.numeric(grep("JulianDay",names(widedf)))
		jdaydf<-widedf[,jdyind]
		dssind<-as.numeric(grep("diff.rise.set",names(widedf)))
		dssdf<-widedf[,dssind]
		
		ydf<-obsdf[,grep("Visit",names(obsdf))]
		covsdf<-obsdf[,c("StudyArea","Point","YearCollected","EmMarsh","AlkMeadow","Area_emgMarsh","Area_alkMeadow")]
		ocovs<-list(JulianDay=jdaydf,diffRS=dssdf)
		res<-list(ydf=ydf,covsdf=covsdf,ocovs=ocovs)
		udflist[[ss]]<-res
	}
	return(udflist)
}

## This function calculates the mean and confidence intervals of prediction (occupance or abundance) estimates
# method refers to estimating via unique values (u) or the delta method (d)
# df is the data.frame of predictions
# cdf is the data.frame of prediction confidence limits
aggregateDensities<-function(df,cdf,method="u"){
	is.na(df) <- sapply(df, is.infinite)
	df<-cbind(df,cdf)
	if(method=="u"){
		ldf<-unique(df[,c("Predicted","upper","lower","YearCollected")])
	}else if(method=="d"){
		df$lgPredicted<-log(df$Predicted)
		df$lgupper<-log(df$upper)
		df$lglower<-log(df$lower)
		meandens<-aggregate(as.formula("lgPredicted~YearCollected"),data=df,FUN=mean)
		meanupper<-aggregate(as.formula("lgupper~YearCollected"),data=df,FUN=mean)
		meanlower<-aggregate(as.formula("lglower~YearCollected"),data=df,FUN=mean)
		ldf<-merge(meandens,meanupper,by="YearCollected")
		ldf<-merge(ldf,meanlower,by="YearCollected")
		ldf$Predicted<-exp(ldf$lgPredicted)
		ldf$upper<-exp(ldf$lgupper)
		ldf$lower<-exp(ldf$lglower)
	}
	ldf<-ldf[,c("YearCollected","Predicted","upper","lower")]
	return(ldf)
}

## This function calculates the geometric mean of the mean and confidence limits of predictions
# df is the data.frame of predictions
getGeometricMean<-function(df){
	df$lgPredicted<-log(df$Predicted)
	df$lgupper<-log(df$upper)
	df$lglower<-log(df$lower)
	pdf<-aggregate(as.formula("lgPredicted~YearCollected"),data=df,FUN=mean)
	udf<-aggregate(as.formula("lgupper~YearCollected"),data=df,FUN=mean)
	ldf<-aggregate(as.formula("lglower~YearCollected"),data=df,FUN=mean)
	pdf$Predicted<-exp(pdf$lgPredicted)
	udf$upper<-exp(udf$lgupper)
	ldf$lower<-exp(ldf$lglower)
	rdf<-merge(pdf,udf,by="YearCollected");rdf<-merge(rdf,ldf,by="YearCollected")
	rdf<-rdf[,c("YearCollected","Predicted","upper","lower")]
	return(rdf)
}

## This function returns the number of detections by species and year
# df is the data.frame of predictions
# eff is the effort table
# fct is the aggregating function (by default the mean)
getNumDetectionsbyYear<-function(df,eff,fct="mean"){
	spp<-unique(df$BirdCd)
	res<-data.frame()
	for(ss in spp){
		tdf<-subset(df, BirdCd==ss)
		mdf<-merge(eff,tdf,by=c("SamplingUnitId","YearCollected","JulianDay","SamplingEventStartTm"),all.x=T)
		mdf$BirdCd<-ss;mdf$ObservationCount<-ifelse(is.na(mdf$ObservationCount),0,mdf$ObservationCount)
		madf<-aggregate(ObservationCount~BirdCd+SamplingUnitId+YearCollected,data=mdf,FUN=max,na.rm=T)
		yadf<-aggregate(ObservationCount~BirdCd+YearCollected,data=madf,FUN=fct,na.rm=T)
		res<-rbind(res,yadf)
	}
	return(res)
}



