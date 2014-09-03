#script calculates CPUE for each lake in DVM survey using minnow trap and fyke net data collected by Nikki and monitoring
#Patrick Kelly, 1 Sep 2014

#load minnow trap 2013 data
setwd('~/Zoop-Fitness-Model/data/fish 2013/csv')
minnow2013<-read.csv('fishLog2013_SurveyLakes.csv')

#make vector of dvm lakes to pull data from
dvmLakes<-c('BA','BR','WL','RB','TU','DW','CB','CB','HB','RS','RE')

#pull lakes from that dataset
minnow2013<-minnow2013[minnow2013$Lake_ID %in% dvmLakes,]

#make a vector of fish species to pull data fram
species<-c('BSB','BCP','BLG','FHM','PKS','RBD','SHI','SMB','YWP')
minnow2013<-minnow2013[minnow2013$Species %in% species,]

#combine lakeID and date sample to get unique IDs
minnow2013$uniqueID<-paste(minnow2013$Lake_ID,minnow2013$datesample,sep='_')
uniques<-unique(minnow2013$uniqueID) #make vector of unique uniqueIDs

CPUE_minnow2013<-c()
for(i in 1:length(uniques)){
	samplei<-minnow2013[minnow2013$uniqueID==uniques[i],]
	catch<-nrow(samplei)
	effort<-mean(samplei$Effort)
	x<-data.frame(lakeID=samplei$Lake_ID[1],dateSample=samplei$datesample[1],CPUE=catch/effort)
	CPUE_minnow2013<-rbind(CPUE_minnow2013,x)
}

#load minnow trap 2014 data
setwd('~/Zoop-Fitness-Model/data/fish 2014/csv')
minnow2014<-read.csv('Monitoring.Minnow.2014.csv')

minnow2014<-minnow2014[minnow2014$Lake_ID %in% dvmLakes,] #use only survey lakes
minnow2014<-minnow2014[minnow2014$Species %in% species,] #use only planktivores
minnow2014$uniqueID<-paste(minnow2014$Lake_ID,minnow2014$datesample,sep='_') #make unique IDs
uniques<-unique(minnow2014$uniqueID)

CPUE_minnow2014<-c() #do the same as above for 2014 data
for(i in 1:length(uniques)){
	samplei<-minnow2014[minnow2014$uniqueID==uniques[i],]
	catch<-nrow(samplei)
	effort<-mean(samplei$Effort)
	x<-data.frame(lakeID=samplei$Lake_ID[1],dateSample=samplei$datesample[1],CPUE=catch/effort)
	CPUE_minnow2014<-rbind(CPUE_minnow2014,x)
}

#load fyke data
setwd('~/Zoop-Fitness-Model/data/fish 2013/csv')
fyke2013<-read.csv('gonadSurvey2013.csv')

#use only survey lakes
fyke2013<-fyke2013[fyke2013$Lake_ID %in% dvmLakes,]
fyke2013$uniqueID<-paste(fyke2013$Lake_ID,fyke2013$datesample,sep='_')
uniques<-unique(fyke2013$uniqueID)

CPUE_fyke2013<-c() #Do the same as above, except for fyke data.  Will combine this with minnow trap data for an estimate of fish abundance - fills in some empty lakes
for(i in 1:length(uniques)){
	samplei<-fyke2013[fyke2013$uniqueID==uniques[i],]
	catch<-nrow(samplei)
	effort<-mean(samplei$Effort)
	x<-data.frame(lakeID=samplei$Lake_ID[1],dateSample=samplei$datesample[1],CPUE=catch/effort)
	CPUE_fyke2013<-rbind(CPUE_fyke2013,x)
}

#Average all of this data to get to one number for each lake
CPUE_minnow2013<-tapply(CPUE_minnow2013$CPUE,CPUE_minnow2013$lakeID,mean,na.rm=T)
CPUE_minnow2013<-CPUE_minnow2013[!is.na(CPUE_minnow2013)]
CPUE_minnow2013<-data.frame(lakeID=rownames(CPUE_minnow2013),CPUE=CPUE_minnow2013)

CPUE_minnow2014<-tapply(CPUE_minnow2014$CPUE,CPUE_minnow2014$lakeID,mean,na.rm=T)
CPUE_minnow2014<-data.frame(lakeID=rownames(CPUE_minnow2014[!is.na(CPUE_minnow2014)]),CPUE=CPUE_minnow2014[!is.na(CPUE_minnow2014)])

CPUE_fyke2013<-tapply(CPUE_fyke2013$CPUE,CPUE_fyke2013$lakeID,mean,na.rm=T)
CPUE_fyke2013<-data.frame(lakeID=rownames(CPUE_fyke2013[!is.na(CPUE_fyke2013)]),CPUE=CPUE_fyke2013[!is.na(CPUE_fyke2013)])

CPUE<-rbind(CPUE_minnow2013,CPUE_minnow2014,CPUE_fyke2013)
CPUE<-tapply(CPUE$CPUE,CPUE$lakeID,mean,na.rm=T)

CPUE<-c(CPUE[1],CPUE[2],CB=0,CPUE[5],CPUE[3],CPUE[1],RE=0,CPUE[6],CPUE[4],WL=0)
lakes<-c('BA','BR','CB','DW','HB','RB','RE','RS','TU','WL')
CPUE<-data.frame(lakeID=lakes,CPUE=CPUE) #final data frame of CPUE data
#save this data as csv file

setwd('~/Zoop-Fitness-Model')

write.csv(CPUE,'fishCPUE_all.csv')