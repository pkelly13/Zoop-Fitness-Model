#This script combines resource quality data into one data frame to be used for growth calculations

#Script adds P data from Ali and Stephen into CN data from the EA
#script also adds sample information to phosphorus data from isotope log, i.e. lakeID, dateSample, depths, etc.
#PTK 29 April 2014

#load CN data
setwd('~/Documents/Notre Dame/CEST Costech EA data/2013 survey/R files')
cn<-read.csv('EAdataAllData_P-0605_P-0963_FINAL.csv')

#load P data
setwd('~/Documents/Notre Dame/UNDERC 2013/particulate P data')
p<-read.csv('ParticulateP_UNDERC2013.csv')

#match sample IDs and add P data to CN data
pData<-c()
for(i in 1:nrow(cn)){
	rowi=match(cn$sample[i],p$sample)
	pData[i]=p$Pconcentration_ug[rowi]
}
cn$P_ug<-pData

#load log data
setwd('~/Documents/Notre Dame/UNDERC 2013/CP data')
log<-read.csv('IsotopeLog2013.csv')

#split date.time into seperate date and time vectors
date<-c()
time<-c()
for(i in 1:nrow(log)){
	dateTime=strsplit(log$Date.time[i],' ')
	date[i]=dateTime[[1]][1]
	time[i]=dateTime[[1]][2]
}
log$dateSample=date
log$time=time

#match cn samples with log information and add columns to cn data
lakeID<-c()
dateSample<-c()
depth<-c()
volFiltered<-c()
for(i in 1:nrow(cn)){
	rowi=match(cn$sample[i],log$Isotope.ID)
	lakeID[i]=log$Lake.ID[rowi]
	dateSample[i]=log$dateSample[rowi]
	depth[i]=log$Depth..m.[rowi]
	volFiltered[i]=log$Volume.Filtered..mL.[rowi]
}
cn$lakeID=lakeID
cn$dateSample=dateSample
cn$depth=depth
cn$volFiltered=volFiltered

#Calculate mgP/mgC
cn$Pmg_Cmg<-(cn$P_ug/1000)/cn$Cmg #mg P per mg C

#add depthClass
depthClass=rep(NA,nrow(cn)) #make vector of NAs that is the length of cn data
depthClass[grep('0',cn$depth)]='PML' #fill any depth thta contains a 0 with PML
depthClass[is.na(depthClass)]='Hypo' #all others make Hypo

cn$depthClass=depthClass #add to cn data

#load EFA data
setwd('~/Documents/Notre Dame/lipids/final data/2013 survey')
faData<-read.csv('sestonFAcategorized_8April2014.csv')

#make unique ID for both cn and FA data
faData$uniqueID<-paste(faData$lakeID,faData$dateSample,faData$depthClass,sep='.')
cn$uniqueID<-paste(cn$lakeID,cn$dateSample,cn$depthClass,sep='.')

#match cn data and fa data by unique IDs, add FA data to CN data frame
#+only need EPA and DHA data <- in ug/L
epa<-c()
dha<-c()
c18<-c()
for(i in 1:nrow(cn)){
	rowi=match(cn$uniqueID[i],faData$uniqueID)
	epa[i]=faData$EPA_ugL[rowi]
	dha[i]=faData$DHA_ugL[rowi]
	c18[i]=faData$C18PUFA_ugL[rowi]
}
cn$EPA_ugL=epa
cn$DHA_ugL=dha
cn$C18PUFA_ugL=c18

#calculate all of these in mg FA/mg C
mgC_L<-cn$Cmg/(as.numeric(cn$volFiltered)/1000)

cn$mgEPA_mgC<-(cn$EPA_ugL/1000)/mgC_L
cn$mgDHA_mgC<-(cn$DHA_ugL/1000)/mgC_L
cn$C18PUFA_mg<-(cn$C18PUFA_ugL/1000)/mgC_L
#write resource quality data to R files folder
setwd('~/Documents/Notre Dame/UNDERC 2013/zoop growth model/R files')
#write.csv(cn,'resourceQuality_growth model.csv')

#Add chaoborus density to cn data
#make unique IDs for cn and chaobDens data that correspond to bring in resource quality data into 
chaobDens<-read.csv('2013chaoborusDensities.csv')

#match chaoborus data with resource qual data from the closest date
for(i in nrow(chaobDens)){
	samplei=chaobDens[i,]
	cni=cn[cn$lakeID==samplei$lakeID & cn$depthClass==samplei$depthClass,]
	
}