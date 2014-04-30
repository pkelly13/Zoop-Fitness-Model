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

#Calculate mgC/L for use in EFA calculations
cn$mgC_L<-cn$Cmg/(as.numeric(cn$volFiltered)/1000)

#add depthClass
depthClass=rep(NA,nrow(cn)) #make vector of NAs that is the length of cn data
depthClass[grep('0',cn$depth)]='PML' #fill any depth thta contains a 0 with PML
depthClass[is.na(depthClass)]='Hypo' #all others make Hypo

cn$depthClass=depthClass #add to cn data

#load EFA data
setwd('~/Documents/Notre Dame/lipids/final data/2013 survey')
faData<-read.csv('sestonFAcategorized_8April2014.csv')

#match FA data with the nearest data point for PC and mgC_L
#remove EL samples from faData
faData<-faData[faData$lakeID!='EL',]
faData<-faData[faData$depthClass!='Meta',]
#remove weird 1000 mL samples - not sure what these are from
cn<-cn[as.numeric(cn$volFiltered)<1000,]
cn<-cn[!is.na(cn$mgC_L),]

#for loop that will match nearest mgC_L and P:C data to EFA data - do not have all dates represented, so will need to match to the closest date available to get the best estimate of resource quality possible
resourceQual<-c()
for(i in 1:nrow(faData)){
	samplei=faData[i,]
	cni=cn[cn$lakeID==samplei$lakeID & cn$depthClass==samplei$depthClass,]
	cn.noNA=cni[!is.na(cni$Pmg_Cmg),]
	if(nrow(cni)>=1){
	mgC_L=cni$mgC_L[which(abs(as.Date(cni$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(cni$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	}
	if(nrow(cni)<1){
		mgC_L=NA
	}
	if(nrow(cn.noNA)>=1){
	mgP_mgC=cn.noNA$Pmg_Cmg[which(abs(as.Date(cn.noNA$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(cn.noNA$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	}
	if(nrow(cn.noNA)<1){
		mgP_mgC=NA		
	}
	samplei$mgC_L=mgC_L
	samplei$mgP_mgC=mgP_mgC
	resourceQual=rbind(samplei,resourceQual)
}

resourceQual$mgEPA_mgC<-(resourceQual$EPA_ugL/1000)/resourceQual$mgC_L
resourceQual$mgDHA_mgC<-(resourceQual$DHA_ugL/1000)/resourceQual$mgC_L
resourceQual$C18PUFA_mgC<-(resourceQual$C18PUFA_ugL/1000)/resourceQual$mgC_L

#seperate only the data that I need - cols 4,6,8,22,23,24,25,26
resourceQual<-resourceQual[,c(4,6,8,22:26)]

#write resource quality data to R files folder
setwd('~/Documents/Notre Dame/UNDERC 2013/zoop growth model/R files')
write.csv(resourceQual,'resourceQuality_growth model.csv')

#Add chaoborus density to cn data
#make unique IDs for cn and chaobDens data that correspond to bring in resource quality data into 
chaobDens<-read.csv('2013chaoborusDensities.csv')

#aggregate resource quality data by lake date and depth
x=aggregate(resourceQual$mgC_L,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(x)=c('lakeID','dateSample','depthClass','mgC_L')
y=aggregate(resourceQual$mgP_mgC,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(y)=c('lakeID','dateSample','depthClass','mgP_mgC')
z=aggregate(resourceQual$mgEPA_mgC,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(z)=c('lakeID','dateSample','depthClass','mgEPA_mgC')
a=aggregate(resourceQual$mgDHA_mgC,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(a)=c('lakeID','dateSample','depthClass','mgDHA_mgC')
b=aggregate(resourceQual$C18PUFA_mgC,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(b)=c('lakeID','dateSample','depthClass','C18PUFA_mgC')
c=aggregate(resourceQual$mgC_L,by=list(resourceQual$lakeID,resourceQual$dateSample,resourceQual$depthClass),mean,na.rm=T)
colnames(c)=c('lakeID','dateSample','depthClass','mgC_L')

resourceQual=cbind(x,mgC_L=c[,4],mgP_mgC=y[,4],mgEPA_mgC=z[,4],mgDHA_mgC=a[,4],C18PUFA_mgC=b[,4])

#remove CR from chaob data
chaobDens<-chaobDens[chaobDens$lakeID!='CR',]

#match chaoborus data with resource qual data from the closest date similar to how resource quality data was matched
modelParms<-c()
for(i in 1:nrow(chaobDens)){
	samplei=chaobDens[i,]
	RQi=resourceQual[resourceQual$lakeID==samplei$lakeID & resourceQual$depthClass==samplei$depthClass,]
	mgC_L=RQi$mgC_L[which(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	epa=RQi$mgEPA_mgC[which(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	dha=RQi$mgDHA_mgC[which(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	PC=RQi$mgP_mgC[which(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	C18=RQi$C18PUFA_mgC[which(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y')) == min(abs(as.Date(RQi$dateSample,'%m/%d/%y')-as.Date(samplei$dateSample,'%m/%d/%y'))))]
	samplei$mgC_L=mgC_L
	samplei$EPA_mgC=epa
	samplei$DHA_mgC=dha
	samplei$PC=PC
	samplei$C18_mgC=C18
	modelParms=rbind(modelParms,samplei)
}

