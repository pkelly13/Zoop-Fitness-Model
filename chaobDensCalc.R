#Script determines depth specific chaoborus densities for use in predation  risk model
#PTK 30 April 2014

#load tot.chaob data
setwd('~/Documents/Notre Dame/UNDERC 2013/zoopData 2013/R files')
tot.chaob<-read.csv('tot.chaob.csv')
tot.chaob$uniqueID<-paste(tot.chaob$lakeID,tot.chaob$dateSample,tot.chaob$TOD,sep='_')

#make vector of unique IDs
id<-unique(tot.chaob$uniqueID)
chaobDens<-c()
for(i in 1:length(id)){
	samplei=tot.chaob[tot.chaob$uniqueID==id[i],]
	samplei=samplei[order(samplei$depthTop),]
	rowi=match(samplei$lakeID,lakeParms$lakeID)[1]
	pmlDepth=lakeParms$PMLdepth[rowi]
	pmlDepth=ceiling(pmlDepth)
	pmlDens=sum(samplei$tot.counts[samplei$depthTop<=pmlDepth])
	hypoDens=sum(samplei$tot.counts[samplei$depthTop>pmlDepth & samplei$depthTop<=pmlDepth+5])
	x=data.frame(lakeID=rep(samplei$lakeID[1],2),dateSample=rep(samplei$dateSample[1],2),time=rep(samplei$time[1],2),TOD=rep(samplei$TOD[1],2),depthClass=c('PML','Hypo'),counts=c(pmlDens,hypoDens))
	chaobDens=rbind(chaobDens,x)
}

#calculate chaob density in ind_m3
chaobDens$ind_m3<-(chaobDens$counts/15)*1000

#write data to R files folder
write.csv(chaobDens,'2013chaoborusDensities.csv')
