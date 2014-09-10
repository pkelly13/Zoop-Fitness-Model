#Script will determine DVM strategy for empirical data using thermocline depth as the cutoff for whether they are in the epi or in the hypo
#10 September 2014

#load zoop sample data and zoop log data sheet
setwd('~/Documents/Notre Dame/UNDERC 2013/zoopData 2013/data analyses')

source('2013DVMcharact_10Feb2014.R')
#remove Crampton
dvmZoops<-dvmZoops[dvmZoops$Lake.ID!='CR',]

#make vector of lakeIDs
lakes<-unique(dvmZoops$Lake.ID)

#load tCline data
setwd('~/Zoop-Fitness-Model')
source('thermocline.calc.R')

#fix dates in dvmZoops
dvmZoops$Sample.date=format(as.Date(dvmZoops$Sample.date,'%m/%d/%y'),'%m/%d/%y')

#add sample point to dvmZoops data
dvmZoops$jDate<-strptime(dvmZoops$Sample.date,'%m/%d/%y')$yday+1
#sample point 1:day 139-151, sample point 2:153-165, sample point 3:>165
sample.point<-c()
for(i in 1:nrow(dvmZoops)){
	if(dvmZoops$jDate[i]>=139 & dvmZoops$jDate[i]<=151){
		sample.point[i]=1
	}
	if(dvmZoops$jDate[i]>151 & dvmZoops$jDate[i]<=165){
		sample.point[i]=2
	}
	if(dvmZoops$jDate[i]>165){
		sample.point[i]=3
	}
}

dvmZoops$sample.point=sample.point

#make unique ID of lake, taxa, sample point, and depth
dvmZoops$uniqueID<-paste(dvmZoops$Lake.ID, dvmZoops$Taxa, dvmZoops$sample.point,dvmZoops$Depth.Top..m.,sep='_')

#separate into day and night data frames
dayZoops<-dvmZoops[dvmZoops$TOD=='Day',]
nightZoops<-dvmZoops[dvmZoops$TOD=='Night',]

#match unique IDs from day zoops and night zoops
nightCount<-c()
for(i in 1:nrow(dayZoops)){
	rowi=match(dayZoops$uniqueID[i],nightZoops$uniqueID)
	nightCount[i]=nightZoops$Counts[rowi]
}
dayZoops$nightCount<-nightCount #add nightCount to dayZoops data

#go through each unique ID, find the max abundance depth and make new data frame of max abundance depth for day and night
#first make new uniqueID - lake, taxa, sample point
dayZoops$uniqueID<-paste(dayZoops$Lake.ID,dayZoops$Taxa,dayZoops$sample.point,sep='_')
#make vector of all uniqueIDs
uniques<-unique(dayZoops$uniqueID)

max.abundance<-c()
for(i in 1:length(uniques)){ #loop through each unique ID and find maximum abundance of zooplankton depth and make into new data frame
	lakei<-dayZoops[dayZoops$uniqueID==uniques[i],]
	day.max.depth<-lakei$Depth.Top..m.[lakei$Counts==max(lakei$Counts)]
	day.max.depth<-day.max.depth[!is.na(day.max.depth)]
	day.max.depth<-day.max.depth[1]
	night.max.depth<-lakei$Depth.Top..m.[lakei$nightCount==max(lakei$nightCount,na.rm=T)]
	night.max.depth<-night.max.depth[!is.na(night.max.depth)]
	night.max.depth=night.max.depth[1]
	x<-data.frame(lakeID=lakei$Lake.ID[1],Sample.date=lakei$Sample.date[1],taxa=lakei$Taxa[1],sample.point=lakei$sample.point[1],day.max.depth=day.max.depth,night.max.depth=night.max.depth)
	max.abundance<-rbind(max.abundance,x)
}

#match max.abundance and tCline data
#need to make new uniqueID
max.abundance$uniqueID<-paste(max.abundance$lakeID,max.abundance$Sample.date,sep='_')
#match unique ID in max.abundance and tCline
metaDepth<-c()
bottomDepth<-c()
for(i in 1:nrow(max.abundance)){
	rowi<-match(max.abundance$uniqueID[i],tCline$uniqueID)
	metaDepth[i]<-tCline$middleDepth[rowi]
	bottomDepth[i]<-tCline$bottomDepth[rowi]
}
max.abundance$metaDepth<-metaDepth #add to max abundance data
max.abundance$bottomDepth<-bottomDepth

#need to characterize DVM strategy based on where they are at night and during the day in relation to the bottom of the thermocline
#remove the NAs first
max.abundance<-max.abundance[!is.na(max.abundance$night.max.depth),]
strategy<-c()
for(i in 1:nrow(max.abundance)){
	if(max.abundance$day.max.depth[i]<=max.abundance$metaDepth[i] & max.abundance$night.max.depth[i]>=max.abundance$metaDepth[i]){
		strategy[i]<-'normal'
	}
	if(max.abundance$day.max.depth[i]>=max.abundance$metaDepth[i] & max.abundance$night.max.depth[i]<=max.abundance$metaDepth[i]){
		strategy[i]<-'reverse'
	}
	if(max.abundance$day.max.depth[i]<=max.abundance$metaDepth[i] & max.abundance$night.max.depth[i]<=max.abundance$metaDepth[i]){
		strategy[i]<-'bottom'
	}
	if(max.abundance$day.max.depth[i]>=max.abundance$metaDepth[i] & max.abundance$night.max.depth[i]>=max.abundance$metaDepth[i]){
		strategy[i]<-'noDVM'
	}
}
max.abundance$strategy<-strategy

#write data to a csv file
setwd('~/Zoop-Fitness-Model')
write.csv(max.abundance,'DVMstrategy.empirical.csv')

