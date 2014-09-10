#Script that determines DVM pattern from empirical data - find depth of max density of specific taxa - compare to whether it is above or below the thermocline
#10 September 2014 Patrick Kelly

#load zoop sample data and zoop log data sheet
setwd('~/Documents/Notre Dame/UNDERC 2013/zoopData 2013/data analyses')

source('2013DVMcharact_10Feb2014.R')
#remove Crampton
dvmZoops<-dvmZoops[dvmZoops$Lake.ID!='CR',]

#make vector of lakeIDs
lakes<-unique(dvmZoops$Lake.ID)

#load profile data
profs<-dbGetQuery(con,'SELECT profs.lakeID,profs.dateSample,profs.dateTimeSample,profs.depthTop,profs.temp,profs.DOmgL FROM LIMNO_PROFILES AS profs')
#addYear
profs<-addYear(profs)
profs$dateSample=format(as.Date(profs$dateSample,'%Y-%m-%d %H:%M:%S'),format='%m/%d/%y')

profs<-profs[profs$year==2013,] #use only 2013 data
profs<-profs[profs$depthTop!=0.25 & profs$depthTop!=0.75,] #remove depths with no DO or temp data
profs<-profs[profs$lakeID %in% lakes,]

#make unique ID of lake-date
profs$uniqueID<-paste(profs$lakeID,profs$dateSample,sep='_')

#make vector of uniqueIDs
uniques<-unique(profs$uniqueID)
#remove early HB sample
uniques<-uniques[-50]

#make a data frame of profile lakes, dates, and unique IDs, and find where the middle of the thermocline is 
tCline<-c()
for(i in 1:length(uniques)){ #cycle through each profile we have
	lakei<-profs[profs$uniqueID==uniques[i],] #use one lake at a time
	lakei<-lakei[order(lakei$depthTop),]
	if(nrow(lakei)<5){ #one of the lakes only has a one data point for some reason - ignore that
		i=i+1
	}
	else{
	top<-c()
	for(j in 2:nrow(lakei)){
		if(lakei$temp[j-1]-lakei$temp[j]>=1){ #standard way to get PML depth
			x=lakei$depthTop[j-1]
			top=c(top,x)
		}
	}
	top=top[1]
	if(is.null(top)){ #early season data may not have strong thermocline data - relax the assumption a bit
		for(k in 2:nrow(lakei)){
			if(lakei$temp[k-1]-lakei$temp[k]>=0.6){
				x=lakei$depthTop[k-1]
				top=c(top,x)
			}
		}
		top=top[1]
	}
	bottom<-c()
	for(l in nrow(lakei):2){ #for the bottom of the thermocline, go backwards from the deepest to the shallowest and find degree change
		if(lakei$temp[l-1]-lakei$temp[l]>=1){
			x=lakei$depthTop[l-1]
			bottom=c(bottom,x)
		}
	}
	bottom=bottom[1]
	if(is.null(bottom)){
		for(m in nrow(lakei):2){ #again, relax the assumption when the thermocline is not sharp
			if(lakei$temp[m-1]-lakei$temp[m]>=0.4){
				x=lakei$depthTop[m-1]
				bottom=c(bottom,x)
			}
		}
		bottom=bottom[1]
	}
	}
	avg<-mean(c(top,bottom))
	together<-data.frame(lakeID=lakei$lakeID[1],dateSample=lakei$dateSample[1],uniqueID=lakei$uniqueID[1],middleDepth=avg)
	tCline<-rbind(tCline,together) #these were all checked after the fact to make sure no bottom depth was shallow then the top depth, and that they did not equal each other
}