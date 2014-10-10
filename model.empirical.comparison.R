#make a script that will first add taxa.2 column with calanoid or cyclopoid, then change the taxa column so that calanoids and cyclopoids are copepods, then match empirical strategy and modeled strategy, make a column of 1 or 0 for where they match or do not, then find out how many were predicted - need to figure out what stats to run on that.

#load empirical data
setwd('~/Zoop-Fitness-Model')
dvm.emp<-read.csv('DVMstrategy.empirical.csv')

#load optimal strategy
dvm.opt<-read.csv('optimalDVMpatterns.csv')

#need to add sample point info to dvm.opt to match to empirical data

dvm.opt$jDate<-strptime(dvm.opt$date,'%m/%d/%y')$yday+1
#sample point 1:day 139-151, sample point 2:153-165, sample point 3:>165
#first remove WL with no date
dvm.opt<-dvm.opt[!is.na(dvm.opt$date),]
sample.point<-c()
for(i in 1:nrow(dvm.opt)){
	if(dvm.opt$jDate[i]>=139 & dvm.opt$jDate[i]<=151){
		sample.point[i]=1
	}
	if(dvm.opt$jDate[i]>151 & dvm.opt$jDate[i]<=165){
		sample.point[i]=2
	}
	if(dvm.opt$jDate[i]>165){
		sample.point[i]=3
	}
}

dvm.opt$sample.point=sample.point

#need only calanoid, cyclopoid, daphnia, and holopedium data
dvm.emp<-dvm.emp[dvm.emp$taxa=='cyclpoids' | dvm.emp$taxa=='calanoids' | dvm.emp$taxa=='daphnia' | dvm.emp$taxa=='holopedium',]

#change dvm.emp data so that calanoids and cyclopoids are just copepod, then make another column 'taxa2' where they are calanoid or cycopoid

taxa.2<-c()
for(i in 1:nrow(dvm.emp)){
	if(dvm.emp$taxa[i]=='cyclopoids' | dvm.emp$taxa[i]=='calanoids'){
		taxa.2[i]<-'copepod'
	}
	if(dvm.emp$taxa[i]=='daphnia'){
		taxa.2[i]<-'daphnia'
	}
	if(dvm.emp$taxa[i]=='holopedium'){
		taxa.2[i]<-'holopedium'
	}
}
dvm.emp$taxa.2<-taxa.2

#make unique ID for both dvm.emp and dvm.opt - lake, sample point, taxa
dvm.emp$uniqueID<-paste(dvm.emp$lakeID,dvm.emp$taxa.2,dvm.emp$sample.point,sep='_')
dvm.opt$uniqueID<-paste(dvm.opt$lakeID,dvm.opt$taxa,dvm.opt$sample.point,sep='_')

#match optimal strategy to empirical strategy and add to emp data
opt<-c()
for(i in 1:nrow(dvm.emp)){
	rowi<-match(dvm.emp$uniqueID[i],dvm.opt$uniqueID)
	opt[i]<-dvm.opt$optimal[rowi]
}
dvm.emp$optimal.strategy<-opt

#make a column of 1s and 0s for when its right and when its wrong
correct<-c()
for(i in 1:nrow(dvm.emp)){
	if(is.na(dvm.emp$optimal.strategy[i])){
		correct[i]=0
	}
	if(!is.na(dvm.emp$optimal.strategy[i]) & dvm.emp$optimal.strategy[i]==dvm.emp$strategy[i]){
		correct[i]=1
	}
	if(!is.na(dvm.emp$optimal.strategy[i]) & dvm.emp$optimal.strategy[i]!=dvm.emp$strategy[i]){
		correct[i]=0
	}
}

#add correct to dvm.emp
dvm.emp$correct<-correct

#percentage total right
sum(dvm.emp$correct[!is.na(dvm.emp$optimal.strategy)])/length(dvm.emp$correct[!is.na(dvm.emp$optimal.strategy)]) #48%
#% right for daphnia
sum(dvm.emp$correct[dvm.emp$taxa=='daphnia' & !is.na(dvm.emp$optimal.strategy)])/length(dvm.emp$correct[dvm.emp$taxa=='daphnia' & !is.na(dvm.emp$optimal.strategy)]) #52%
#% right for holopedium
sum(dvm.emp$correct[dvm.emp$taxa=='holopedium' & !is.na(dvm.emp$optimal.strategy)])/length(dvm.emp$correct[dvm.emp$taxa=='holopedium' & !is.na(dvm.emp$optimal.strategy)]) #44%
#% right for copepods
sum(dvm.emp$correct[dvm.emp$taxa.2=='copepod' & !is.na(dvm.emp$optimal.strategy)])/length(dvm.emp$correct[dvm.emp$taxa.2=='copepod' & !is.na(dvm.emp$optimal.strategy)]) #48%