#Cross lake fitness analysis using sensitivity analysis parameters
#Patrick Kelly 15 July 2014

#Use same script as regular calculation, but with sensitivity analysis parameters instead of regular ones

#load model parameter data
setwd('~/Zoop-Fitness-Model')
modelParms<-read.csv('fitnessModel_parameters_sensAnalysis.csv')
#get rid of unused columns
modelParms<-modelParms[,-c(1:3)]

#add mu/g - need to exp(growth) to handle 0s and negative numbers
modelParms$mu.gEst<-exp(modelParms$muEst)/exp(modelParms$growth)
modelParms$mu.g0<-exp(modelParms$mu0)/exp(modelParms$growth)
modelParms$mu.g0.01<-exp(modelParms$mu0.01)/exp(modelParms$growth)
modelParms$mu.g0.1<-exp(modelParms$mu0.1)/exp(modelParms$growth)
modelParms$mu.g0.25<-exp(modelParms$mu0.25)/exp(modelParms$growth)
modelParms$mu.g0.5<-exp(modelParms$mu0.5)/exp(modelParms$growth)
modelParms$mu.g1<-exp(modelParms$mu1)/exp(modelParms$growth)
modelParms$mu.g5<-exp(modelParms$mu5)/exp(modelParms$growth)


#Go through each lake and calculate fitness comparisons for each situation for the last date
#convert dates into matching format
modelParms$dateSample<-format(as.Date(modelParms$dateSample,'%m/%d/%y'),'%m/%d/%y')

#remove NAs from data
modelParms<-modelParms[!is.na(modelParms$EPA_mgC),]
#remove bad dates
modelParms<-modelParms[modelParms$dateSample!='05/22/12',]

#make vector of lakes
lakes<-unique(modelParms$lakeID)


fitnessTest<-c()

for(i in 1:9){
lakei=modelParms[modelParms$lakeID==lakes[i],]
night.dates=unique(lakei$dateSample[lakei$TOD=='Night']) #same for nights
#use day data from closest date
night.dates=night.dates[order(night.dates)]
x=c()
for(j in 1:length(night.dates)){
night.data=lakei[lakei$TOD=='Night' & lakei$dateSample==night.dates[j],]
lakei.day<-lakei[lakei$TOD=='Day',]
day.data<-lakei.day[which(abs(as.Date(lakei$dateSample[lakei$TOD=='Day' ],'%m/%d/%y')-as.Date(night.dates[j],'%m/%d/%y'))==min(abs(as.Date(lakei$dateSample[lakei$TOD=='Day'],'%m/%d/%y')-as.Date(night.dates[j],'%m/%d/%y')))),]
tot.data<-rbind(night.data,day.data)
day.diff<-abs(as.Date(day.data$dateSample[1],'%m/%d/%y')-as.Date(night.dates[j],'%m/%d/%y'))
if(nrow(tot.data)<4){
	j=j+1
}
sens<-c()
	for(g in 35:42){
if(nrow(tot.data)==4){
	
normal=mean(c(tot.data[tot.data$depthClass=='Hypo' & tot.data$TOD=='Day',g],tot.data[tot.data$depthClass=='PML' & tot.data$TOD=='Night',g]),na.rm=T) #calculate fitness for normal DVM
reverse=mean(c(tot.data[tot.data$depthClass=='Hypo' & tot.data$TOD=='Night',g],tot.data[tot.data$depthClass=='PML' & tot.data$TOD=='Day',g]),na.rm=T)
noDVM=mean(c(tot.data[tot.data$depthClass=='PML' & tot.data$TOD=='Day',g],tot.data[tot.data$depthClass=='PML' & tot.data$TOD=='Night',g]),na.rm=T) #calculate fitness for noDVM
bottom=mean(c(tot.data[tot.data$depthClass=='Hypo' & tot.data$TOD=='Day',g],tot.data[tot.data$depthClass=='Hypo' & tot.data$TOD=='Night',g]),na.rm=T) #calculate fitness for daphnia sitting on the bottom
z=c(normal,reverse,noDVM,bottom) #make into vector
if(min(c(normal,reverse,noDVM,bottom))==z[1]){
	optimal='normal'
} #find minimum --> optimal strategy
if(min(c(normal,reverse,noDVM,bottom))==z[2]){
	optimal='reverse'
}
if(min(c(normal,reverse,noDVM,bottom))==z[3]){
	optimal='noDVM'
}
if(min(c(normal,reverse,noDVM,bottom))==z[4]){
	optimal='bottom'
}

y=data.frame(lakeID=lakes[i],date=night.dates[j],mu.g=colnames(tot.data[g]),normal=normal,reverse=reverse,noDVM=noDVM,bottom=bottom,optimal=optimal)
sens=rbind(sens,y)
}
}
x=rbind(x,sens)
}
fitnessTest=rbind(fitnessTest,x) #add to fitness
}

#get rid of day differences of longer than 4 days
fitness<-fitnessTest[fitnessTest$dayDifference<4,]