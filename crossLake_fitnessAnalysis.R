#data exploration for zooplankton fitness
#PTK 6 May 2014
#load model parameter data
setwd('~/Zoop-Fitness-Model')
modelParms<-read.csv('fitnessModel_parameters.csv')
#get rid of unused columns
modelParms<-modelParms[,-c(1:3)]

#add mu/g - need to exp(growth) to handle 0s and negative numbers
modelParms$mu.g<-modelParms$mu/exp(modelParms$growth)

#Go through each lake and calculate fitness comparisons for each situation for the last date
#convert dates into matching format
modelParms$dateSample<-format(as.Date(modelParms$dateSample,'%m/%d/%y'),'%m/%d/%y')

fitness<-c()
i=10
lake=lakes[i]
lakei=modelParms[modelParms$lakeID==lake,]
day.date='07/30/13' #find day dates that correspond to plots that actually have data
night.date='07/30/13' #same for nights
lakej.day=lakei[lakei$TOD=='Day' & lakei$dateSample==day.date,]
lakej.night=lakei[lakei$TOD=='Night' & lakei$dateSample==night.date,]
normal=mean(c(lakej.day$mu.g[lakej.day$depthClass=='Hypo'],lakej.night$mu.g[lakej.night$depthClass=='PML'])) #calculate fitness for normal DVM
reverse=mean(c(lakej.day$mu.g[lakej.day$depthClass=='PML'],lakej.night$mu.g[lakej.night$depthClass=='Hypo'])) #calculate fitness for reverse DVM
noDVM=mean(c(lakej.day$mu.g[lakej.day$depthClass=='PML'],lakej.day$mu.g[lakej.day$depthClass=='PML'])) #calculate fitness for noDVM
bottom=mean(c(lakej.day$mu.g[lakej.day$depthClass=='Hypo'],lakej.night$mu.g[lakej.day$depthClass=='Hypo'])) #calculate fitness for daphnia sitting on the bottom
x=c(normal,reverse,noDVM,bottom) #make into vector
if(min(c(normal,reverse,noDVM,bottom))==x[1]){
	optimal='normal'
} #find minimum --> optimal strategy
if(min(c(normal,reverse,noDVM,bottom))==x[2]){
	optimal='reverse'
}
if(min(c(normal,reverse,noDVM,bottom))==x[3]){
	optimal='noDVM'
}
if(min(c(normal,reverse,noDVM,bottom))==x[4]){
	optimal='bottom'
}
y=data.frame(lakeID=lakes[i],date=day.dates[length(day.dates)],normal=normal,reverse=reverse,noDVM=noDVM,bottom=bottom,optimal=optimal)
fitness=rbind(fitness,y) #add to fitness

