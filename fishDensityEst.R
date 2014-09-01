#this script calculates fish abundance data from minnow traps using mark/recapture -- purpose is to get some sort of conversion factor from CPUE to density
#Patrick Kelly
#29 August 2014

#load fish data
setwd('~/Zoop-Fitness-Model/data/fish 2014/csv')

minnow<-read.csv('Monitoring.Minnow.2014.csv') 

#use just HB data on and beyond 6/11/14
#first add julian date
minnow$jDate<-strptime(minnow$datesample,'%Y-%m-%d')$yday+1

#clean up data, use only HB data after day 162
minnowHB<-minnow[minnow$Lake_ID=='HB' & minnow$jDate>=162,]

#make a vector of the usable dates
dates<-unique(minnowHB$jDate)

M<-nrow(minnowHB[minnowHB$jDate==dates[1] & minnowHB$Clip_apply==1,])
N<-c()
for(i in 2:length(dates)){
	C<-nrow(minnowHB[minnowHB$jDate==dates[i],])
	m<-M[i-1]
	R<-nrow(minnowHB[minnowHB$jDate==dates[i] & !is.na(minnowHB$Clip_recap==1),])
	n<-(C*m)/R
	N<-c(N,n)
	newM<-nrow(minnowHB[minnowHB$jDate==dates[i] & minnowHB$Clip_apply==1,])
	M<-c(M,M+newM)
}#N is in total abundance - need to convert that to fish/m3
N<-N/27000 #=the volume of HB in m3

#calculate average CPUE for HB
CPUE<-c()
for(i in 1:length(dates)){
	catch<-nrow(minnowHB[minnowHB$jDate==dates[i],])
	effort<-mean(minnowHB$Effort[minnowHB$jDate==dates[i]])
	CPUE[i]<-catch/effort
}

#plot 0,0 and CPUE,N to figure out y=CPUEx conversion to get density
x<-c(0,mean(CPUE))
y<-c(0,N[1])
plot(x,y)
summary(lm(y~x)) #density=0.02859*CPUE