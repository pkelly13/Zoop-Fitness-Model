#Zooplankton DVM fitness model - objective is to determine the optimal strategy for a given lake
#Need to include cost of swimming, and change predation rates in fitness cost of predation, i.e. probability of predation and opporunity costs involved in probability of predation imoacting future reproduction - oxygen survival ratio = model from Koh et al 1997 in Ecological modeling
#PTK 14 April 2014


#Calculate u/g for each situation for each lake
#+develop a function that will calculate u/g given a set of parameters - develop u from Jensen et al 2006 using reaction distances, swimming speeds, and predator density - use the submodel for growth rates - alter growth rates by costs (O2, DO, etc.)

library(deSolve)
#source zooplankton growth submodel - can use this function to determine growth rates in different environments
setwd('~/Zoop-Fitness-Model')
source('zoopGrowthSubmodel.R')

#open data frame of model parameters
setwd('~/Documents/Notre Dame/UNDERC 2013/zoop growth model/R files')
modelParms<-read.csv('modelParameters_forFitnessModel.csv')

#convert PAR to lx = 1ueinstein m-2 s-1 = 54 lx
modelParms$lx<-modelParms$PAR*54

#calculate reaction distances for fish - from 
modelParms$reactionDist<-4.3*log(modelParms$lx,10)*(0.1+log(modelParms$lx,10))^-1
#change NaNs to 0
modelParms$reactionDist[is.na(modelParms$reactionDist)]=0

#calculate encounter rates (foraging rate potential) from reaction distance and swimming speed (Daphna ~ )

#run growth model on each row of model parameters
growth<-c()
for(i in 1:nrow(modelParms)){
if(!is.na(modelParms$mgC_L[i]) & !is.na(modelParms$PC[i]) & !is.na(modelParms$EPA_mgC[i]) & !is.na(modelParms$DHA_mgC[i])){	
parms<-c(FQphyt=0.5,PHYT=modelParms$mgC_L[i],FQdet=0.1,DETc=0.1,aC1=0.9,aC2=0.03,lambda=0.6,wPhyt=1,wDet=0,mu=0.035,PCphyt=modelParms$PC[i],fEPA=modelParms$EPA_mgC[i],fDHA=modelParms$DHA_mgC[i],sPC=0.00001,sEPA=0.00001,sDHA=0.00001,tP=0.1,tEPA=0.1,tDHA=0.1,tM=0.05,m=0.05,v=0.5,e=0.05,Pmin=0.009,Popt=0.05,EPAmin=0.0007,EPAopt=0.0082,DHAmin=0.0001,DHAopt=0.0014,X=0.25,hEPA=0.8,hDHA=0.8,r=0.9,p=0.5,thresh=0.05,Jpufa=0.0118)

times=seq(1,500,by=1)

n=c(Pint=0.5,EPAint=0.5,DHAint=0.5)

test<-ode(y=n,times=times,func=timestep,parms=parms)

Pmin=0.009
Popt=0.05
EPAmin=0.0007
EPAopt=0.0082
DHAmin=0.0001
DHAopt=0.0014
r=0.9

#calculate growth rate
glimP<-(test[41,2]-Pmin)/(Popt-Pmin)
glimEPA<-(test[41,3]-EPAmin)/(EPAopt-EPAmin)
glimDHA<-(test[41,4]-DHAmin)/(DHAopt-DHAmin)

r=0.9
growthi<-r*min(c(glimP,glimEPA,glimDHA))
growthi<-growthi*exp(-0.015*abs(20-modelParms$temp[i]))
if(modelParms$DOmgL[i]>=1.1){
	s=1
}
if(modelParms$DOmgL[i]<1.1 & modelParms$DOmgL[i]>=0.2){
	s=sqrt(1.111*(modelParms$DOmgL-0.2))
}
if(modelParms$DOmgL[i]<0.2){
	s=0
}
growth[i]=growthi*s
if(is.na(modelParms$mgC_L[i]) | is.na(modelParms$PC[i]) | is.na(modelParms$EPA_mgC[i]) | is.na(modelParms$DHA_mgC[i])){
	growth[i]=NA
	}
}
}

modelParms$growth=growth

#write data to Zoop-Fitness-Model folder
setwd('~/Zoop-Fitness-Model')
write.csv(modelParms,'fitnessModel_parameters.csv')
